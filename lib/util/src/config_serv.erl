-module(config_serv).

%%% external exports
-export([start_link/7]).
-export([lookup/1, lookup/2]).
-export([subscribe/0]).
-export([tcp_send/3]).
-export([format_error/1]).

%%% internal exports
-export([init/8]).

%%% include files
-include_lib("util/include/log.hrl").

%%% constants
-define(DEFAULT_CONTROL_ADDRESS, {127, 0, 0, 1}).
-define(DEFAULT_CONTROL_PORT, 23311).
-define(MAX_SESSIONS, 8).

%%% records
-record(state, {
	  parent            :: pid(),
          schema_filename   :: file:filename(),
          config_filename   :: file:filename(),
          convert_callback  :: tree_store:convert_callback(),
	  prepend_path = [] :: tree_store:path(),
	  config_tree       :: tree_store:tree(),
          subscribers = []  :: [pid()],
          tcp_serv_pid      :: pid()
        }).

%%% types
-type error_reason() :: {'tree_store', tree_store:error_reason()} |
                        {'tcp_serv', tcp_serv:error_reason()} |
                        {'posix', inet:posix()}.

%%%
%%% exported: start_link
%%%

-spec start_link(SchemaFilename :: file:filename(),
                 ConfigFilename :: file:filename(),
                 ConvertCallback :: tree_store:convert_callback(),
		 PrependPath :: tree_store:path(),
                 ControlAddressPath :: tree_store:path(),
                 ControlPortPath:: tree_store:path(),
                 tcp_serv:session_handler()) ->
                        {'ok', pid()} |
                            {'error',
                             {'not_started', error_reason()} |
                             'already_started'}.

start_link(SchemaFilename, ConfigFilename, ConvertCallback, PrependPath,
           ControlAddressPath, ControlPortPath, SessionHandler) ->    
    Args = [self(), SchemaFilename, ConfigFilename, ConvertCallback,
            PrependPath, ControlAddressPath, ControlPortPath, SessionHandler],
    Pid = proc_lib:spawn_link(?MODULE, init, Args),
    receive
	{Pid, started} ->
	    {ok, Pid};
	{Pid, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: lookup
%%%

-spec lookup(tree_store:path(),
             DefaultMultiValue :: tree_store:multi_value()) -> 
                    tree_store:multi_value().

lookup(Path) ->
    serv:call(?MODULE, {lookup, Path}).

lookup(Path, DefaultMultiValue) ->
    serv:call(?MODULE,  {lookup, Path, DefaultMultiValue}).

%%%
%%% exported: subscribe
%%%

-spec subscribe() -> 'ok'.

subscribe() ->
    ?MODULE ! {subscribe, self()},
    ok.

%%%
%%% exported: tcp_send
%%%

-spec tcp_send(inet:ip_address(), inet:ip_port(), Message :: binary()) ->
                      'ok' | {'error', {'posix', inet:posix()}}.

tcp_send(Address, Port, Message) ->
    case gen_tcp:connect(Address, Port,
                         [{packet,2}, {nodelay, true}, binary]) of
        {ok, Socket} ->
            gen_tcp:send(Socket, Message),
            gen_tcp:close(Socket);
        {error, Reason} ->
            {error, {posix, Reason}}
    end.

%%%
%%% exported: format_error
%%%

-spec format_error(error_reason()) -> iolist().

format_error({tree_store, Reason}) ->
    tree_store:format_error(Reason);
format_error({tcp_serv, Reason}) ->
    tcp_serv:format_error(Reason);
format_error({posix, Reason}) ->
    inet:format_error(Reason);
format_error(UnknownReason) ->
    ?error_log(UnknownReason),
    "internal error".

%%%
%%% server loop
%%%

init(Parent, SchemaFilename, ConfigFilename, ConvertCallback, PrependPath,
     ControlAddressPath, ControlPortPath, SessionHandler) ->    
    process_flag(trap_exit, true),
    case catch register(?MODULE, self()) of
        true ->
            case setup(Parent, SchemaFilename, ConfigFilename, ConvertCallback,
                       PrependPath, ControlAddressPath, ControlPortPath,
                       SessionHandler) of
                {ok, S} ->
                    Parent ! {self(), started},
                    loop(S);
                {error, Reason} ->
                    Parent ! {self(), {not_started, Reason}}
            end;
        _ ->
            Parent ! {self(), already_started}
    end.

setup(Parent, SchemaFilename, ConfigFilename, ConvertCallback, PrependPath,
      ControlAddressPath, ControlPortPath, SessionHandler) ->
    case tree_store:load_xml(SchemaFilename, ConfigFilename, ConvertCallback) of
	{ok, ConfigTree} ->
            [ControlAddress] =
                tree_store:lookup(ConfigTree, PrependPath++ControlAddressPath),
            [ControlPort] =
                tree_store:lookup(ConfigTree, PrependPath++ControlPortPath),
            SocketOptions = [{packet, 2}, {ip, ControlAddress}, binary],
            case tcp_serv:start_link(ControlPort, ?MAX_SESSIONS, [],
                                     SocketOptions, SessionHandler) of
                {ok, Pid} ->
                    {ok, #state{parent = Parent,
                                schema_filename = SchemaFilename,
                                config_filename = ConfigFilename,
                                convert_callback = ConvertCallback,
                                prepend_path = PrependPath,
                                config_tree = ConfigTree,
                                tcp_serv_pid = Pid}};
                {error, {not_started, Reason}} ->
                    {error, {tcp_serv, Reason}}
            end;
        {error, Reason} ->
            {error, {tree_store, Reason}}
    end.

loop(#state{parent = Parent,
            schema_filename = SchemaFilename,
            config_filename = ConfigFilename,
            convert_callback = ConvertCallback,
            prepend_path = PrependPath,
            config_tree = ConfigTree,
            subscribers = Subscribers} = S) ->
    receive
        {From, {lookup, Path}} ->
            AbsolutePath = PrependPath++Path,
            From ! {self(), tree_store:lookup(ConfigTree, AbsolutePath)},
            loop(S);
        {From, {lookup, Path, DefaultMultiValue}} ->
            AbsolutePath = PrependPath++Path,
            case tree_store:lookup(ConfigTree, AbsolutePath) of
                [] ->
                    From ! {self(), DefaultMultiValue},
                    loop(S);
                MultiValue ->
                    From ! {self(), MultiValue},
                    loop(S)
            end;
        {subscribe, ClientPid} ->
            case lists:member(ClientPid, Subscribers) of
                true ->
                    loop(S);
                false ->
                    erlang:monitor(process, ClientPid),
                    loop(S#state{subscribers = [ClientPid|Subscribers]})
            end;
        {'DOWN', _MonitorRef, process, ClientPid, _Info} ->
            UpdatedSubscribers = lists:delete(ClientPid, Subscribers),
            loop(S#state{subscribers = UpdatedSubscribers});        
        reload ->
            case tree_store:load_xml(SchemaFilename, ConfigFilename,
                                     ConvertCallback) of
                {ok, NewConfigTree} ->
                    ?daemon_log("~s: load succeeded", [ConfigFilename]),
                    lists:foreach(fun(ClientPid) ->
                                          ClientPid ! config_updated
                                  end, Subscribers),
                    loop(S#state{config_tree = NewConfigTree});
                {error, Reason} ->
                    ?daemon_log("~s: load failed: ~s",
                                [ConfigFilename,
                                 tree_store:format_error(Reason)]),
                    loop(S)
            end;
	{'EXIT', Parent, shutdown} ->
	    exit(shutdown);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.
