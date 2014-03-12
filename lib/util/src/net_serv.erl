-module(net_serv).

%%% external exports
-export([start_link/4, stop/1, stop/2]).
-export([format_error/1]).

%%% internal exports
-export([start_session/5]).
-export([init/5]).

%%% include files
-include_lib("util/include/log.hrl").

%%% constants
-define(FIVE_SECONDS, 5000).
-define(VALID_OPTIONS, [max_sessions, max_sessions_per_client, name]).
-define(DEFAULT_MAX_SESSIONS, 16384).
-define(DEFAULT_MAX_SESSIONS_PER_CLIENT, 64).

%%% types
-type handler() :: {Module :: atom(), Function :: atom(), Args :: list()}.
-type options() :: [option()].
-type option() :: {'max_sessions', integer()} |
                  {'max_sessions_per_client', integer()} |
                  {'name', atom()}.
-type socket_options() :: [gen_tcp:listen_option()].
-type error_reason() :: inet:posix().

%%% records
-record(state, {
          parent                  :: pid(),
          listen_socket           :: gen_tcp:socket(),
          session_db              :: ets:tid(),
          handler                 :: handler(),
          max_sessions            :: integer(),
          max_sessions_per_client :: integer()
         }).

%%%
%%% exported: start_link
%%%

-spec start_link(inet:port_number(), options(), socket_options(), handler()) ->
                        {'ok', pid()} |
                        {'error',
                         {'not_started', error_reason()} |
                         'already_started' |
                         {'invalid_option', any()}}.

start_link(Port, Options, SocketOptions, Handler) ->
    case valid_options(?VALID_OPTIONS, Options) of
        yes ->
            Args = [self(), Port, Options, SocketOptions, Handler],
            Pid = proc_lib:spawn_link(?MODULE, init, Args),
            receive
                {Pid, started} ->
                    {ok, Pid};
                {Pid, Reason} ->
                    {error, Reason}
            end;
        {no, InvalidOption} ->
            {error, {invalid_option, InvalidOption}}
    end.

valid_options(_ValidOptions, []) ->
    yes;
valid_options(ValidOptions, [Option|Rest]) ->
    case lists:member(Option, ValidOptions) of
        true ->
            valid_options(ValidOptions, Rest);
        false ->
            {no, Option}
    end.

%%%
%%% exported: stop
%%%

-spec stop(pid() | atom(), timeout()) -> 'ok'.

stop(PidOrName) ->
    stop(PidOrName, 15000).

stop(PidOrName, Timeout) ->
    serv:call(PidOrName, stop, Timeout).

%%%
%%% exported: format_error
%%%

-spec format_error(error_reason()) -> iolist().

format_error(Reason) ->
    inet:format_error(Reason).

%%%
%%% server loop
%%%

init(Parent, Port, Options, SocketOptions, Handler) ->
    process_flag(trap_exit, true),
    case lookup_option(name, Options, '$not_set') of
        '$not_set' ->
            setup(Parent, Port, Options, SocketOptions, Handler);
        Name ->
            case catch register(Name, self()) of
                true ->
                    setup(Parent, Port, Options, SocketOptions, Handler);
                _ ->
                    Parent ! {self(), already_started}
            end
    end.

lookup_option(_Option, [], DefaultValue) ->
    DefaultValue;
lookup_option(Option, [{Option, Value}|_], _DefaultValue) ->
    Value;
lookup_option(_Option, [_|Rest], DefaultValue) ->
    lookup_option(_Option, Rest, DefaultValue).

setup(Parent, Port, Options, SocketOptions, Handler) ->
    MaxSessions = lookup_option(max_sessions, Options, ?DEFAULT_MAX_SESSIONS),
    MaxSessionsPerClient =
        lookup_option(max_sessions_per_client, Options,
                      ?DEFAULT_MAX_SESSIONS_PER_CLIENT),
    case gen_tcp:listen(Port, SocketOptions) of
        {ok, ListenSocket} ->
            Parent ! {self(), started},
            self() ! {start_session, undefined, undefined},
            SessionDb = ets:new(session_db, [bag]),
            loop(#state{parent = Parent,
                        listen_socket = ListenSocket,
                        session_db = SessionDb,
                        handler = Handler,
                        max_sessions = MaxSessions,
                        max_sessions_per_client = MaxSessionsPerClient});
        {error, Reason} ->
            Parent ! {self(), {not_started, Reason}}
    end.

loop(#state{parent = Parent,
            listen_socket = ListenSocket,
            session_db = SessionDb,
            handler = Handler,
            max_sessions = MaxSessions,
            max_sessions_per_client = MaxSessionsPerClient} = S) ->
    receive
	{From, stop} ->
            cleanup(ListenSocket, SessionDb),
	    From ! {self(), ok};
	{start_session, IpAddress, SessionPid} ->
            log_session(SessionDb, IpAddress, SessionPid),
            NumberOfSessions = ets:info(SessionDb, size),
            if
                MaxSessions /= -1 andalso
                NumberOfSessions > MaxSessions ->
                    timer:sleep(?FIVE_SECONDS),
                    self() ! {start_session, undefined, undefined},
                    loop(S);
                true ->
                    Args = [self(), Handler, ListenSocket, SessionDb,
                            MaxSessionsPerClient],
                    proc_lib:spawn_link(?MODULE, start_session, Args),
                    loop(S)
            end;
        {'EXIT', Parent, shutdown} ->
            cleanup(ListenSocket, SessionDb),
            exit(shutdown);
        {'EXIT', SessionPid, _Reason} ->
            true = ets:match_delete(SessionDb, {'_', SessionPid}),
            loop(S);
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            loop(S)
    end.

cleanup(ListenSocket, SessionDb) ->
    ets:foldl(fun({_, SessionPid}) ->
                      exit(SessionPid, shutdown)
              end, [], SessionDb),
    ets:delete(SessionDb),
    gen_tcp:close(ListenSocket).

start_session(Parent, {Module, Function, Args}, ListenSocket, SessionDb,
              MaxSessionsPerClient) ->
    case gen_tcp:accept(ListenSocket) of
	{ok, Socket} ->
            IpAddress = peername(Socket),
            Parent ! {start_session, IpAddress, self()},
            case verify_session(SessionDb, MaxSessionsPerClient, IpAddress) of
                false ->
                    gen_tcp:close(Socket);
                true ->
                    case catch apply(Module, Function, [Socket|Args]) of
                        ok ->
                            gen_tcp:close(Socket);
                        {error, closed} ->
                            ok;
                        {error, Reason} ->
                            ?daemon_log("Unexpected handler error: ~p",
                                        [Reason]),
                            gen_tcp:close(Socket);
                        {'EXIT', Reason} ->
                            ?error_log(Reason),
                            gen_tcp:close(Socket)
                    end
            end;
	Reason ->
            ?daemon_log("Unexpected accept error: ~p", [Reason]),
	    timer:sleep(?FIVE_SECONDS),
	    Parent ! {start_session, undefined, undefined}
    end.

peername(Socket) ->
    case inet:peername(Socket) of
        {ok, {IpAddress, _}} ->
            IpAddress;
        _ ->
            undefined
    end.

log_session(_SessionDb, _IpAddress, undefined) ->
    true;
log_session(_SessionDb, undefined, _SessionPid) ->
    true;
log_session(SessionDb, IpAddress, SessionPid) ->
    true = ets:insert(SessionDb, {IpAddress, SessionPid}).

verify_session(_SessionDb, undefined, _MaxNumberOfSessionsPerClient) ->
    true;
verify_session(_SessionDb, _IpAddress, -1) ->
    true;
verify_session(SessionDb, IpAddress, MaxNumberOfSessionsPerClient) ->
    length(ets:lookup(SessionDb, IpAddress)) < MaxNumberOfSessionsPerClient.
