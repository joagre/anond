-module(tcp_serv).

%%% external exports
-export([start_link/5, stop/1, stop/2]).
-export([format_error/1]).

%%% internal exports
-export([start_session/6]).
-export([init/6]).

%%% include files
-include_lib("util/include/log.hrl").

%%% constants
-define(FIVE_SECONDS, 5000).
-define(VALID_OPTIONS, [max_sessions, max_sessions_per_client, name]).
-define(DEFAULT_MAX_SESSIONS, 16384).
-define(DEFAULT_MAX_SESSIONS_PER_CLIENT, 64).

%%% types

-type transport_module() :: 'gen_tcp' | 'ssl'.
-type socket() :: gen_tcp:socket() | ssl:sslsocket().
-type handler() :: {Module :: atom(), Function :: atom(), Args :: list()}.
-type options() :: [option()].
-type option() :: {'max_sessions', integer()} |
                  {'max_sessions_per_client', integer()} |
                  {'name', atom()}.
-type transport_options() :: [ssl:option()].
-type error_reason() :: inet:posix().

%%% records
-record(state, {
          parent                  :: pid(),
          transport_module        :: transport_module(),
          listen_socket           :: socket(),
          session_db              :: ets:tid(),
          handler                 :: handler(),
          max_sessions            :: integer(),
          max_sessions_per_client :: integer()
         }).

%%%
%%% exported: start_link
%%%

-spec start_link(inet:port_number(), options(), transport_module(),
                 transport_options(), handler()) ->
                        {'ok', pid()} |
                        {'error',
                         {'not_started', error_reason()} |
                         'already_started' |
                         {'invalid_option', any()}}.

start_link(Port, Options, TransportModule, TransportOptions, Handler) ->
    case valid_options(?VALID_OPTIONS, Options) of
        yes ->
            Args = [self(), Port, Options, TransportModule, TransportOptions,
                    Handler],
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

init(Parent, Port, Options, TransportModule, TransportOptions, Handler) ->
    process_flag(trap_exit, true),
    case lookup_option(name, Options, '$not_set') of
        '$not_set' ->
            setup(Parent, Port, Options, TransportModule, TransportOptions,
                  Handler);
        Name ->
            case catch register(Name, self()) of
                true ->
                    setup(Parent, Port, Options, TransportModule,
                          TransportOptions, Handler);
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

setup(Parent, Port, Options, TransportModule, TransportOptions, Handler) ->
    case listen(Port, TransportModule, TransportOptions) of
        {ok, ListenSocket} ->
            SessionDb = create_session_db(),
            MaxSessions =
                lookup_option(max_sessions, Options, ?DEFAULT_MAX_SESSIONS),
            MaxSessionsPerClient =
                lookup_option(max_sessions_per_client, Options,
                              ?DEFAULT_MAX_SESSIONS_PER_CLIENT),
            self() ! {start_session, undefined, undefined},
            Parent ! {self(), started},
            loop(#state{parent = Parent,
                        transport_module = TransportModule,
                        listen_socket = ListenSocket,
                        session_db = SessionDb,
                        handler = Handler,
                        max_sessions = MaxSessions,
                        max_sessions_per_client = MaxSessionsPerClient});
        {error, Reason} ->
            Parent ! {self(), {not_started, Reason}}
    end.

loop(#state{parent = Parent,
            transport_module = TransportModule,
            listen_socket = ListenSocket,
            session_db = SessionDb,
            handler = Handler,
            max_sessions = MaxSessions,
            max_sessions_per_client = MaxSessionsPerClient} = S) ->
    receive
	{From, stop} ->
            cleanup(TransportModule, ListenSocket, SessionDb),
	    From ! {self(), ok};
	{start_session, IpAddress, SessionPid} ->
            log_session(SessionDb, IpAddress, SessionPid),
            NumberOfSessions = number_of_sessions(SessionDb),
            if
                MaxSessions /= -1 andalso
                NumberOfSessions > MaxSessions ->
                    ?daemon_log("At most ~w sessions are allowed. Waiting "
                                "for five seconds...", [MaxSessions]),
                    timer:sleep(?FIVE_SECONDS),
                    self() ! {start_session, undefined, undefined},
                    loop(S);
                true ->
                    Args = [self(), Handler, TransportModule, ListenSocket,
                            SessionDb, MaxSessionsPerClient],
                    proc_lib:spawn_link(?MODULE, start_session, Args),
                    loop(S)
            end;
        {'EXIT', Parent, shutdown} ->
            cleanup(TransportModule, ListenSocket, SessionDb),
            exit(shutdown);
        {'EXIT', SessionPid, _Reason} ->
            delete_session(SessionDb, SessionPid),
            loop(S);
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            loop(S)
    end.

cleanup(TransportModule, ListenSocket, SessionDb) ->
    for_all_sessions(SessionDb,
                     fun({_IpAddress, SessionPid}) ->
                             exit(SessionPid, shutdown)
                     end),
    delete_session_db(SessionDb),
    close(TransportModule, ListenSocket).

start_session(Parent, Handler, TransportModule, ListenSocket, SessionDb,
              MaxSessionsPerClient) ->
    case accept(TransportModule, ListenSocket) of
	{ok, Socket} ->
            IpAddress = peername(TransportModule, Socket),
            Parent ! {start_session, IpAddress, self()},
            case verify_session(SessionDb, MaxSessionsPerClient, IpAddress) of
                false ->
                    ?daemon_log("~s tried to open more than ~w sessions",
                                [net_tools:string_address(IpAddress),
                                 MaxSessionsPerClient]),
                    close(TransportModule, Socket);
                true ->
                    {Module, Function, Args} = Handler,
                    case catch apply(Module, Function, [Socket|Args]) of
                        ok ->
                            close(TransportModule, Socket);
                        {error, closed} ->
                            ok;
                        {error, Reason} ->
                            ?daemon_log("Unexpected handler error: ~p",
                                        [Reason]),
                            close(TransportModule, Socket);
                        {'EXIT', Reason} ->
                            ?error_log(Reason),
                            close(TransportModule, Socket)
                    end
            end;
	{error, Reason} ->
            ?daemon_log("Unexpected networking problem: ~p", [Reason]),
	    timer:sleep(?FIVE_SECONDS),
	    Parent ! {start_session, undefined, undefined}
    end.

%%%
%%% socket abstractions for TCP and SSL
%%%

listen(Port, TransportModule, TransportOptions) ->
    TransportModule:listen(Port, TransportOptions).

accept(ssl, ListenSocket) ->
    case ssl:transport_accept(ListenSocket) of
        {ok, Socket} ->
            case ssl:ssl_accept(Socket) of
                ok ->
                    {ok, Socket};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
accept(gen_tcp, ListenSocket) ->
    gen_tcp:accept(ListenSocket).

peername(gen_tcp, Socket) ->
    peername(inet, Socket);
peername(TransportModule, Socket) ->
    case TransportModule:peername(Socket) of
        {ok, {IpAddress, _}} ->
            IpAddress;
        _ ->
            undefined
    end.

close(TransportModule, Socket) ->
    TransportModule:close(Socket).

%%%
%%% session database
%%%

create_session_db() ->
    ets:new(session_db, [bag]).

delete_session_db(SessionDb) ->
    ets:delete(SessionDb).

number_of_sessions(SessionDb) ->
    ets:info(SessionDb, size).

delete_session(SessionDb, SessionPid) ->
    true = ets:match_delete(SessionDb, {'_', SessionPid}).

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

for_all_sessions(SessionDb, Fun) ->
    ets:foldl(fun({IpAddress, SessionPid}, Acc) ->
                      Fun({IpAddress, SessionPid}),
                      Acc
              end, [], SessionDb).
