-module(tcp_serv).

%%% external exports
-export([start_link/5, stop/1, stop/2]).
-export([format_error/1]).

%%% internal exports
-export([start_session/3]).
-export([init/6]).

%%% include files
-include_lib("util/include/log.hrl").

%%% constants
-define(WAIT, 5000).

%%% types
-type handler() :: {M :: atom(), F :: atom(), A :: list()}.
-type options() :: [{'name', atom()}].
-type socket_options() :: [gen_tcp:listen_option()].
-type error_reason() :: inet:posix().

%%% records
-record(state, {
          max_sessions    :: integer(),
          handler         :: handler(),
          session_list    :: [pid()],
          listen_socket   :: gen_tcp:socket(),
          parent          :: pid()
         }).

%%%
%%% exported: start_link
%%%

-spec start_link(inet:port_number(), MaxSessions :: integer(), options(),
                 socket_options(), handler()) ->
                        {'ok', pid()} |
                        {'error',
                         {'not_started', error_reason()} |
                         'already_started'}.

start_link(Port, MaxSessions, Options, SocketOptions, Handler) ->
    Args = [self(), Port, MaxSessions, Options, SocketOptions, Handler],
    Pid = proc_lib:spawn_link(?MODULE, init, Args),
    receive
	{Pid, started} ->
	    {ok, Pid};
	{Pid, Reason} ->
            {error, Reason}
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

init(Parent, Port, MaxSessions, Options, SocketOptions, Handler) ->
    process_flag(trap_exit, true),
    case lists:keysearch(name, 1, Options) of
        {value, {name, Name}} ->
            case catch register(Name, self()) of
                true ->
                    setup(Parent, Port, MaxSessions, SocketOptions, Handler);
                _ ->
                    Parent ! {self(), already_started}
            end;
        false ->
            setup(Parent, Port, MaxSessions, SocketOptions, Handler)
    end.

setup(Parent, Port, MaxSessions, SocketOptions, Handler) ->
    case gen_tcp:listen(Port, SocketOptions) of
        {ok, ListenSocket} ->
            Parent ! {self(), started},
            self() ! start_session,
            loop(#state{max_sessions = MaxSessions,
                        handler = Handler,
                        session_list = [],
                        listen_socket = ListenSocket,
                        parent = Parent});
        {error, Reason} ->
            Parent ! {self(), {not_started, Reason}}
    end.

loop(#state{session_list = SessionList, listen_socket = ListenSocket,
	    parent = Parent} = S) ->
    receive
	{From, stop} ->
            lists:foreach(fun(Pid) -> exit(Pid, shutdown) end, SessionList),
            gen_tcp:close(S#state.listen_socket),
	    From ! {self(), ok};
        start_session when length(SessionList) > S#state.max_sessions ->
            timer:sleep(?WAIT),
	    self() ! start_session,
	    loop(S);
	start_session ->
	    Args = [self(), S#state.handler, ListenSocket],
	    Pid = spawn_link(?MODULE, start_session, Args),
	    loop(S#state{session_list = [Pid|SessionList]});
        {'EXIT', Parent, shutdown} ->
            gen_tcp:close(S#state.listen_socket),
            exit(shutdown);
	{'EXIT', Pid, _Reason} ->
	    case lists:member(Pid, SessionList) of
		true ->
		    PurgedSessionList = lists:delete(Pid, SessionList),
		    loop(S#state{session_list = PurgedSessionList});
		false ->
		    loop(S)
	    end;
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

start_session(Parent, {M, F, A}, ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
	{ok, Socket} ->
	    Parent ! start_session,
	    case catch apply(M, F, [Socket|A]) of
		ok ->
                    gen_tcp:close(Socket);
		{error, closed} ->
                    ok;
		{error, Reason} ->
		    ?error_log(Reason),
		    gen_tcp:close(Socket);
                {'EXIT', Reason} ->
		    ?error_log(Reason),
		    gen_tcp:close(Socket)
	    end;
	_Error ->
	    timer:sleep(?WAIT),
	    Parent ! start_session
    end.
