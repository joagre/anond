-module(node_tcp_echo_serv).

%%% external exports
-export([start_link/2]).
-export([session_handler/1]).

%%% internal exports
-export([init/3]).

%%% include files
-include_lib("util/include/config.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants
-define(LISTEN_RECBUF, 8192).
-define(LISTEN_BACKLOG, 128).
-define(MAX_SESSIONS, 1024).
-define(RELAY_CHUNK_SIZE, 4096).

%%% records
-record(state, {
	  parent       :: pid(),
          enabled      :: boolean(),
          address      :: inet:ip_address(),
          port         :: inet:ip_port(),
          tcp_serv_pid :: pid()
         }).

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link(inet:ip(), inet:ip_port()) ->
                        {'ok', pid()} |
                        {'error',
                         {'not_started', tcp_serv:error_reason()}}.

start_link(Address, Port) ->
    Args = [self(), Address, Port],
    Pid = proc_lib:spawn_link(?MODULE, init, Args),
    receive
	{Pid, started} ->
	    {ok, Pid};
	{Pid, {not_started, Reason}} ->
            ?daemon_log("Failed to start echo server: ~s",
                        [tcp_serv:format_error(Reason)]),
            {error, {not_started, Reason}};
        {Pid, Reason} ->
            {error, Reason}
    end.

%%%
%%% server loop
%%%

init(Parent, Address, Port) ->
    process_flag(trap_exit, true),
    case setup(Parent, Address, Port) of
        {ok, S} ->
            Parent ! {self(), started},
            loop(S);
        {error, Reason} ->
            Parent ! {self(), {not_started, Reason}}
    end.

setup(Parent, Address, Port) ->
    case start_echo_server(Address, Port) of
        {ok, TcpServPid} ->
            {ok, #state{parent = Parent,
                        address = Address,
                        port = Port,
                        tcp_serv_pid = TcpServPid}};
        {error, {not_started, Reason}} ->
            {error, Reason}
    end.

start_echo_server(Address, Port) ->
    SocketOptions = [{active, false}, binary, {ip, Address},
                     {recbuf, ?LISTEN_RECBUF}, {reuseaddr, true},
                     {backlog, ?LISTEN_BACKLOG}],
    SessionHandler = {?MODULE, session_handler, []},
    tcp_serv:start_link(Port, ?MAX_SESSIONS, [], SocketOptions, SessionHandler).

%%%
%%% server loop
%%%

loop(#state{parent = Parent,
            enabled = _Enabled,
            address = _Address,
            port = _Port,
            tcp_serv_pid = _TcpServPid} = S) ->
    receive
        {'EXIT', Parent, Reason} ->
            exit(Reason);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

%%%
%%% echo server session handler
%%%

-spec session_handler(inet:socket()) -> tcp_server:session_handler_result().

session_handler(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            case gen_tcp:send(Socket, Data) of
                ok ->
                    session_handler(Socket);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
