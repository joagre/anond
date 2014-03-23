-module(dummy_tcp_serv).

%%% external exports
-export([start_link/2]).
-export([session_handler/3]).

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
            ?daemon_log("Failed to start dummy tcp server: ~s",
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
    TransportOptions = [{active, false}, binary, {ip, Address},
                        {recbuf, ?LISTEN_RECBUF}, {reuseaddr, true},
                        {backlog, ?LISTEN_BACKLOG}],
    case tcp_serv:start_link(Port, [], gen_tcp, TransportOptions,
                             {?MODULE, session_handler, [Address, Port]}) of
        {ok, TcpServPid} ->
            S = #state{parent = Parent,
                       address = Address,
                       port = Port,
                       tcp_serv_pid = TcpServPid},
            Parent ! {self(), started},
            loop(S);
        {error, {not_started, Reason}} ->
            Parent ! {self(), {not_started, Reason}}
    end.

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
%%% server session handler
%%%

-spec session_handler(inet:socket(), inet:ip_address(), inet:ip_port()) ->
                             tcp_serv:session_handler_result().

session_handler(Socket, Address, Port) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            case gen_tcp:send(Socket, Data) of
                ok ->
                    ?dbg_log({got_data, Address, Port, Data}),
                    session_handler(Socket, Address, Port);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
