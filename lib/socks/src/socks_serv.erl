-module(socks_serv).

%%% external exports
-export([start_link/0]).
-export([session_handler/1]).

%%% internal exports
-export([init/1]).
-export([tcp_relay/3]).

%%% include files
-include_lib("util/include/config.hrl").
-include_lib("util/include/log.hrl").
-include_lib("socks.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants
-define(LISTEN_RECBUF, 8192).
-define(LISTEN_BACKLOG, 128).
-define(MAX_SESSIONS, 1024).

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

-spec start_link() -> {'ok', pid()} |
                          {'error',
                           {'not_started', tcp_serv:error_reason()} |
                           'already_started'}.

start_link() ->
    Args = [self()],
    Pid = proc_lib:spawn_link(?MODULE, init, Args),
    receive
	{Pid, started} ->
	    {ok, Pid};
	{Pid, {not_started, Reason}} ->
            ?daemon_log("Failed to start SOCKS server: ~s",
                        [tcp_serv:format_error(Reason)]),
            {error, {not_started, Reason}};
        {Pid, Reason} ->
            {error, Reason}
    end.

%%%
%%% server loop
%%%

init(Parent) ->
    process_flag(trap_exit, true),
    case catch register(?MODULE, self()) of
        true ->
            case setup(Parent) of
                {ok, S} ->
                    Parent ! {self(), started},
                    loop(S);
                {error, Reason} ->
                    Parent ! {self(), {not_started, Reason}}
            end;
        _ ->
            Parent ! {self(), already_started}
    end.

setup(Parent) ->
    [Enabled] = ?cfg([socks, enabled]),
    case Enabled of
        false ->
            {ok, #state{parent = Parent, enabled = false}};
        true ->
            [Address] = ?cfg([socks, listen, address]),
            [Port] = ?cfg([socks, listen, port]),
            case start_server(Address, Port) of
                {ok, TcpServPid} ->
                    ok = config_serv:subscribe(),
                    {ok, #state{parent = Parent,
                                enabled = true,
                                address = Address,
                                port = Port,
                                tcp_serv_pid = TcpServPid}};
                {error, {not_started, Reason}} ->
                    {error, Reason};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

start_server(Address, Port) ->
    SocketOptions = [{active, false}, binary, {packet, 0}, {ip, Address},
                     {recbuf, ?LISTEN_RECBUF}, {reuseaddr, true},
                     {backlog, ?LISTEN_BACKLOG}],
    SessionHandler = {?MODULE, session_handler, []},
    tcp_serv:start_link(Port, ?MAX_SESSIONS, [], SocketOptions, SessionHandler).

loop(#state{parent = Parent,
            enabled = Enabled,
            address = Address,
            port = Port,
            tcp_serv_pid = TcpServPid} = S) ->
    receive
	config_updated ->
            ?dbg_log(config_updated),
            case ?cfg([socks, enabled]) of
                [false] when Enabled == true ->
                    tcp_serv:stop(TcpServPid),
                    ?daemon_log("The SOCKS server has been stopped.", []),
                    loop(S#state{enabled = false});
                [false] ->
                    loop(S);
                [true] when Enabled == true ->
                    [NewAddress] = ?cfg([socks, listen, address]),
                    [NewPort] = ?cfg([socks, listen, port]),
                    case {NewAddress, NewPort} of
                        {Address, Port} ->
                            loop(S);
                        _ ->
                            ok = tcp_serv:stop(TcpServPid),
                            case start_server(NewAddress, NewPort) of
                                {ok, NewTcpServPid} ->
                                    ?daemon_log(
                                       "The SOCKS server has been restarted.",
                                       []),
                                    loop(S#state{address = NewAddress,
                                                 port = NewPort,
                                                 tcp_serv_pid = NewTcpServPid});
                                {error, {not_started, Reason}} ->
                                    ?daemon_log(
                                       "The SOCKS server could not be "
                                       "restarted: ~s",
                                       [tcp_serv:format_error(Reason)]),
                                    loop(S);
                                {error, Reason} ->
                                    ?error_log(Reason),
                                    {error, Reason}
                            end
                    end;
                [true] ->
                    [NewAddress] = ?cfg([socks, listen, address]),
                    [NewPort] = ?cfg([socks, listen, port]),
                    case start_server(NewAddress, NewPort) of
                        {ok, NewTcpServPid} ->
                            ?daemon_log(
                               "The SOCKS server has been started.", []),
                            loop(S#state{address = NewAddress,
                                         port = NewPort,
                                         tcp_serv_pid = NewTcpServPid});
                        {error, Reason} ->
                            ?daemon_log(
                               "The SOCKS server could not be restarted: ~s",
                               [tcp_serv:format_error(Reason)]),
                            loop(S)
                    end
            end;
        {'EXIT', Parent, Reason} ->
            exit(Reason);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

%%%
%%% SOCKS session handler
%%%

-spec session_handler(inet:socket()) -> tcp_server:session_handler_result().

session_handler(ClientSocket) ->
    try
        ?dbg_log(handshake),
        handshake(ClientSocket),
        ?dbg_log(parse_request),
        Command = parse_request(ClientSocket),
        ?dbg_log(handle_command),
        handle_command(ClientSocket, Command)
    catch
        Reason ->
            ?error_log(Reason),
            {error, Reason}
    end.

handshake(ClientSocket) ->
    {ok, <<?SOCKS_VER, Nmethods>>} = gen_tcp:recv(ClientSocket, 2),
    {ok, Methods} = gen_tcp:recv(ClientSocket, Nmethods),
    authenticate(ClientSocket, ?b2l(Methods)).

authenticate(ClientSocket, Methods) ->
    case lists:member(?SOCKS_NO_AUTHENTICATION_REQUIRED, Methods) of
        true ->
            ok = gen_tcp:send(ClientSocket,
                              <<?SOCKS_VER,
                                ?SOCKS_NO_AUTHENTICATION_REQUIRED>>);        
        false ->
            ok = gen_tcp:send(ClientSocket,
                              <<?SOCKS_VER,
                                ?SOCKS_NO_ACCEPTABLE_METHODS>>),
            throw({suicide, authenticate})
    end.

parse_request(ClientSocket) ->
    {ok, <<?SOCKS_VER, Cmd, 0, Atyp>>} = gen_tcp:recv(ClientSocket, 4),
    case Cmd of
        ?SOCKS_CONNECT ->
            {connect, connect(ClientSocket, Atyp)};
%        ?SOCKS_BIND ->
%            {bind, bind(ClientSocket, Atyp)};
%        ?SOCKS_UDP_ASSOCIATE ->
%            {udp_associate, udp_associate(ClientSocket, Atyp)};
        _ ->
            ok = gen_tcp:send(ClientSocket,
                              <<?SOCKS_VER,
                                ?SOCKS_COMMAND_NOT_SUPPORTED>>),
            throw({suicide, parse_request})
    end.

connect(ClientSocket, Atyp) ->
    Address = get_connect_address(ClientSocket, Atyp),
    {ok, <<Port:16/integer>>} = gen_tcp:recv(ClientSocket, 2),
    Options = [{active, false}, {packet, raw}, binary],
    
    ?dbg_log({Address, Port, Options}),
    


    case gen_tcp:connect(Address, Port, Options) of
	{ok, Socket} ->
            {ok, {LocalAddress, LocalPort}} = inet:sockname(Socket),
            DestAtyp = extract_atyp(LocalAddress),
            ok = gen_tcp:send(ClientSocket,
                              <<?SOCKS_VER, ?SOCKS_SUCCEEDED, 0, DestAtyp>>),
            ok = gen_tcp:send(ClientSocket, mk_binary_address(LocalAddress)),
            ok = gen_tcp:send(ClientSocket, <<LocalPort:16>>),
	    Socket;
        {error, Reason} ->
            ?error_log(Reason),
            Rep = reply_field(Reason),
            ok = gen_tcp:send(ClientSocket, <<?SOCKS_VER, Rep>>),
            throw({suicide, connect})
    end.

reply_field(ehostunreach) -> ?SOCKS_HOST_UNREACHABLE;
reply_field(enetunreach) -> ?SOCKS_NETWORK_UNREACHABLE;
reply_field(econnrefused) -> ?SOCKS_CONNECTION_REFUSED;
reply_field(_) -> ?SOCKS_GENERAL_SOCKS_SERVER_FAILURE.

get_connect_address(Socket, ?SOCKS_IP_V4) ->
    {ok, <<Ip1, Ip2, Ip3, Ip4>>} = gen_tcp:recv(Socket, 4),
    {Ip1, Ip2, Ip3, Ip4};
get_connect_address(Socket, ?SOCKS_DOMAINNAME) ->
    {ok, <<Length:16/integer>>} = gen_tcp:recv(Socket, 1),
    {ok, DomainName} = gen_tcp:recv(Socket, Length),
    ?b2l(DomainName);
get_connect_address(Socket, ?SOCKS_IP_V6) ->
    {ok, <<Ip1:16, Ip2:16, Ip3:16, Ip4:16, Ip5:16, Ip6:16, Ip7:16, Ip8:16>>} =
        gen_tcp:recv(Socket, 16),
    {Ip1, Ip2, Ip3, Ip4, Ip5, Ip6, Ip7, Ip8}.

extract_atyp(Address) when is_tuple(Address), size(Address) == 4 ->
    ?SOCKS_IP_V4;
extract_atyp(Address) when is_tuple(Address), size(Address) == 8 ->
    ?SOCKS_IP_V6.

mk_binary_address({Ip1, Ip2, Ip3, Ip4}) ->
    <<Ip1, Ip2, Ip3, Ip4>>;
mk_binary_address({Ip1, Ip2, Ip3, Ip4, Ip5, Ip6, Ip7, Ip8}) ->
    <<Ip1:16, Ip2:16, Ip3:16, Ip4:16, Ip5:16, Ip6:16, Ip7:16, Ip8:16>>.

handle_command(ClientSocket, {connect, ServerSocket}) ->
    ?dbg_log(tcp_relay_incoming),
    IncomingPid =
	spawn(?MODULE, tcp_relay, [self(), ClientSocket, ServerSocket]),
    ?dbg_log(tcp_relay_outgoing),
    OutgoingPid =
	spawn(?MODULE, tcp_relay, [self(), ServerSocket, ClientSocket]),
    wait_for(IncomingPid, OutgoingPid).

wait_for(done, done) ->
    ok;
wait_for(IncomingPid, OutgoingPid) ->
    receive
        {done, IncomingPid} ->
	    wait_for(done, OutgoingPid);
	{done, OutgoingPid} ->
	    wait_for(IncomingPid, done);
        UnknownMessage ->
            {error, {unknown_message, UnknownMessage}}
    end.

tcp_relay(ParentPid, Peer1Socket, Peer2Socket) ->
    case gen_tcp:recv(Peer1Socket, 0) of
	{ok, Packet} ->
	    ?dbg_log({size, size(Packet)}),
	    case gen_tcp:send(Peer2Socket, Packet) of
		ok ->
		    tcp_relay(ParentPid, Peer1Socket, Peer2Socket);
		{error, closed} ->
		    ?dbg_log({closed}),
		    ParentPid ! {done, self()};
		{error, Reason} ->
		    ?dbg_log({error, Reason}),
		    ?error_log({tcp_relay, Reason}),
		    ParentPid ! {done, self()}
	    end;
	{error, closed} ->
	    ?dbg_log({error, closed}),
	    ParentPid ! {done, self()};
	{error, Reason} ->
	    ?error_log({tcp_relay, Reason}),
	    ParentPid ! {done, self()}
    end.
