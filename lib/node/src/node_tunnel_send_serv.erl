-module(node_tunnel_send_serv).

%%% external exports
-export([start_link/4, stop/1, stop/2]).
-export([send/2]).

%%% internal exports
-export([init/5]).

%%% include files
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").
-include_lib("node/include/node.hrl").
-include_lib("node/include/node_path_cost.hrl").
-include_lib("node/include/node_route.hrl").
-include_lib("node/include/node_tunnel.hrl").

%%% constants

%%% records
-record(state, {
	  parent  :: pid(),
	  na      :: na(),
          peer_na :: na(),
          socket  :: gen_udp:socket()
	 }).

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link(na(), na(), supervisor:sup_ref(), supervisor:sup_ref()) ->
                        {'ok', pid()} |
                        {'error', inet:posix()}.

start_link(Na, PeerNa, NodeSup, NodeTunnelSup) ->
    {ok, NodeServ} = node_sup:lookup_child(NodeSup, node_serv),
    {ok, NodeTunnelRecvServ} =
        node_tunnel_sup:lookup_child(NodeTunnelSup, node_tunnel_recv_serv),
    Args = [self(), Na, PeerNa, NodeServ, NodeTunnelRecvServ],
    Pid = proc_lib:spawn_link(?MODULE, init, Args),
    receive
	{Pid, started} ->
	    {ok, Pid};
	{Pid, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: send
%%%

-spec send(pid(),
           binary() |
           {'ip_packet', noa(), binary()} |
           #routing_entry{} |
           #echo_request{}) ->
                  'ok'.

send(NodeSendServ, Data) ->
    NodeSendServ ! Data,
    ok.

%%%
%%% exported: stop
%%%

-spec stop(pid()) -> 'ok'.

stop(Pid) ->
    stop(Pid, 15000).

-spec stop(pid(), timeout()) -> 'ok'.

stop(Pid, Timeout) ->
    serv:call(Pid, stop, Timeout).

%%%
%%% server loop
%%%

init(Parent, {IpAddress, Port} = Na, PeerNa, NodeServ, NodeTunnelRecvServ) ->
    process_flag(trap_exit, true),
    Options = [binary, {ip, IpAddress}, {active, false}],
    case gen_udp:open(Port, Options) of
        {ok, Socket} ->
            ok = node_serv:handshake(NodeServ, {?MODULE, Na, self()}),
            ok = node_tunnel_recv_serv:handshake(
                   NodeTunnelRecvServ, {?MODULE, Socket, self()}),
            Parent ! {self(), started},
            loop(#state{parent = Parent,
                        na = Na,
                        peer_na = PeerNa,
                        socket = Socket});
        {error, Reason} ->
            Parent ! {self(), {udp_failure, Reason}}
    end.

loop(#state{parent = Parent,
            na = _Na,
            peer_na = PeerNa,
            socket = Socket} = S) ->
    receive
        Cell when is_binary(Cell) ->
            udp_send(PeerNa, Socket, Cell),
            loop(S);
        {ip_packet, {Oa0, Oa1, Oa2, Oa3, Oa4, Oa5, Oa6, Oa7}, Packet} ->
            Length = size(Packet),
            Cell =
                <<?IP_PACKET:4,
                  Oa0:16, Oa1:16, Oa2:16, Oa3:16, Oa4:16, Oa5:16, Oa6:16,
                  Oa7:16,
                  Length:12,
                  Packet/binary>>,
            udp_send(PeerNa, Socket, Cell),
            loop(S);
        % FIXME
        #routing_entry{noa = {Oa0, Oa1, Oa2, Oa3, Oa4, Oa5, Oa6, Oa7},
                       path_cost = Pc, nhops = Hops} ->
            NumberOfHops = length(Hops),
            BinaryHops =
                ?l2b([<<Na0:8, Na1:8, Na2:8, Na3:8, Port:16>> ||
                         {{Na0, Na1, Na2, Na3}, Port} <- Hops]),
            Cell =
                <<?ROUTING_ENTRY:4,
                  Oa0:16, Oa1:16, Oa2:16, Oa3:16, Oa4:16, Oa5:16, Oa6:16,
                  Oa7:16,
                  Pc:16,
                  NumberOfHops:4,
                  BinaryHops/binary>>,
            udp_send(PeerNa, Socket, Cell),
            loop(S);
        #echo_request{sequence_number = SeqNumber, timestamp = Timestamp} ->
            Cell = <<?ECHO_REQUEST:4, SeqNumber:12, Timestamp:32>>,
            udp_send(PeerNa, Socket, Cell),
            loop(S);
	{From, stop} ->
	    From ! {self(), ok};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

udp_send({PeerIpAddress, PeerPort}, Socket, Cell) ->
    case gen_udp:send(Socket, PeerIpAddress, PeerPort, Cell) of
        ok ->
            ok;
        {error, Reason} ->
            ?daemon_log("Sending data to ~s:~w failed: ~s",
                        [inet_tools:string_address(PeerIpAddress), PeerPort,
                         inet:format_error(Reason)])
    end.
