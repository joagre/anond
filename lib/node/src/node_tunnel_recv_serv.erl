-module(node_tunnel_recv_serv).

%%% external exports
-export([start_link/4, stop/1, stop/2]).
-export([handshake/2]).

%%% internal exports
-export([init/7, receiver/1]).

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
          parent                :: pid(),
          oa                    :: noa(),
          na                    :: na(),
          peer_na               :: na(),
          node_serv             :: pid(),
          node_db               :: node_db(),
          route_db            :: route_db(),
          node_path_cost_serv   :: pid(),
          tun_device            :: pid(),
          socket                :: gen_udp:socket(),
          receiver              :: pid(),
          send_serv             :: pid()
	 }).

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link(noa(), na(), na(), supervisor:sup_ref()) -> {'ok', pid()}.

start_link(Oa, Na, PeerNa, NodeSup) ->
    {ok, NodeServ} = node_sup:lookup_child(NodeSup, node_serv),
    {ok, NodePathCostServ} =
        node_sup:lookup_child(NodeSup, node_path_cost_serv),
    {ok, NodeTunServ} = node_sup:lookup_child(NodeSup, node_tun_serv),
    Args = [self(), Oa, Na, PeerNa, NodeServ, NodePathCostServ, NodeTunServ],
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

-spec stop(pid()) -> 'ok'.

stop(Pid) ->
    stop(Pid, 15000).

-spec stop(pid(), timeout()) -> 'ok'.

stop(Pid, Timeout) ->
    serv:call(Pid, stop, Timeout).

%%%
%%% exported: handshake
%%%

-spec handshake(pid(), {'node_tunnel_send_serv', gen_udp:socket(), pid()}) ->
                       'ok'.

handshake(NodeTunnelRecvServ,
          {node_tunnel_send_serv, Socket, NodeTunnelSendServ}) ->
    NodeTunnelRecvServ !
        {handshake, {node_tunnel_send_serv, Socket, NodeTunnelSendServ}},
    ok.

%%%
%%% server loop
%%%

init(Parent, Oa, Na, PeerNa, NodeServ, NodePathCostServ, _NodeTunServ) ->
    {ok, NodeDb, RouteDb} = node_serv:handshake(NodeServ, ?MODULE),
%    {ok, TunDevice} = node_tun_serv:handshake(NodeTunServ, ?MODULE),
    TunDevice = self(),
    Parent ! {self(), started},
    loop(#state{parent = Parent, oa = Oa, na = Na, peer_na = PeerNa,
                node_serv = NodeServ, node_db = NodeDb, route_db = RouteDb,
                node_path_cost_serv = NodePathCostServ,
                tun_device = TunDevice}).

loop(#state{parent = Parent,
            receiver = Receiver} = S) ->
    receive
        {handshake, {node_tunnel_send_serv, Socket, NodeTunnelSendServ}} ->
            UpdatedS = S#state{socket = Socket, send_serv = NodeTunnelSendServ},
            NewReceiver = proc_lib:spawn_link(?MODULE, receiver, [UpdatedS]),
            loop(UpdatedS#state{receiver = NewReceiver});
	{From, stop} ->
	    From ! {self(), ok};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        {'EXIT', Receiver, Reason} ->
            exit(Reason);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

receiver(#state{node_db = NodeDb,
                route_db = RouteDb,
                oa = {Oa0, Oa1, Oa2, Oa3, Oa4, Oa5, Oa6, Oa7},
                peer_na = {PeerIpAddress, PeerPort},
                node_serv = NodeServ,
                node_path_cost_serv = NodePathCostServ,
                tun_device = TunDevice,
                socket = Socket,
                send_serv = NodeTunnelSendServ} = S) ->
    case gen_udp:recv(Socket, 0) of
        %% send ip packet to tun device
        {ok, {PeerIpAddress, PeerPort,
              <<?IP_PACKET:4,
                Oa0:16, Oa1:16, Oa2:16, Oa3:16, Oa4:16, Oa5:16, Oa6:16, Oa7:16,
                _Length:12, IpPacket/binary>>}} ->
            ok = tuncer:send(TunDevice, IpPacket),
            receiver(S);
        %% forward ip packet to appropriate peer node 
        {ok, {PeerIpAddress, PeerPort,
              <<?IP_PACKET:4,
                A0:16, A1:16, A2:16, A3:16, A4:16, A5:16, A6:16, A7:16,
                _Length:12,
                _IpPacket/binary>> = Cell}} ->
            Oa = {A0, A1, A2, A3, A4, A5, A6, A7},
            %% is this too expensive? angst!!!
            case node_route:lookup_send_serv(NodeDb, RouteDb, Oa) of
                {ok, NodeTunnelSendServ} ->
                    ok = node_tunnel_send_serv:send(NodeTunnelSendServ, Cell),
                    receiver(S);
                {error, Reason} ->
                    ?error_log({lookup_send_serv, Oa, Reason}),
                    receiver(S)
            end;
        %% send route entry to node_serv
        {ok, {PeerIpAddress, PeerPort,
              <<?ROUTE_ENTRY:4,
                A0:16, A1:16, A2:16, A3:16, A4:16, A5:16, A6:16, A7:16,
                Pc:16,
                _NumberOfHops:4,
                BinaryHops/binary>>}} ->
            Oa = {A0, A1, A2, A3, A4, A5, A6, A7},
            Hops = mk_hops(BinaryHops),
            Re = #route_entry{
% FIXME
%              oa = Oa,
              na = {PeerIpAddress, PeerPort},
              path_cost = Pc,
              hops = Hops},
            ok = node_serv:route_entry(NodeServ, Re),
            receiver(S);
        %% reply to echo request
        {ok, {PeerIpAddress, PeerPort,
              <<?ECHO_REQUEST:4, UniqueId:16, SeqNumber:12, Timestamp:32>>}} ->
            Cell = <<?ECHO_REPLY:4, UniqueId:16, SeqNumber:12, Timestamp:32>>,
            ok = node_tunnel_send_serv:send(NodeTunnelSendServ, Cell),
            receiver(S);
        %% send echo reply to path cost server
        {ok, {PeerIpAddress, PeerPort,
              <<?ECHO_REPLY:4, UniqueId:16, SeqNumber:12, Timestamp:32>>}} ->
            EchoReply =
                #echo_reply{unique_id = UniqueId,
                            sequence_number = SeqNumber,
                            timestamp = Timestamp},
            ok = path_cost_serv:echo_reply(NodePathCostServ, EchoReply),
            receiver(S);
        {ok, {IpAddress, Port, Cell}} ->
            ?daemon_log("An unknown packet cell received from ~s:~w: ~s",
                        [net_tools:string_address(IpAddress), Port, Cell]),
            receiver(S);
        {error, Reason} ->
            ?daemon_log("Sending data to ~s:~w failed: ~s",
                        [net_tools:string_address(PeerIpAddress), PeerPort,
                         inet:format_error(Reason)]),
            receiver(S)
    end.

mk_hops(<<>>) ->
    [];
mk_hops(<<Na0:8, Na1:8, Na2:8, Na3:8, Port:16, Rest/binary>>) ->
    [{{Na0, Na1, Na2, Na3}, Port}|mk_hops(Rest)].
