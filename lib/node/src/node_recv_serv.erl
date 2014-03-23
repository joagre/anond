-module(node_recv_serv).

%%% external exports
-export([start_link/2, stop/1, stop/2]).
-export([handshake/2]).

%%% internal exports
-export([init/3, receiver/1]).

%%% include files
-include_lib("node/include/node.hrl").
-include_lib("node/include/node_path_cost.hrl").
-include_lib("node/include/node_recv_send.hrl").
-include_lib("node/include/node_route.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants

%%% records
-record(serv_state, {
          parent   :: pid(),
          receiver :: pid(),
          socket   :: gen_udp:socket(),
          %% anond.conf parameters
          my_na    :: na()
         }).

-record(receiver_state, {
          node_db             :: node_db(),
          route_db            :: route_db(),
          node_route_serv     :: pid(),
          node_path_cost_serv :: pid(),
          tun_fd              :: node_tun_serv:tun_fd(),
          socket              :: gen_udp:socket(),
          %% anond.conf parameters
          my_na               :: na(),
          my_oa               :: oa(),
          public_key          :: node_crypto:pki_key(),
          private_key         :: node_crypto:pki_key()
	 }).

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link(na(), supervisor:sup_ref()) ->
                        {'ok', pid()} |
                        {'error', {'udp_failure', inet:posix()}}.

start_link(MyNa, NodeInstanceSup) ->
    Args = [self(), MyNa, NodeInstanceSup],
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

-spec handshake(pid(),
                'node_send_serv' |
                {'node_path_cost_serv', pid()} |
                {'node_tun_serv', node_tun_serv:tun_fd()}) ->
                       {'ok', gen_udp:socket()} | 'ok'.

handshake(NodeRecvServ, node_send_serv) ->
    serv:call(NodeRecvServ, {handshake, node_send_serv});
handshake(NodeRecvServ, {node_tun_serv, TunFd}) ->
    NodeRecvServ ! {handshake, {node_tun_serv, TunFd}},
    ok;
handshake(NodeRecvServ, {node_path_cost_serv, NodePathCostServ}) ->
    NodeRecvServ ! {handshake, {node_path_cost_serv, NodePathCostServ}},
    ok.

%%%
%%% server loop
%%%

init(Parent, MyNa, NodeInstanceSup) ->
    process_flag(trap_exit, true),
    {MyIpAddress, MyPort} = MyNa,
    Options = [binary, {ip, MyIpAddress}, {active, false}],
    case gen_udp:open(MyPort, Options) of
        {ok, Socket} ->
            Parent ! {self(), started},
            {ok, NodeRouteServ} =
                node_instance_sup:lookup_child(NodeInstanceSup,
                                               node_route_serv),
            {ok, NodeDb, RouteDb} =
                node_route_serv:handshake(NodeRouteServ, ?MODULE),
            S = read_config(#receiver_state{
                               my_na = MyNa,
                               node_db = NodeDb,
                               route_db = RouteDb,
                               node_route_serv = NodeRouteServ,
                               socket = Socket}),
            Receiver = proc_lib:spawn_link(?MODULE, receiver, [S]),
            loop(#serv_state{parent = Parent, my_na = MyNa, receiver = Receiver,
                             socket = Socket});
        {error, Reason} ->
            Parent ! {self(), {udp_failure, Reason}}
    end.

loop(#serv_state{parent = Parent,
                 receiver = Receiver,
                 socket = Socket,
                 my_na = {MyIpAddress, MyPort}} = S) ->
    receive
        {From, {handshake, node_send_serv}} ->
            From ! {self(), {ok, Socket}},
            loop(S);
        {handshake, {node_tun_serv, TunFd}} ->
            Receiver ! {node_tun_serv, TunFd},
            ok = gen_udp:send(Socket, MyIpAddress, MyPort,
                              <<?MESSAGE_ARRIVED:8>>),
            loop(S);
        {handshake, {node_path_cost_serv, NodePathCostServ}} ->
            Receiver ! {node_path_cost_serv, NodePathCostServ},
            ok = gen_udp:send(Socket, MyIpAddress, MyPort,
                              <<?MESSAGE_ARRIVED:8>>),
            loop(S);
	{From, stop} ->
            gen_udp:close(Socket),
	    From ! {self(), ok};
        {'EXIT', Parent, Reason} ->
            gen_udp:close(Socket),
            exit(Reason);
        {'EXIT', Receiver, Reason} ->
            gen_udp:close(Socket),
            exit(Reason);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

receiver(#receiver_state{node_db = NodeDb,
                         route_db = RouteDb,
                         node_route_serv = NodeRouteServ,
                         node_path_cost_serv = NodePathCostServ,
                         tun_fd = TunFd,
                         socket = Socket,
                         my_na = {MyIpAddress, MyPort} = MyNa,
                         my_oa = MyOa} = S) ->
    case gen_udp:recv(Socket, 0) of
        {ok, {MyIpAddress, MyPort, <<?MESSAGE_ARRIVED:8>>}} ->
            receive
                {node_path_cost_serv, NewNodePathCostServ} ->
                    receiver(S#receiver_state{
                               node_path_cost_serv = NewNodePathCostServ});
                {node_tun_serv, NewTunFd} ->
                    receiver(S#receiver_state{tun_fd = NewTunFd})
            end;
        {ok, {NeighbourIpAddress, NeighbourIpPort, Cell}} ->
            handle_cell(
              NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyNa,
              MyOa, NeighbourIpAddress, NeighbourIpPort, Cell),
            receiver(S);
        {error, Reason} ->
            ?daemon_log("UDP failure on ~s:~w: ~s",
                        [net_tools:string_address(MyIpAddress), MyPort,
                         inet:format_error(Reason)]),
            receiver(S)
    end.

%% write ipv6 packet to tun device
handle_cell(
  NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyNa,
  {MyOa0, MyOa1, MyOa2, MyOa3, MyOa4, MyOa5, MyOa6, MyOa7} = MyOa,
  NeighbourIpAddress, NeighbourPort,
  <<?IP_PACKET:8,
    MyOa0:16, MyOa1:16, MyOa2:16, MyOa3:16,
    MyOa4:16, MyOa5:16, MyOa6:16, MyOa7:16,
    Ipv6PacketSize:16,
    Ipv6Packet:Ipv6PacketSize/binary,
    Rest/binary>>) ->
    case TunFd of
        undefined ->
            handle_cell(
              NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyNa,
              MyOa, NeighbourIpAddress, NeighbourPort, Rest);
        _ ->
            case tuncer:write(TunFd, Ipv6Packet) of
                ok ->
                    handle_cell(
                      NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd,
                      MyNa, MyOa, NeighbourIpAddress, NeighbourPort, Rest);
                {error, Reason} ->
                    ?daemon_log("Can not write to tun device: ~s",
                                [inet:format_error(Reason)]),
                    handle_cell(
                      NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd,
                      MyNa, MyOa, NeighbourIpAddress, NeighbourPort, Rest)
            end
    end;
%% forward ip packet to appropriate neighbour node
handle_cell(
  NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyNa, MyOa,
  NeighbourIpAddress, NeighbourPort,
  <<?IP_PACKET:8,
    DestOa0:16, DestOa1:16, DestOa2:16, DestOa3:16,
    DestOa4:16, DestOa5:16, DestOa6:16, DestOa7:16,
    Ipv6PacketSize:16,
    Ipv6Packet:Ipv6PacketSize/binary,
    Rest/binary>>) ->
    DestOa = {DestOa0, DestOa1, DestOa2, DestOa3,
              DestOa4, DestOa5, DestOa6, DestOa7},
    send(DestOa, NodeDb, RouteDb, Ipv6Packet),
    handle_cell(
      NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyNa, MyOa,
      NeighbourIpAddress, NeighbourPort, Rest);
%% send route entry to node_route_serv
handle_cell(
  NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyNa, MyOa,
  NeighbourIpAddress, NeighbourPort,
  <<?ROUTE_ENTRY:8,
    Oa0:16, Oa1:16, Oa2:16, Oa3:16, Oa4:16, Oa5:16, Oa6:16, Oa7:16,
    Pc:16/signed-integer,
    HopsSize:16,
    Hops:HopsSize/binary,
    PspSize:16,
    Psp:PspSize/binary,
    Rest/binary>>) ->
    Oa = {Oa0, Oa1, Oa2, Oa3, Oa4, Oa5, Oa6, Oa7},
    case decode_hops(Hops) of
        {ok, DecodedHops} ->
            NeighbourNa = {NeighbourIpAddress, NeighbourPort},
            Re = #route_entry{
              oa = Oa,
              na = NeighbourNa,
              path_cost = Pc,
              hops = [NeighbourNa|DecodedHops],
              psp = Psp
             },
            ok = node_route_serv:route_entry(NodeRouteServ, Re),
            handle_cell(
              NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyNa,
              MyOa, NeighbourIpAddress, NeighbourPort, Rest);
        {error, Reason} ->
            ?dbg_log(Reason),
            handle_cell(
              NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyNa,
              MyOa, NeighbourIpAddress, NeighbourPort, Rest)
    end;
%% reply to echo request
handle_cell(
  NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyNa, MyOa,
  NeighbourIpAddress, NeighbourPort,
  <<?ECHO_REQUEST:8,
    SeqNumber:16,
    UniqueId:16,
    Timestamp:32,
    Rest/binary>>) ->
    EchoReply = <<?ECHO_REPLY:8, SeqNumber:16, UniqueId:16, Timestamp:32>>,
    send({NeighbourIpAddress, NeighbourPort}, NodeDb, RouteDb, EchoReply),
    handle_cell(
      NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyNa, MyOa,
      NeighbourIpAddress, NeighbourPort, Rest);
%% send echo reply to path cost server
handle_cell(
  NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyNa, MyOa,
  NeighbourIpAddress, NeighbourPort,
  <<?ECHO_REPLY:8,
    SeqNumber:16,
    UniqueId:16,
    Timestamp:32,
    Rest/binary>>) ->
    EchoReply = #echo_reply{sequence_number = SeqNumber, unique_id = UniqueId,
                            timestamp = Timestamp},
    ok = node_path_cost_serv:echo_reply(NodePathCostServ, EchoReply),
    handle_cell(
      NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyNa, MyOa,
      NeighbourIpAddress, NeighbourPort, Rest);
%% throw away padding
handle_cell(
  _NodeDb, _RouteDb, _NodeRouteServ, _NodePathCostServ, _TunFd, _MyNa, _MyOa,
  _NeighbourIpAddress, _NeighbourPort, <<?PADDING:8, _/binary>>) ->
    ok;
%% no padding to throw away
handle_cell(
  _NodeDb, _RouteDb, _NodeRouteServ, _NodePathCostServ, _TunFd, _MyNa, _MyOa,
  _NeighbourIpAddress, _NeighbourPort, <<>>) ->
    ok;
%% unknown cell fragment
handle_cell(
  _NodeDb, _RouteDb, _NodeRouteServ, _NodePathCostServ, _TunFd, _MyNa, _MyOa,
  NeighbourIpAddress, NeighbourPort, Data) ->
    ?daemon_log("An unknown cell fragment received from ~s:~w: ~w",
                [net_tools:string_address(NeighbourIpAddress), NeighbourPort,
                 Data]).

send(DestOaOrNa, NodeDb, RouteDb, Data) ->
    case node_route:lookup_node_send_serv(NodeDb, RouteDb, DestOaOrNa) of
        {ok, NodeSendServ} ->
            ok = node_send_serv:send(NodeSendServ, {?MODULE, Data});
        {error, Reason} ->
            ?dbg_log(Reason),
            ok
    end.

decode_hops(Hops) ->
    decode_hops(Hops, []).

decode_hops(<<>>, Acc) ->
    {ok, lists:reverse(Acc)};
decode_hops(<<Na0:8, Na1:8, Na2:8, Na3:8, Port:16, Rest/binary>>, Acc) ->
    decode_hops(Rest, [{{Na0, Na1, Na2, Na3}, Port}|Acc]);
decode_hops(Hops, Acc) ->
    {error, {invalid_hops, Hops, Acc}}.

%%%
%%% init
%%%

read_config(S) ->
    NodeInstance = ?config([nodes, {'node-address', S#receiver_state.my_na}]),
    read_config(S, NodeInstance).

read_config(S, []) ->
    S;
read_config(S, [{'overlay-addresses', [MyOa]}|Rest]) ->
    read_config(S#receiver_state{my_oa = MyOa}, Rest);
read_config(_S, [{'overlay-addresses', _Oas}|_Rest]) ->
    throw(nyi);
read_config(S, [{'public-key', Value}|Rest]) ->
    Key = node_crypto:read_pki_key(Value),
    read_config(S#receiver_state{public_key = Key}, Rest);
read_config(S, [{'private-key', Value}|Rest]) ->
    Key = node_crypto:read_pki_key(Value),
    read_config(S#receiver_state{private_key = Key}, Rest);
read_config(S, [_|Rest]) ->
    read_config(S, Rest).
