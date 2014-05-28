-module(node_recv_serv).

%%% external exports
-export([start_link/2, stop/1, stop/2]).
-export([handshake/2]).
-export([ds_register/4]).
-export([ds_establish_tunnels/2, ds_unestablish_tunnels/2]).
-export([decode_hops/1]).

%%% internal exports
-export([init/3, udp_receiver/1]).

%%% include files
-include_lib("ds/include/ds.hrl").
-include_lib("node/include/node.hrl").
-include_lib("node/include/node_path_cost.hrl").
-include_lib("node/include/node_recv_send.hrl").
-include_lib("node/include/node_route.hrl").
-include_lib("util/include/bits.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants
-define(MESSAGE_AWAITS, 16#ff).
-define(DS_REGISTER_INTERVAL, {3, seconds}).
-define(DS_KEEPALIVE_INTERVAL, {10, seconds}).
-define(DS_ESTABLISH_TUNNELS_INTERVAL, {5, seconds}).
-define(NETWORK_TOPOLOGY_FRAGMENT_SIZE, 480). % bytes

%%% records
-record(state, {
          parent                              :: pid(),
          node_instance_sup                   :: supervisor:sup_ref(),
          node_db                             :: node_db(),
          route_db                            :: route_db(),
          node_route_serv                     :: pid(),
          udp_receiver                        :: pid(),
          socket                              :: gen_udp:socket(),
          my_node_id                          :: node_id(),
          ds_id                               :: ds_id(),
          ds_shared_key                       :: binary(),
          ds_register_message_id              :: message_id() | 'done' |
                                                 'undefined',
          ds_keepalive_started = no           :: 'yes' | 'no',
          ds_establish_tunnel_messages = []   :: [{node_id(), message_id()}],
          node_establish_tunnel_messages = [] :: [{node_id(), message_id()}],
          next_message_id = 0                 :: message_id(),
          %% anond.conf parameters
          my_na                               :: na(),
          logging                             :: boolean(),
          ds_ip_address_port                  :: {inet:ip_address(),
                                                  inet:port_number()}
         }).

-record(udp_receiver_state, {
          parent              :: pid(),
          node_db             :: node_db(),
          route_db            :: route_db(),
          node_route_serv     :: pid(),
          node_path_cost_serv :: pid(),
          tun_fd              :: node_tun_serv:tun_fd(),
          socket              :: gen_udp:socket(),
          my_node_id          :: node_id(),
          ds_id               :: ds_id(),
          ds_shared_key       :: binary(),
          ds_ip_address_port  :: {inet:ip_address(), inet:port_number()},
          %% anond.conf parameters
          my_na               :: na(),
          logging             :: boolean(),
          my_oa               :: oa()
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

stop(Pid) ->
    stop(Pid, 15000).

-spec stop(pid(), timeout()) -> 'ok'.

stop(Pid, Timeout) ->
    serv:call(Pid, stop, Timeout).

%%%
%%% exported: handshake
%%%

-spec handshake(pid(),
                {'node_route_serv', node_db(), route_db(), pid()} |
                {'node_tun_serv', node_tun_serv:tun_fd()} |
                {'node_path_cost_serv', pid()}) ->
                       {'ok', gen_udp:socket()} | 'ok'.

handshake(NodeRecvServ, {node_route_serv, NodeDb, RouteDb, NodeRouteServ}) ->
    NodeRecvServ !
        {handshake, {node_route_serv, NodeDb, RouteDb, NodeRouteServ}},
    ok;
handshake(NodeRecvServ, {node_tun_serv, TunFd}) ->
    NodeRecvServ ! {handshake, {node_tun_serv, TunFd}},
    ok;
handshake(NodeRecvServ, {node_path_cost_serv, NodePathCostServ}) ->
    NodeRecvServ ! {handshake, {node_path_cost_serv, NodePathCostServ}},
    ok.

%%%
%%% exported: ds_register
%%%

-spec ds_register(pid(), node_id(), ds_id(), binary()) -> 'ok'.

ds_register(NodeRecvServ, MyNodeId, DsId, SharedKey) ->
    NodeRecvServ ! {ds_register, MyNodeId, DsId, SharedKey},
    ok.

%%%
%%% exported: ds_establish_tunnels
%%%

-spec ds_establish_tunnels(pid(), [node_id()]) -> 'ok'.

ds_establish_tunnels(NodeRecvServ, DestNodeIds) ->
    NodeRecvServ ! {ds_establish_tunnels, DestNodeIds},
    ok.

%%%
%%% exported: ds_unestablish_tunnels
%%%

-spec ds_unestablish_tunnels(pid(), [node_id()]) -> 'ok'.

ds_unestablish_tunnels(NodeRecvServ, DestNodeIds) ->
    NodeRecvServ ! {ds_unestablish_tunnels, DestNodeIds},
    ok.

%%%
%%% server loop
%%%

init(Parent, MyNa, NodeInstanceSup) ->
    process_flag(trap_exit, true),
    ok = config_json_serv:subscribe(),
    {MyIpAddress, MyPort} = MyNa,
    Options = [binary, {ip, MyIpAddress}, {active, false}],
    case gen_udp:open(MyPort, Options) of
        {ok, Socket} ->
            UdpReceiverS =
                read_udp_receiver_config(
                  #udp_receiver_state{
                     parent = self(), socket = Socket, my_na = MyNa}),
            UdpReceiver =
                proc_lib:spawn_link(?MODULE, udp_receiver, [UdpReceiverS]),
            ok = log_serv:toggle_logging(
                   UdpReceiver, UdpReceiverS#udp_receiver_state.logging),
            S = read_config(
                  #state{parent = Parent, node_instance_sup = NodeInstanceSup,
                         udp_receiver = UdpReceiver, socket = Socket,
                         my_na = MyNa}),
            ok = log_serv:toggle_logging(self(), S#state.logging),
            Parent ! {self(), started},
            loop(S);
        {error, Reason} ->
            Parent ! {self(), {udp_failure, Reason}}
    end.

loop(#state{parent = Parent,
            node_instance_sup = NodeInstanceSup,
            node_db = NodeDb,
            route_db = RouteDb,
            node_route_serv = NodeRouteServ,
            udp_receiver = UdpReceiver,
            socket = Socket,
            my_node_id = MyNodeId,
            ds_id = _DsId,
            ds_shared_key = DsSharedKey,
            ds_register_message_id = DsRegisterMessageId,
            ds_keepalive_started = DsKeepaliveStarted,
            ds_establish_tunnel_messages = DsEstablishTunnelMessages,
            node_establish_tunnel_messages = NodeEstablishTunnelMessages,
            next_message_id = NextMessageId,
            my_na = {MyIpAddress, MyPort} = MyNa,
            logging = _Logging,
            ds_ip_address_port = DsIpAddressPort =
                {DsIpAddress, DsPort}} = S) ->
    receive
        config_updated ->
            ?daemon_log("Node ~w (~s) starts to update its configuration",
                        [MyNodeId, net_tools:string_address(MyNa)]),
            UdpReceiver ! config_updated,
            ok = gen_udp:send(Socket, MyIpAddress, MyPort,
                              <<?MESSAGE_AWAITS:8>>),
            loop(read_config(S));
        {handshake,
         {node_route_serv, NewNodeDb, NewRouteDb, NewNodeRouteServ}} ->
            UdpReceiver !
                {node_route_serv, NewNodeDb, NewRouteDb, NewNodeRouteServ},
            ok = gen_udp:send(Socket, MyIpAddress, MyPort,
                              <<?MESSAGE_AWAITS:8>>),
            loop(S#state{node_db = NewNodeDb, route_db = NewRouteDb,
                         node_route_serv = NewNodeRouteServ});
        {handshake, {node_tun_serv, TunFd}} ->
            UdpReceiver ! {node_tun_serv, TunFd},
            ok = gen_udp:send(Socket, MyIpAddress, MyPort,
                              <<?MESSAGE_AWAITS:8>>),
            loop(S);
        {handshake, {node_path_cost_serv, NodePathCostServ}} ->
            UdpReceiver ! {node_path_cost_serv, NodePathCostServ},
            ok = gen_udp:send(Socket, MyIpAddress, MyPort,
                              <<?MESSAGE_AWAITS:8>>),
            loop(S);
        ds_register when DsRegisterMessageId == done ->
            loop(S#state{ds_register_message_id = undefined});
        ds_register ->
            RandomBytes = salt:crypto_random_bytes(42),
            Payload = <<?DS_REGISTER:8, NextMessageId:24, RandomBytes/binary>>,
            Nonce = salt:crypto_random_bytes(24),
            send_ds_message(MyNodeId, Socket, DsIpAddress, DsPort, Nonce,
                            DsSharedKey, Payload),
            timelib:start_timer(?DS_REGISTER_INTERVAL, ds_register),
            IncrementedNextMessageId = next_message_id(NextMessageId),
            loop(S#state{ds_register_message_id = NextMessageId,
                         next_message_id = IncrementedNextMessageId});
        {ds_register, NewMyNodeId, NewDsId, NewDsSharedKey} ->
            ?daemon_log(
               "Node ~w (~s) tries to register itself with directory server",
               [NewMyNodeId, net_tools:string_address(MyNa)]),
            UdpReceiver ! {ds_register, NewMyNodeId, NewDsId, NewDsSharedKey,
                           DsIpAddressPort},
            ok = gen_udp:send(Socket, MyIpAddress, MyPort,
                              <<?MESSAGE_AWAITS:8>>),
            self() ! ds_register,
            loop(S#state{my_node_id = NewMyNodeId, ds_id = NewDsId,
                         ds_shared_key = NewDsSharedKey});
        {node_registered, DsRegisterMessageId} when DsKeepaliveStarted == yes ->
            ?daemon_log(
               "Node ~w (~s) succeeded to register with directory server",
               [MyNodeId, net_tools:string_address(MyNa)]),
            loop(S#state{ds_register_message_id = done});
        {node_registered, DsRegisterMessageId} ->
            ?daemon_log(
               "Node ~w (~s) succeeded to register with directory server and "
               "starts to send keepalive messages to it",
               [MyNodeId, net_tools:string_address(MyNa)]),
            self() ! ds_keepalive,
            loop(S#state{ds_register_message_id = done,
                         ds_keepalive_started = yes});
        {node_registered, StaleDsRegisterMessageId} ->
            ?dbg_log({stale_ds_register_message_id, StaleDsRegisterMessageId}),
            loop(S);
        ds_keepalive ->
            RandomBytes = salt:crypto_random_bytes(45),
            Payload = <<?DS_KEEPALIVE:8, RandomBytes/binary>>,
            Nonce = salt:crypto_random_bytes(24),
            send_ds_message(MyNodeId, Socket, DsIpAddress, DsPort, Nonce,
                            DsSharedKey, Payload),
            timelib:start_timer(?DS_KEEPALIVE_INTERVAL, ds_keepalive),
            loop(S);
        ds_establish_tunnels when DsEstablishTunnelMessages == [] ->
            ?daemon_log(
               "Node ~w (~s) has established all tunnels",
               [MyNodeId, net_tools:string_address(MyNa)]),
            loop(S);
        ds_establish_tunnels ->
            DestNodeIds =
                [DestNodeId || {DestNodeId, _} <- DsEstablishTunnelMessages],
            ?daemon_log(
               "Node ~w (~s) retries to establish tunnels with nodes ~w",
               [MyNodeId, net_tools:string_address(MyNa), DestNodeIds]),
            {NewDsEstablishTunnelMessages, IncrementedNextMessageId} =
                send_ds_establish_tunnel_messages(
                  MyNodeId, Socket, DsSharedKey, DsIpAddress, DsPort,
                  NextMessageId, DestNodeIds, []),
            timelib:start_timer(
              ?DS_ESTABLISH_TUNNELS_INTERVAL, ds_establish_tunnels),
            loop(S#state{
                   ds_establish_tunnel_messages = NewDsEstablishTunnelMessages,
                   next_message_id = IncrementedNextMessageId});
        {ds_establish_tunnels, DestNodeIds} ->
            ?daemon_log(
               "Node ~w (~s) tries to establish tunnels with nodes ~w",
               [MyNodeId, net_tools:string_address(MyNa), DestNodeIds]),
            {NewDsEstablishTunnelMessages, IncrementedNextMessageId} =
                send_ds_establish_tunnel_messages(
                  MyNodeId, Socket, DsSharedKey, DsIpAddress, DsPort,
                  NextMessageId, DestNodeIds, []),
            case DsEstablishTunnelMessages of
                [] ->
                    timelib:start_timer(
                      ?DS_ESTABLISH_TUNNELS_INTERVAL, ds_establish_tunnels);
                _ ->
                    ok
            end,
            loop(S#state{
                   ds_establish_tunnel_messages = NewDsEstablishTunnelMessages,
                   next_message_id = IncrementedNextMessageId});
        {node_establish_tunnel, MessageId, SrcNodeId, SrcIpAddress, SrcPort,
         SharedKey} ->
            ?daemon_log(
               "Node ~w (~s) got a request to establish a tunnel with node "
               "~w (~s)",
               [MyNodeId, net_tools:string_address(MyNa), SrcNodeId,
                net_tools:string_address({SrcIpAddress, SrcPort})]),
            case lists:keysearch(SrcNodeId, 1, NodeEstablishTunnelMessages) of
                {value, {SrcNodeId, LatestMessageId}}
                  when MessageId > LatestMessageId orelse
                       MessageId-LatestMessageId+?LARGEST_MESSAGE_ID/2 < 0 ->
                    Skip = false,
                    NewNodeEstablishTunnelMessages =
                        lists:keyreplace(
                          SrcNodeId, 1, NodeEstablishTunnelMessages,
                          {SrcNodeId, MessageId});
                {value, {SrcNodeId, _LatestMessageId}} ->
                    Skip = true,
                    NewNodeEstablishTunnelMessages =
                        lists:keyreplace(
                          SrcNodeId, 1, NodeEstablishTunnelMessages,
                          {SrcNodeId, MessageId});
                false ->
                    Skip = false,
                    NewNodeEstablishTunnelMessages =
                        [{SrcNodeId, MessageId}|NodeEstablishTunnelMessages]
            end,
            if
                Skip ->
                    ?dbg_log({stale_node_establish_message_id, MessageId}),
                    loop(S);
                true ->
                    ?daemon_log(
                       "Node ~w (~s) accepted a request to establish a tunnel "
                       "with node ~w (~s)",
                       [MyNodeId, net_tools:string_address(MyNa), SrcNodeId,
                        net_tools:string_address({SrcIpAddress, SrcPort})]),
                    RandomBytes = salt:crypto_random_bytes(38),
                    Payload = <<?DS_TUNNEL_ESTABLISHED:8, MessageId:24,
                                SrcNodeId:32, RandomBytes/binary>>,
                    Nonce = salt:crypto_random_bytes(24),
                    send_ds_message(
                      MyNodeId, Socket, DsIpAddress, DsPort, Nonce,
                      DsSharedKey, Payload),
                    update_node(
                      NodeInstanceSup, NodeRouteServ, Socket, MyNodeId, MyNa,
                      SrcNodeId, SrcIpAddress, SrcPort, SharedKey),
                    NewDsEstablishTunnelMessages =
                        lists:keydelete(
                          SrcNodeId, 1, DsEstablishTunnelMessages),
                    loop(S#state{
                           ds_establish_tunnel_messages =
                               NewDsEstablishTunnelMessages,
                           node_establish_tunnel_messages =
                               NewNodeEstablishTunnelMessages})
            end;
        {node_tunnel_established, MessageId, DestNodeId, DestIpAddress,
         DestPort, SharedKey} ->
            case lists:member(
                   {DestNodeId, MessageId}, DsEstablishTunnelMessages) of
                true ->
                    ?daemon_log(
                       "Node ~w (~s) succeeded to establish a tunnel with node "
                       "~w (~s)",
                       [MyNodeId, net_tools:string_address(MyNa), DestNodeId,
                        net_tools:string_address({DestIpAddress, DestPort})]),
                    update_node(
                      NodeInstanceSup, NodeRouteServ, Socket, MyNodeId, MyNa,
                      DestNodeId, DestIpAddress, DestPort, SharedKey),
                    case DsEstablishTunnelMessages of
                        [{DestNodeId, MessageId}] ->
                            ?daemon_log(
                               "Node ~w (~s) succeeded to establish tunnels "
                               "with *all* neighbouring nodes",
                               [MyNodeId, net_tools:string_address(MyNa)]),
                            loop(S#state{ds_establish_tunnel_messages = []});
                        _ ->
                            loop(S#state{
                                   ds_establish_tunnel_messages =
                                       lists:delete({DestNodeId, MessageId},
                                                    DsEstablishTunnelMessages)})
                    end;
                false ->
                    ?dbg_log({stale_ds_establish_tunnel_message_id, MessageId}),
                    loop(S)
            end;
        {get_network_topology, MessageId} ->
            {EncodedNeighbours, NumberOfNeighbours} =
                node_route:foldl_node(
                  fun(#node{node_id = NodeId, na = {{A, B, C, D}, Port},
                            path_cost = Pc, flags = Flags},
                      {Acc, NumberOf}) ->
                          {[<<NodeId:32, A:8, B:8, C:8, D:8, Port:16, Pc:16,
                              Flags:8>>|Acc],
                           NumberOf+1};
                     (#node{node_id = NodeId, na = undefined},
                      {Acc, NumberOf}) ->
                          ?error_log({missing_na, NodeId}),
                          {Acc, NumberOf}
                  end, {[], 0}, NodeDb),
            {EncodedRes, NumberOfRes} =
                node_route:foldl_route_entry(
                  fun(#route_entry{path_cost = Pc, hops = Hops},
                      {Acc, NumberOf}) ->
                          EncodedHops = encode_hops(Hops),
                          HopsSize = size(EncodedHops),
                          {[<<Pc:16, HopsSize:16, EncodedHops/binary>>|Acc],
                           NumberOf+1}
                  end, {[], 0}, RouteDb),
            EncodedNetworkTopology =
                <<NumberOfNeighbours:32, (?l2b(EncodedNeighbours))/binary,
                  NumberOfRes:32, (?l2b(EncodedRes))/binary>>,
            send_ds_network_topology(
              MyNodeId, Socket, DsIpAddress, DsPort, DsSharedKey,
              MessageId, EncodedNetworkTopology, 0),
            loop(S);
	{From, stop} ->
            gen_udp:close(Socket),
	    From ! {self(), ok};
        {'EXIT', Parent, Reason} ->
            gen_udp:close(Socket),
            exit(Reason);
        {'EXIT', UdpReceiver, Reason} ->
            gen_udp:close(Socket),
            exit(Reason);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

send_ds_message(MyNodeId, Socket, IpAddress, Port, Nonce, SharedKey, Payload) ->
    case catch salt:crypto_stream_xor(Payload, Nonce, SharedKey) of
        {'EXIT', Reason} ->
            ?error_log(Reason);
        EncryptedPayload ->
            Message = <<MyNodeId:32, Nonce/binary, EncryptedPayload/binary>>,
            case gen_udp:send(Socket, IpAddress, Port, Message) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?error_log(Reason)
            end
    end.

next_message_id(?LARGEST_MESSAGE_ID) ->
    0;
next_message_id(MessageId) ->
    MessageId+1.

send_ds_establish_tunnel_messages(
  _MyNodeId, _Socket, _DsSharedKey, _DsIpAddress, _DsPort, NextMessageId, [],
  Acc) ->
    {lists:reverse(Acc), next_message_id(NextMessageId)};
send_ds_establish_tunnel_messages(
  MyNodeId, Socket, DsSharedKey, DsIpAddress, DsPort, NextMessageId,
  [DestNodeId|Rest], Acc) ->
    RandomBytes = salt:crypto_random_bytes(38),
    Payload = <<?DS_ESTABLISH_TUNNEL:8, NextMessageId:24, DestNodeId:32,
                RandomBytes/binary>>,
    Nonce = salt:crypto_random_bytes(24),
    send_ds_message(MyNodeId, Socket, DsIpAddress, DsPort, Nonce, DsSharedKey,
                    Payload),
    send_ds_establish_tunnel_messages(
      MyNodeId, Socket, DsSharedKey, DsIpAddress, DsPort, NextMessageId, Rest,
      [{DestNodeId, NextMessageId}|Acc]).

update_node(NodeInstanceSup, NodeRouteServ, Socket, MyNodeId, MyNa,
            NeighbourNodeId, NeighbourIpAddress, NeighbourPort, SharedKey) ->
    case node_route_serv:lookup_node(NodeRouteServ, NeighbourNodeId) of
        %% my chosen neighbour also needs a node_send_serv
        [#node{node_send_serv = undefined, flags = Flags} = Node] ->
            NeighbourNa = {NeighbourIpAddress, NeighbourPort},
            {ok, NewNodeSendServ} =
                node_send_sup:start_node_send_serv(
                  NodeInstanceSup, Socket, MyNodeId, MyNa, NeighbourNodeId,
                  NeighbourNa, SharedKey),
            NewNode = Node#node{
                        na = NeighbourNa,
                        shared_key = SharedKey,
                        flags = ?bit_set(Flags, ?F_NODE_UPDATED),
                        node_send_serv = NewNodeSendServ},
            ok = node_route_serv:insert_node(NodeRouteServ, NewNode);
        %% no need to restart node_send_serv, i.e. ip-address and port the same
        [#node{na = {NeighbourIpAddress, NeighbourPort},
               node_send_serv = NodeSendServ} = Node] ->
            ok = node_send_serv:set_shared_key(NodeSendServ, SharedKey),
            NewNode = Node#node{shared_key = SharedKey},
            ok = node_route_serv:insert_node(NodeRouteServ, NewNode);
        %% ip-address or port (or both) has changed, i.e. restart node_send_serv
        [#node{node_send_serv = NodeSendServ, flags = Flags} = Node] ->
            ok = node_send_serv:stop(NodeSendServ),
            NeighbourNa = {NeighbourIpAddress, NeighbourPort},
            {ok, NewNodeSendServ} =
                node_send_sup:start_node_send_serv(
                  NodeInstanceSup, Socket, MyNodeId, MyNa, NeighbourNodeId,
                  NeighbourNa, SharedKey),
            NewNode = Node#node{
                        na = NeighbourNa,
                        shared_key = SharedKey,
                        flags = ?bit_set(Flags, ?F_NODE_UPDATED),
                        node_send_serv = NewNodeSendServ},
            ok = node_route_serv:insert_node(NodeRouteServ, NewNode);
        %% we have been chosen as a neighbour by another node
        [] ->
            NeighbourNa = {NeighbourIpAddress, NeighbourPort},
            {ok, NodeSendServ} =
                node_send_sup:start_node_send_serv(
                  NodeInstanceSup, Socket, MyNodeId, MyNa, NeighbourNodeId,
                  NeighbourNa, SharedKey),
            Node = #node{
              node_id = NeighbourNodeId,
              na = NeighbourNa,
              shared_key = SharedKey,
              path_cost = ?NODE_UNREACHABLE-1,
              flags = ?F_NODE_UPDATED bor ?F_NODE_IS_INCOMING_NEIGHBOUR,
              node_send_serv = NodeSendServ},
            ok = node_route_serv:insert_node(NodeRouteServ, Node)
    end.

send_ds_network_topology(
  MyNodeId, Socket, DsIpAddress, DsPort, DsSharedKey, MessageId, Fragment,
  FragmentCounter)
  when size(Fragment) < ?NETWORK_TOPOLOGY_FRAGMENT_SIZE ->
    Nonce = salt:crypto_random_bytes(24),
    FragmentSize = size(Fragment),
    Payload =
        <<?DS_NETWORK_TOPOLOGY:8, MessageId:24, FragmentCounter:16,
          FragmentSize:15, 1:1, Fragment:FragmentSize/binary>>,
    send_ds_message(MyNodeId, Socket, DsIpAddress, DsPort, Nonce, DsSharedKey,
                    Payload);
send_ds_network_topology(
  MyNodeId, Socket, DsIpAddress, DsPort, DsSharedKey, MessageId,
  <<Fragment:?NETWORK_TOPOLOGY_FRAGMENT_SIZE/binary, Rest/binary>>,
  FragmentCounter) ->
    Nonce = salt:crypto_random_bytes(24),
    Payload =
        <<?DS_NETWORK_TOPOLOGY:8, MessageId:24, FragmentCounter:16,
          ?NETWORK_TOPOLOGY_FRAGMENT_SIZE:15, 0:1,
          Fragment:?NETWORK_TOPOLOGY_FRAGMENT_SIZE/binary>>,
    send_ds_message(MyNodeId, Socket, DsIpAddress, DsPort, Nonce, DsSharedKey,
                    Payload),
    send_ds_network_topology(
      MyNodeId, Socket, DsIpAddress, DsPort, DsSharedKey, MessageId, Rest,
      FragmentCounter+1).

%%%
%%% udp receiver loop
%%%

udp_receiver(#udp_receiver_state{
                parent = Parent,
                node_db = NodeDb,
                route_db = RouteDb,
                node_route_serv = NodeRouteServ,
                node_path_cost_serv = NodePathCostServ,
                tun_fd = TunFd,
                socket = Socket,
                my_node_id = MyNodeId,
                ds_id = DsId,
                ds_shared_key = DsSharedKey,
                ds_ip_address_port = {DsIpAddress, DsPort},
                my_na = {MyIpAddress, MyPort} = MyNa,
                logging = _Logging,
                my_oa = MyOa} = S) ->
    case gen_udp:recv(Socket, 0) of
        {ok, {MyIpAddress, MyPort, <<?MESSAGE_AWAITS:8>>}} ->
            receive
                config_updated ->
                    ?daemon_log(
                       "Node ~w (~s) starts to update its configuration",
                       [MyNodeId, net_tools:string_address(MyNa)]),
                    udp_receiver(read_udp_receiver_config(S));
                {ds_register, NewMyNodeId, NewDsId, NewDsSharedKey,
                 NewDsIpAddressPort} ->
                    udp_receiver(S#udp_receiver_state{
                                   my_node_id = NewMyNodeId, ds_id = NewDsId,
                                   ds_shared_key = NewDsSharedKey,
                                   ds_ip_address_port = NewDsIpAddressPort});
                {node_route_serv, NewNodeDb, NewRouteDb, NewNodeRouteServ} ->
                    udp_receiver(S#udp_receiver_state{
                                   node_db = NewNodeDb, route_db = NewRouteDb,
                                   node_route_serv = NewNodeRouteServ});
                {node_tun_serv, NewTunFd} ->
                    udp_receiver(S#udp_receiver_state{tun_fd = NewTunFd});
                {node_path_cost_serv, NewNodePathCostServ} ->
                    udp_receiver(S#udp_receiver_state{
                                   node_path_cost_serv = NewNodePathCostServ});
                UnknownMessage ->
                    ?error_log({unknown_message, UnknownMessage}),
                    udp_receiver(S)
            end;
        {ok, {DsIpAddress, DsPort,
              <<DsId:32, Nonce:24/binary, Payload/binary>>}} ->
            case catch salt:crypto_stream_xor(Payload, Nonce, DsSharedKey) of
                {'EXIT', Reason} ->
                    ?error_log(Reason),
                    udp_receiver(S);
                DecryptedPayload ->
                    handle_ds_payload(Parent, MyNodeId, MyNa, DecryptedPayload),
                    udp_receiver(S)
            end;
        {ok, {NeighbourIpAddress, NeighbourPort,
              <<NeighbourNodeId:32, Nonce:24/binary, Cell/binary>>}} ->
            case node_route_serv:lookup_node(NodeRouteServ, NeighbourNodeId) of
                [] ->
                    ?error_log({unknown_node, MyNodeId, NeighbourNodeId}),
                    udp_receiver(S);
                [#node{shared_key = undefined}] ->
                    ?daemon_log(
                       "Node ~w (~s) has not established a tunnel with node "
                       " ~w (~s) yet",
                       [MyNodeId, net_tools:string_address(MyNa),
                        NeighbourNodeId,
                        net_tools:string_address(
                          {NeighbourIpAddress, NeighbourPort})]),
                    udp_receiver(S);
                [#node{shared_key = SharedKey}] ->
                    case catch salt:crypto_stream_xor(Cell, Nonce, SharedKey) of
                        {'EXIT', Reason} ->
                            ?error_log({Cell, Nonce, SharedKey, Reason}),
                            udp_receiver(S);
                        DecryptedCell ->
                            handle_cell(
                              NodeDb, RouteDb, NodeRouteServ, NodePathCostServ,
                              TunFd, MyOa, NeighbourNodeId, DecryptedCell),
                            udp_receiver(S)
                    end
            end;
        {error, Reason} ->
            ?error_log(Reason),
            udp_receiver(S)
    end.

handle_ds_payload(Parent, _MyNodeId, _MyNa,
                  <<?NODE_REGISTERED:8, MessageId:24,
                    _RandomBytes:42/binary>>) ->
    Parent ! {node_registered, MessageId};
handle_ds_payload(Parent, _MyNodeId, _MyNa,
                  <<?NODE_ESTABLISH_TUNNEL:8, MessageId:24, SrcNodeId:32,
                    A:8, B:8, C:8, D:8, SrcPort:16, SharedKey:32/binary>>) ->
    Parent ! {node_establish_tunnel, MessageId, SrcNodeId, {A, B, C, D},
              SrcPort, SharedKey};
handle_ds_payload(Parent, _MyNodeId, _MyNa,
                  <<?NODE_TUNNEL_ESTABLISHED:8, MessageId:24, DestNodeId:32,
                    A:8, B:8, C:8, D:8, DestPort:16, SharedKey:32/binary>>) ->
    Parent ! {node_tunnel_established, MessageId, DestNodeId, {A, B, C, D},
              DestPort, SharedKey};
handle_ds_payload(Parent, _MyNodeId, _MyNa,
                  <<?NODE_GET_NETWORK_TOPOLOGY:8, MessageId:24,
                    _RandomBytes:42/binary>>) ->
    Parent ! {get_network_topology, MessageId};
handle_ds_payload(_Parent, MyNodeId, MyNa, UnknownPayload) ->
    ?error_log({unknown_payload, MyNodeId, MyNa, UnknownPayload}).

%% ignore keepalive cells
handle_cell(
  NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyOa,
  NeighbourNodeId, <<?NODE_KEEPALIVE:8, Rest/binary>>) ->
    handle_cell(
      NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyOa,
      NeighbourNodeId, Rest);
%% write ipv6 packet to tun device
handle_cell(
  NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd,
  {MyOa0, MyOa1, MyOa2, MyOa3, MyOa4, MyOa5, MyOa6, MyOa7} = MyOa,
  NeighbourNodeId,
  <<?NODE_CELL_IP_PACKET:8,
    MyOa0:16, MyOa1:16, MyOa2:16, MyOa3:16,
    MyOa4:16, MyOa5:16, MyOa6:16, MyOa7:16,
    Ipv6PacketSize:16, Ipv6Packet:Ipv6PacketSize/binary, Rest/binary>>) ->
    case TunFd of
        undefined ->
            handle_cell(
              NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyOa,
              NeighbourNodeId, Rest);
        _ ->
            case tuncer:write(TunFd, Ipv6Packet) of
                ok ->
                    handle_cell(
                      NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd,
                      MyOa, NeighbourNodeId, Rest);
                {error, Reason} ->
                    ?error_log(Reason),
                    handle_cell(
                      NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd,
                      MyOa, NeighbourNodeId, Rest)
            end
    end;
%% forward ip packet to appropriate neighbour node
handle_cell(
  NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyOa,
  NeighbourNodeId,
  <<?NODE_CELL_IP_PACKET:8,
    DestOa0:16, DestOa1:16, DestOa2:16, DestOa3:16,
    DestOa4:16, DestOa5:16, DestOa6:16, DestOa7:16,
    Ipv6PacketSize:16, Ipv6Packet:Ipv6PacketSize/binary, Rest/binary>>) ->
    DestOa = {DestOa0, DestOa1, DestOa2, DestOa3,
              DestOa4, DestOa5, DestOa6, DestOa7},
    send_node_message(DestOa, NodeDb, RouteDb, Ipv6Packet),
    handle_cell(
      NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyOa,
      NeighbourNodeId, Rest);
%% send route entry to node_route_serv
handle_cell(
  NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyOa,
  NeighbourNodeId,
  <<?NODE_CELL_ROUTE_ENTRY:8,
    Oa0:16, Oa1:16, Oa2:16, Oa3:16, Oa4:16, Oa5:16, Oa6:16, Oa7:16,
    Pc:16, HopsSize:16, Hops:HopsSize/binary, PspSize:16, Psp:PspSize/binary,
    Rest/binary>>) ->
    Oa = {Oa0, Oa1, Oa2, Oa3, Oa4, Oa5, Oa6, Oa7},
    case decode_hops(Hops) of
        {ok, DecodedHops} ->
            Re = #route_entry{
              oa = Oa,
              node_id = NeighbourNodeId,
              path_cost = Pc,
              hops = [NeighbourNodeId|DecodedHops],
              psp = Psp
             },
            ok = node_route_serv:new_route_entry(NodeRouteServ, Re),
            handle_cell(
              NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyOa,
              NeighbourNodeId, Rest);
        {error, Reason} ->
            ?error_log(Reason),
            handle_cell(
              NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyOa,
              NeighbourNodeId, Rest)
    end;
%% reply to echo request
handle_cell(
  NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyOa,
  NeighbourNodeId,
  <<?NODE_CELL_ECHO_REQUEST:8, SeqNumber:16, UniqueId:16, Timestamp:32,
    Rest/binary>>) ->
    EchoReply =
        <<?NODE_CELL_ECHO_REPLY:8, SeqNumber:16, UniqueId:16, Timestamp:32>>,
    send_node_message(NeighbourNodeId, NodeDb, RouteDb, EchoReply),
    handle_cell(
      NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyOa,
      NeighbourNodeId, Rest);
%% send echo reply to path cost server
handle_cell(
  NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyOa,
  NeighbourNodeId,
  <<?NODE_CELL_ECHO_REPLY:8, SeqNumber:16, UniqueId:16, Timestamp:32,
    Rest/binary>>) ->
    EchoReply = #echo_reply{sequence_number = SeqNumber, unique_id = UniqueId,
                            timestamp = Timestamp},
    ok = node_path_cost_serv:echo_reply(NodePathCostServ, EchoReply),
    handle_cell(
      NodeDb, RouteDb, NodeRouteServ, NodePathCostServ, TunFd, MyOa,
      NeighbourNodeId, Rest);
%% throw away padding
handle_cell(
  _NodeDb, _RouteDb, _NodeRouteServ, _NodePathCostServ, _TunFd, _MyOa,
  _NeighbourNodeId, <<?NODE_CELL_PADDING:8, _/binary>>) ->
    ok;
%% no padding to throw away
handle_cell(
  _NodeDb, _RouteDb, _NodeRouteServ, _NodePathCostServ, _TunFd, _MyOa,
  _NeighbourNodeId, <<>>) ->
    ok;
%% unknown cell fragment
handle_cell(
  _NodeDb, _RouteDb, _NodeRouteServ, _NodePathCostServ, _TunFd, _MyOa,
  NeighbourNodeId, CellFragment) ->
    ?error_log({unknown_cell_fragment, NeighbourNodeId, CellFragment}).

send_node_message(DestOaOrNodeId, NodeDb, RouteDb, Data) ->
    case node_route:lookup_node_send_serv(NodeDb, RouteDb, DestOaOrNodeId) of
        {ok, NodeSendServ} ->
            ok = node_send_serv:send(NodeSendServ, {?MODULE, Data});
        {error, Reason} ->
            ?error_log(Reason),
            ok
    end.

encode_hops(Hops) ->
    ?l2b([<<NodeId:32>> || NodeId <- Hops]).

decode_hops(Hops) ->
    decode_hops(Hops, []).

decode_hops(<<>>, Acc) ->
    {ok, lists:reverse(Acc)};
decode_hops(<<NodeId:32, Rest/binary>>, Acc) ->
    decode_hops(Rest, [NodeId|Acc]);
decode_hops(Hops, Acc) ->
    {error, {invalid_hops, Hops, Acc}}.

%%%
%%% init
%%%

read_udp_receiver_config(S) ->
    NodeInstance =
        ?config([nodes, {'node-address', S#udp_receiver_state.my_na}]),
    read_udp_receiver_config(S, NodeInstance).

read_udp_receiver_config(S, []) ->
    S;
read_udp_receiver_config(S, [{'logging', Value}|Rest]) ->
    read_udp_receiver_config(S#udp_receiver_state{logging = Value}, Rest);
read_udp_receiver_config(S, [{'directory-server', Value}|Rest]) ->
    read_udp_receiver_config(
      S#udp_receiver_state{ds_ip_address_port = Value}, Rest);
read_udp_receiver_config(S, [{'overlay-addresses', [MyOa]}|Rest]) ->
    read_udp_receiver_config(S#udp_receiver_state{my_oa = MyOa}, Rest);
read_udp_receiver_config(_S, [{'overlay-addresses', _Oas}|_Rest]) ->
    throw(nyi);
read_udp_receiver_config(S, [_|Rest]) ->
    read_udp_receiver_config(S, Rest).

read_config(S) ->
    NodeInstance = ?config([nodes, {'node-address', S#state.my_na}]),
    read_config(S, NodeInstance).

read_config(S, []) ->
    S;
read_config(S, [{'logging', Value}|Rest]) ->
    read_config(S#state{logging = Value}, Rest);
read_config(S, [{'directory-server', Value}|Rest]) ->
    read_config(S#state{ds_ip_address_port = Value}, Rest);
read_config(S, [_|Rest]) ->
    read_config(S, Rest).
