-module(node_send_serv).

%%% external exports
-export([start_link/5, stop/1, stop/2]).
-export([send/2]).
-export([set_shared_key/2]).

%%% internal exports
-export([init/6]).

%%% include files
-include_lib("node/include/node.hrl").
-include_lib("node/include/node_path_cost.hrl").
-include_lib("node/include/node_recv_send.hrl").
-include_lib("node/include/node_route.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants
-define(NODE_KEEPALIVE_INTERVAL, {10, seconds}).

%%% records
-record(state, {
	  parent               :: pid(),
          my_node_id           :: node_id(),
          neighbour_na         :: na(),
          socket               :: gen_udp:socket(),
          shared_key           :: binary(),
          cell_buffer = []     :: [binary()],
          cell_size = 0        :: non_neg_integer(),
          %% anond.conf parameters
          my_na                :: na(),
          logging              :: boolean(),
          max_cell_size        :: non_neg_integer(),
          cell_sending_timeout :: non_neg_integer()}).

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link(gen_udp:socket(), node_id(), na(), na(), binary()) ->
                        {'ok', pid()}.

start_link(Socket, MyNodeId, MyNa, NeighbourNa, SharedKey) ->
    Args = [self(), Socket, MyNodeId, MyNa, NeighbourNa, SharedKey],
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
%%% exported: send
%%%

-spec send(pid(),
           {'node_recv_serv', binary()} |
           {'node_tun_serv', oa(), binary()} |
           {'node_route_serv', #route_entry{}} |
           {'node_path_cost_serv', #echo_request{}}) ->
                  'ok'.

send(NodeSendServ, Data) ->
    NodeSendServ ! Data,
    ok.

%%%
%%% exported: set_shared_key
%%%

-spec set_shared_key(pid(), binary()) -> 'ok'.

set_shared_key(NodeSendServ, NewSharedKey) ->
    NodeSendServ ! {set_shared_key, NewSharedKey},
    ok.

%%%
%%% server loop
%%%

init(Parent, Socket, MyNodeId, MyNa, NeighbourNa, SharedKey) ->
    process_flag(trap_exit, true),
    ok = config_json_serv:subscribe(),
    S = read_config(#state{parent = Parent, my_node_id = MyNodeId,
                           neighbour_na = NeighbourNa, my_na = MyNa,
                           socket = Socket, shared_key = SharedKey}),
    ok = log_serv:toggle_logging(self(), S#state.logging),
    timelib:start_timer(?NODE_KEEPALIVE_INTERVAL, node_keepalive),
    Parent ! {self(), started},
    loop(S).

loop(#state{parent = Parent,
            my_node_id = MyNodeId,
            neighbour_na = NeighbourNa,
            socket = Socket,
            shared_key = SharedKey,
            cell_buffer = CellBuffer,
            cell_size = CellSize,
            my_na = MyNa,
            logging = _Logging,
            max_cell_size = MaxCellSize,
            cell_sending_timeout = CellSendingTimeout} = S) ->
    receive
        config_updated ->
            ?daemon_log("Node ~w (~s) starts to update its configuration",
                        [MyNodeId, net_tools:string_address(MyNa)]),
            loop(read_config(S));
        node_keepalive ->
            NodeKeepalive = <<?NODE_KEEPALIVE:8>>,
            NodeKeepaliveSize = size(NodeKeepalive),
            if
                CellSize+NodeKeepaliveSize > MaxCellSize ->
                    send_cell(MyNodeId, NeighbourNa, Socket, SharedKey,
                              CellBuffer, CellSize, MaxCellSize),
                    timelib:start_timer(?NODE_KEEPALIVE_INTERVAL, node_keepalive),
                    loop(S#state{cell_buffer = [NodeKeepalive],
                                 cell_size = NodeKeepaliveSize});
                true ->
                    timelib:start_timer(?NODE_KEEPALIVE_INTERVAL, node_keepalive),
                    loop(S#state{cell_buffer = [NodeKeepalive|CellBuffer],
                                 cell_size = CellSize+NodeKeepaliveSize})
            end;
        %% handle ip packets and echo replies from node_recv_serv
        {node_recv_serv, <<Type:8, _/binary>> = Data}
          when Type == ?NODE_CELL_IP_PACKET orelse
               Type == ?NODE_CELL_ECHO_REPLY ->
            DataSize = size(Data),
            if
                CellSize+DataSize > MaxCellSize ->
                    send_cell(MyNodeId, NeighbourNa, Socket, SharedKey,
                              CellBuffer, CellSize, MaxCellSize),
                    loop(S#state{cell_buffer = [Data],
                                 cell_size = DataSize});
                true ->
                    loop(S#state{cell_buffer = [Data|CellBuffer],
                                 cell_size = CellSize+DataSize})
            end;
        %% handle ipv6 packet from node_tun_serv
        {node_tun_serv, {DestOa0, DestOa1, DestOa2, DestOa3,
                         DestOa4, DestOa5, DestOa6, DestOa7}, Ipv6Packet} ->
            Ipv6PacketSize = size(Ipv6Packet),
            IpPacket =
                <<?NODE_CELL_IP_PACKET:8,
                  DestOa0:16, DestOa1:16, DestOa2:16, DestOa3:16,
                  DestOa4:16, DestOa5:16, DestOa6:16, DestOa7:16,
                  Ipv6PacketSize:16,
                  Ipv6Packet/binary>>,
            IpPacketSize = size(IpPacket),
            if
                CellSize+IpPacketSize > MaxCellSize ->
                    send_cell(MyNodeId, NeighbourNa, Socket, SharedKey,
                              CellBuffer, CellSize, MaxCellSize),
                    loop(S#state{cell_buffer = [IpPacket],
                                 cell_size = IpPacketSize});
                true ->
                    loop(S#state{cell_buffer = [IpPacket|CellBuffer],
                                 cell_size = CellSize+IpPacketSize})
            end;
        %% handle route entry from node_route_serv
        {node_route_serv,
         #route_entry{oa = {Oa0, Oa1, Oa2, Oa3, Oa4, Oa5, Oa6, Oa7},
                      path_cost = Pc, hops = Hops, psp = Psp} = _Re} ->
            EncodedHops = ?l2b([<<NodeId:32>> || NodeId <- Hops]),
            HopsSize = size(EncodedHops),
            PspSize = size(Psp),
            RouteEntry =
                <<?NODE_CELL_ROUTE_ENTRY:8,
                  Oa0:16, Oa1:16, Oa2:16, Oa3:16,
                  Oa4:16, Oa5:16, Oa6:16, Oa7:16,
                  Pc:16,
                  HopsSize:16,
                  EncodedHops/binary,
                  PspSize:16,
                  Psp/binary>>,
            RouteEntrySize = size(RouteEntry),
            if
                CellSize+RouteEntrySize > MaxCellSize ->
                    send_cell(MyNodeId, NeighbourNa, Socket, SharedKey,
                              CellBuffer, CellSize, MaxCellSize),
                    loop(S#state{cell_buffer = [RouteEntry],
                                 cell_size = RouteEntrySize});
                true ->
                    loop(S#state{cell_buffer = [RouteEntry|CellBuffer],
                                 cell_size = CellSize+RouteEntrySize})
            end;
        %% handle echo request from node_path_cost_serv
        {node_path_cost_serv,
         #echo_request{sequence_number = SeqNumber, unique_id = UniqueId,
                       timestamp = Timestamp}} ->
            EchoRequest =
                <<?NODE_CELL_ECHO_REQUEST:8, SeqNumber:16, UniqueId:16,
                  Timestamp:32>>,
            EchoRequestSize = size(EchoRequest),
            %% note: echo requests are always sent without delay
            case CellSize+EchoRequestSize of
                FinalCellSize when FinalCellSize > MaxCellSize ->
                    send_cell(MyNodeId, NeighbourNa, Socket, SharedKey,
                              EchoRequest, EchoRequestSize, MaxCellSize),
                    loop(S);
                FinalCellSize ->
                    send_cell(MyNodeId, NeighbourNa, Socket, SharedKey,
                              [EchoRequest|CellBuffer], FinalCellSize,
                              MaxCellSize),
                    loop(S#state{cell_buffer = [], cell_size = 0})
            end;
        {set_shared_key, NewSharedKey} ->
            loop(S#state{shared_key = NewSharedKey});
	{From, stop} ->
	    From ! {self(), ok};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    after
        CellSendingTimeout ->
            if
                CellSize == 0 ->
                    loop(S);
                true ->
                    send_cell(MyNodeId, NeighbourNa, Socket, SharedKey,
                              CellBuffer, CellSize, MaxCellSize),
                    loop(S#state{cell_buffer = [], cell_size = 0})
            end
    end.

send_cell(MyNodeId, {NeighbourIpAddress, NeighbourPort}, Socket, SharedKey,
          CellBuffer, CellSize, MaxCellSize) ->
    case MaxCellSize-CellSize of
        0 ->
            FinalCellBuffer = ?l2b(CellBuffer);
        PaddingSize when PaddingSize == 1 ->
            FinalCellBuffer = ?l2b([CellBuffer, <<?NODE_CELL_PADDING:8>>]);
        PaddingSize ->
            FinalCellBuffer =
                ?l2b([CellBuffer, <<?NODE_CELL_PADDING:8>>,
                      salt:crypto_random_bytes(PaddingSize-1)])
    end,
    Nonce = salt:crypto_random_bytes(24),
    case catch salt:crypto_stream_xor(FinalCellBuffer, Nonce, SharedKey) of
        {'EXIT', Reason} ->
            ?error_log(Reason);
        EncryptedCell ->
            Message = <<MyNodeId:32, Nonce/binary, EncryptedCell/binary>>,
            case gen_udp:send(Socket, NeighbourIpAddress, NeighbourPort,
                              Message) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?error_log(Reason)
            end
    end.

%%%
%%% init
%%%

read_config(S) ->
    NodeInstance = ?config([nodes, {'node-address', S#state.my_na}]),
    read_config(S, NodeInstance).

read_config(S, []) ->
    S;
read_config(S, [{'logging', Value}|Rest]) ->
    read_config(S#state{logging = Value}, Rest);
read_config(S, [{'max-cell-size', Value}|Rest]) ->
    read_config(S#state{max_cell_size = Value}, Rest);
read_config(S, [{'cell-sending-timeout', Value}|Rest]) ->
    read_config(S#state{cell_sending_timeout = Value}, Rest);
read_config(S, [_|Rest]) ->
    read_config(S, Rest).
