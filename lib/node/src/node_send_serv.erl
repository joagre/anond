-module(node_send_serv).

%%% external exports
-export([start_link/4, stop/1, stop/2]).
-export([send/2]).

%%% internal exports
-export([init/5]).

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
-record(state, {
	  parent               :: pid(),
          peer_na              :: na(),
          socket               :: gen_udp:socket(),
          cell_buffer = []     :: [binary()],
          cell_size = 0        :: non_neg_integer(),
          %% anond.conf parameters
          my_na                :: na(),
          max_cell_size        :: non_neg_integer(),
          cell_sending_timeout :: non_neg_integer()}).

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link(na(), na(), supervisor:sup_ref(), supervisor:sup_ref()) ->
                        {'ok', pid()}.

start_link(MyNa, PeerNa, NodeRouteServ, NodeRecvServ) ->
    Args = [self(), MyNa, PeerNa, NodeRouteServ, NodeRecvServ],
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
           {'node_recv_serv', binary()} |
           {'node_tun_serv', oa(), binary()} |
           {'node_route_serv', #route_entry{}} |
           {'node_path_cost_serv', #echo_request{}}) ->
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

init(Parent, MyNa, PeerNa, NodeRouteServ, NodeRecvServ) ->
    process_flag(trap_exit, true),
    S = read_config(#state{parent = Parent, my_na = MyNa, peer_na = PeerNa}),
    ok = config_json_serv:subscribe(),
    ok = node_route_serv:handshake(NodeRouteServ, {?MODULE, MyNa, self()}),
    {ok, Socket} = node_recv_serv:handshake(NodeRecvServ, ?MODULE),
    Parent ! {self(), started},
    loop(S#state{socket = Socket}).

loop(#state{parent = Parent,
            peer_na = PeerNa,
            socket = Socket,
            cell_buffer = CellBuffer,
            cell_size = CellSize,
            my_na = _MyNa,
            max_cell_size = MaxCellSize,
            cell_sending_timeout = CellSendingTimeout} = S) ->
    receive
        config_updated ->
            ?daemon_log("Configuration changed...", []),
            loop(read_config(S));
        %% handle ip packets and echo replies from node_recv_serv
        {node_recv_serv, <<Type:8, _/binary>> = Data}
          when Type == ?IP_PACKET orelse Type == ?ECHO_REPLY ->
            DataSize = size(Data),
            if
                CellSize+DataSize > MaxCellSize ->
                    send_cell(PeerNa, Socket, CellBuffer, CellSize,
                              MaxCellSize),
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
            %% note: this means that the tun device mtu should be MaxCellSize-19
            IpPacket =
                <<?IP_PACKET:8,
                  DestOa0:16, DestOa1:16, DestOa2:16, DestOa3:16,
                  DestOa4:16, DestOa5:16, DestOa6:16, DestOa7:16,
                  Ipv6PacketSize:16,
                  Ipv6Packet/binary>>,
            IpPacketSize = size(IpPacket),
            if
                CellSize+IpPacketSize > MaxCellSize ->
                    send_cell(PeerNa, Socket, CellBuffer, CellSize,
                              MaxCellSize),
                    loop(S#state{cell_buffer = [IpPacket],
                                 cell_size = IpPacketSize});
                true ->
                    loop(S#state{cell_buffer = [IpPacket|CellBuffer],
                                 cell_size = CellSize+IpPacketSize})
            end;
        %% handle route entry from node_route_serv
        {node_route_serv,
         #route_entry{oa = {Oa0, Oa1, Oa2, Oa3, Oa4, Oa5, Oa6, Oa7},
                      path_cost = Pc, hops = Hops, psp = Psp}} ->
            EncodedHops =
                ?l2b([<<Na0:8, Na1:8, Na2:8, Na3:8, Port:16>> ||
                         {{Na0, Na1, Na2, Na3}, Port} <- Hops]),
            HopsSize = size(EncodedHops),
            PspSize = size(Psp),
            RouteEntry =
                <<?ROUTE_ENTRY:8,
                  Oa0:16, Oa1:16, Oa2:16, Oa3:16,
                  Oa4:16, Oa5:16, Oa6:16, Oa7:16,
                  Pc:16/signed-integer,
                  HopsSize:16,
                  EncodedHops/binary,
                  PspSize:16,
                  Psp/binary>>,
            RouteEntrySize = size(RouteEntry),
            if
                CellSize+RouteEntrySize > MaxCellSize ->
                    send_cell(PeerNa, Socket, CellBuffer, CellSize,
                              MaxCellSize),
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
                <<?ECHO_REQUEST:8, SeqNumber:16, UniqueId:16, Timestamp:32>>,
            EchoRequestSize = size(EchoRequest),
            %% note: echo requests are always sent without delay
            case CellSize+EchoRequestSize of
                FinalCellSize when FinalCellSize > MaxCellSize ->
                    send_cell(PeerNa, Socket, EchoRequest, EchoRequestSize,
                              MaxCellSize),
                    loop(S);
                FinalCellSize ->
                    send_cell(PeerNa, Socket, [EchoRequest|CellBuffer],
                              FinalCellSize, MaxCellSize),
                    loop(S#state{cell_buffer = [], cell_size = 0})
            end;
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
                    send_cell(PeerNa, Socket, CellBuffer, CellSize,
                              MaxCellSize),
                    loop(S#state{cell_buffer = [], cell_size = 0})
            end
    end.

send_cell({PeerIpAddress, PeerPort}, Socket, CellBuffer, CellSize,
          MaxCellSize) ->
    case MaxCellSize-CellSize of
        0 ->
            FinalCellBuffer = CellBuffer;
        PaddingSize when PaddingSize == 1 ->
            FinalCellBuffer = [CellBuffer, <<?PADDING:8>>];
        PaddingSize ->
            FinalCellBuffer =
                [CellBuffer, <<?PADDING:8>>,
                 salt:crypto_random_bytes(PaddingSize-1)]
    end,
    case gen_udp:send(Socket, PeerIpAddress, PeerPort, FinalCellBuffer) of
        ok ->
            ok;
        {error, Reason} ->
            ?daemon_log("Sending data to ~s:~w failed: ~s",
                        [net_tools:string_address(PeerIpAddress), PeerPort,
                         inet:format_error(Reason)])
    end.

%%%
%%% init
%%%

read_config(S) ->
    NodeInstance = ?config([nodes, {'node-address', S#state.my_na}]),
    read_config(S, NodeInstance).

read_config(S, []) ->
    S;
read_config(S, [{'max-cell-size', Value}|Rest]) ->
    read_config(S#state{max_cell_size = Value}, Rest);
read_config(S, [{'cell-sending-timeout', Value}|Rest]) ->
    read_config(S#state{cell_sending_timeout = Value}, Rest);
read_config(S, [_|Rest]) ->
    read_config(S, Rest).
