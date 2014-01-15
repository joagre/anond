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
          parent :: pid(),
          na :: na(),
          receiver :: pid(),
          socket   :: gen_udp:socket()
         }).

-record(receiver_state, {
          na                  :: na(),
          oa                  :: oa(),
          node_db             :: node_db(),
          route_db            :: route_db(),
          node_route_serv     :: pid(),
          node_path_cost_serv :: pid(),
          tun_fd              :: node_tun_serv:tun_fd(),
          socket              :: gen_udp:socket()
	 }).

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link(na(), supervisor:sup_ref()) ->
                        {'ok', pid()} |
                        {'error', {'udp_failure', inet:posix()}}.

start_link(Na, NodeInstanceSup) ->
    Args = [self(), Na, NodeInstanceSup],
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

init(Parent, Na, NodeInstanceSup) ->
    process_flag(trap_exit, true),
    {IpAddress, Port} = Na,
    Options = [binary, {ip, IpAddress}, {active, false}],
    case gen_udp:open(Port, Options) of
        {ok, Socket} ->
            Parent ! {self(), started},
            {ok, NodeRouteServ} =
                node_instance_sup:lookup_child(NodeInstanceSup,
                                               node_route_serv),
            {ok, NodeDb, RouteDb} =
                node_route_serv:handshake(NodeRouteServ, ?MODULE),            
            S = read_config(#receiver_state{
                               na = Na, node_db = NodeDb, route_db = RouteDb,
                               node_route_serv = NodeRouteServ,
                               socket = Socket}),
            Receiver = proc_lib:spawn_link(?MODULE, receiver, [S]),
            loop(#serv_state{parent = Parent, na = Na, receiver = Receiver,
                             socket = Socket});
        {error, Reason} ->
            Parent ! {self(), {udp_failure, Reason}}
    end.

loop(#serv_state{parent = Parent,
                 na = {NaIpAddress, NaPort},
                 receiver = Receiver,
                 socket = Socket} = S) ->
    receive
        {From, {handshake, node_send_serv}} ->
            From ! {self(), {ok, Socket}},
            loop(S);
        {handshake, {node_tun_serv, TunFd}} ->
            Receiver ! {node_tun_serv, TunFd},
            ok = gen_udp:send(Socket, NaIpAddress, NaPort,
                              <<?MESSAGE_ARRIVED:8>>),
            loop(S);
        {handshake, {node_path_cost_serv, NodePathCostServ}} ->
            Receiver ! {node_path_cost_serv, NodePathCostServ},
            ok = gen_udp:send(Socket, NaIpAddress, NaPort,
                              <<?MESSAGE_ARRIVED:8>>),
            loop(S);
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

receiver(#receiver_state{na = {NaIpAddress, NaPort},
                         oa = {Oa0, Oa1, Oa2, Oa3, Oa4, Oa5, Oa6, Oa7},
                         node_db = NodeDb,
                         route_db = RouteDb,
                         node_route_serv = NodeRouteServ,
                         node_path_cost_serv = NodePathCostServ,
                         tun_fd = TunFd,
                         socket = Socket} = S) ->
    case gen_udp:recv(Socket, 0) of
        {ok, {NaIpAddress, NaPort, <<?MESSAGE_ARRIVED:8>>}} ->
            receive
                {node_path_cost_serv, NewNodePathCostServ} ->
                    receiver(S#receiver_state{
                               node_path_cost_serv = NewNodePathCostServ})
            end;
        {ok, {NaIpAddress, NaPort, <<?MESSAGE_ARRIVED:8>>}} ->
            receive
                {node_tun_serv, TunFd} ->
                    receiver(S#receiver_state{tun_fd = TunFd})
            end;
        %% send ip packet to tun device
        {ok, {_PeerIpAddress, _PeerPort,
              <<?IP_PACKET:4,
                Oa0:16, Oa1:16, Oa2:16, Oa3:16, Oa4:16, Oa5:16, Oa6:16, Oa7:16,
                Length:12,
                IpPacket/binary>>}} when size(IpPacket) == Length ->
            case TunFd of
                undefined ->
                    receiver(S);
                _ ->
                    case tuncer:write(TunFd, IpPacket) of
                        ok ->
                            receiver(S);
                        {error, Reason} ->
                            ?daemon_log("Can not write to tun device: ~s",
                                        [inet:format_error(Reason)]),
                            receiver(S)
                    end
            end;
        %% forward ip packet to appropriate peer node 
        {ok, {_PeerIpAddress, _PeerPort,
              <<?IP_PACKET:4,
                A0:16, A1:16, A2:16, A3:16, A4:16, A5:16, A6:16, A7:16,
                Length:12,
                IpPacket/binary>> = Cell}} when size(IpPacket) == Length ->
            Oa = {A0, A1, A2, A3, A4, A5, A6, A7},
            send(Oa, NodeDb, RouteDb, Cell),
            receiver(S);
        %% send route entry to node_route_serv
        {ok, {PeerIpAddress, PeerPort,
              <<?ROUTE_ENTRY:4,
                A0:16, A1:16, A2:16, A3:16, A4:16, A5:16, A6:16, A7:16,
                Pc:16,
                NumberOfHops:4,
                Rest/binary>>}} ->
            Oa = {A0, A1, A2, A3, A4, A5, A6, A7},
            case parse_hops(Rest, NumberOfHops) of
                {ok, Hops, <<PspSize:16, Psp/binary>>}
                  when size(Psp) == PspSize ->
                    Re = #route_entry{
                      oa = Oa, na = {PeerIpAddress, PeerPort}, path_cost = Pc,
                      hops = Hops, psp = Psp
                     },
                    ok = node_route_serv:route_entry(NodeRouteServ, Re),
                    receiver(S);
                {ok, Hops, Psp} ->
                    ?dbg_log({invalid_psp, Hops, Psp}),
                    receiver(S);
                {error, Reason} ->
                    ?dbg_log(Reason),
                    receiver(S)
            end;
        %% reply to echo request
        {ok, {PeerIpAddress, PeerPort,
              <<?ECHO_REQUEST:4, SeqNumber:12, UniqueId:16, Timestamp:32>>}} ->
            Cell = <<?ECHO_REPLY:4, SeqNumber:12, UniqueId:16, Timestamp:32>>,
            send({PeerIpAddress, PeerPort}, NodeDb, RouteDb, Cell),
            receiver(S);
        %% send echo reply to path cost server
        {ok, {_PeerIpAddress, _PeerPort,
              <<?ECHO_REPLY:4, SeqNumber:12, UniqueId:16, Timestamp:32>>}} ->
            EchoReply =
                #echo_reply{sequence_number = SeqNumber, unique_id = UniqueId,
                            timestamp = Timestamp},
            ok = node_path_cost_serv:echo_reply(NodePathCostServ, EchoReply),
            receiver(S);
        {ok, {IpAddress, Port, Cell}} ->
            ?daemon_log("An unknown packet cell received from ~s:~w: ~s",
                        [net_tools:string_address(IpAddress), Port, Cell]),
            receiver(S);
        {error, Reason} ->
            ?daemon_log("UDP failure on ~s:~w: ~s",
                        [net_tools:string_address(NaIpAddress), NaPort,
                         inet:format_error(Reason)]),
            receiver(S)
    end.

send(PeerOaOrNa, NodeDb, RouteDb, Cell) ->
    case node_route:lookup_node_send_serv(NodeDb, RouteDb, PeerOaOrNa) of
        {ok, NodeSendServ} ->
            ok = node_send_serv:send(NodeSendServ, Cell);
        {error, _Reason} ->
            ok
    end.

parse_hops(Rest, NumberOfHops) ->
    parse_hops(Rest, NumberOfHops, []).

parse_hops(Rest, 0, Acc) ->
    {ok, lists:reverse(Acc), Rest};
parse_hops(<<Na0:8, Na1:8, Na2:8, Na3:8, Port:16, Rest/binary>>, NumberOfHops,
           Acc) ->
    parse_hops(Rest, NumberOfHops-1, [{{Na0, Na1, Na2, Na3}, Port}|Acc]);
parse_hops(Rest, NumberOfHops, Acc) ->
    {error, {invalid_hops, Rest, NumberOfHops, Acc}}.

%%%
%%% init
%%%

read_config(S) ->
    NodeInstance = ?config([nodes, {'node-address', S#receiver_state.na}]),
    read_config(S, NodeInstance).

read_config(S, []) ->
    S;
read_config(S, [{'overlay-addresses', [Oa]}|Rest]) ->
    read_config(S#receiver_state{oa = Oa}, Rest);
read_config(_S, [{'overlay-addresses', _Oas}|_Rest]) ->
    throw(nyi);
read_config(S, [_|Rest]) ->
    read_config(S, Rest).
