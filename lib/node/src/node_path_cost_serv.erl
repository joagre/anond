-module(node_path_cost_serv).

%%% external exports
-export([start_link/2, stop/1, stop/2]).
-export([updated_peer_nas/2]).
-export([echo_reply/2]).

%%% internal exports
-export([init/3]).
-export([send_echo_requests/5]).

%%% include files
-include_lib("node/include/node.hrl").
-include_lib("node/include/node_path_cost.hrl").
-include_lib("node/include/node_route.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/log.hrl").

%%% constants
-define(PERCENT_NUDGE, 5).

%%% records

-record(state, {
          parent                            :: pid(),
          node_db                           :: node_db(),
          route_db                          :: route_db(),
          node_route_serv                   :: pid(),
          peer_nas = []                     :: [na()],
          path_costs = []                   :: [{{inet:port_number(),
                                                  inet:port_number()},
                                                 path_cost()}],
          unique_id = 0                     :: non_neg_integer(),
          %% anond.conf parameters
          mode                              :: common_config_json_serv:mode(),
          na                                :: na(),
          number_of_echo_requests           :: non_neg_integer(),
          acceptable_number_of_echo_replies :: non_neg_integer(),
          delay_between_echo_requests       :: timeout(),
          delay_between_measurements        :: timeout(),
          echo_reply_timeout                :: timeout()}).

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link(na(), supervisor:sup_ref()) -> {'ok', pid()}.

start_link(Na, NodeInstanceSup) ->
    Args = [self(), Na, NodeInstanceSup],
    Pid = proc_lib:spawn_link(?MODULE, init, Args),
    receive
	{Pid, started} ->
	    {ok, Pid}
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
%%% exported: updated_peer_nas
%%%

-spec updated_peer_nas(pid() | 'undefined', [na()]) -> 'ok'.

updated_peer_nas(undefined, _UpdatedPeerNas) ->
    ok;
updated_peer_nas(NodePathCostServ, UpdatedPeerNas) ->
    NodePathCostServ ! {updated_peer_nas, UpdatedPeerNas},
    ok.

%%%
%%% exported: echo_reply
%%%

-spec echo_reply(pid(), #echo_reply{}) -> 'ok'.

echo_reply(NodePathCostServ, EchoReply) ->
    NodePathCostServ ! EchoReply,
    ok.

%%%
%%% server loop
%%%

init(Parent, Na, NodeInstanceSup) ->
    process_flag(trap_exit, true),
    ok = config_json_serv:subscribe(),
    S = read_config(#state{parent = Parent, path_costs = ?NON_RANDOM_PATH_COSTS,
                           na = Na}),
    Parent ! {self(), started},
    {ok, NodeRouteServ} =
        node_instance_sup:lookup_child(NodeInstanceSup, node_route_serv),
    {ok, NodeRecvServ} =
        node_instance_sup:lookup_child(NodeInstanceSup, node_recv_serv),
    {ok, NodeDb, RouteDb} =
        node_route_serv:handshake(NodeRouteServ, {?MODULE, self()}),
    ok = node_recv_serv:handshake(NodeRecvServ, {?MODULE, self()}),
    self() ! measure,
    loop(S#state{node_db = NodeDb, route_db = RouteDb,
                 node_route_serv = NodeRouteServ}).

loop(#state{parent = Parent,
            node_db = NodeDb,
            route_db = RouteDb,
            node_route_serv = NodeRouteServ,
            peer_nas = PeerNas,
            path_costs = Pcs,
            unique_id = UniqueId,            
            mode = Mode,
            na = Na,
            number_of_echo_requests = NumberOfEchoRequests,
            acceptable_number_of_echo_replies = AcceptableNumberOfEchoReplies,
            delay_between_echo_requests = DelayBetweenEchoRequests,
            delay_between_measurements = DelayBetweenMeasurements,
            echo_reply_timeout = EchoReplyTimeout} = S) ->
    receive
        config_updated ->
            ?daemon_log("Configuration changed...", []),
            loop(read_config(S));
        {updated_peer_nas, UpdatedPeerNas} ->
            loop(S#state{peer_nas = UpdatedPeerNas});
        measure when PeerNas == [] ->
            timelib:start_timer(DelayBetweenMeasurements, measure),
            loop(S);
        measure ->
            PeerNa = hd(PeerNas),
            RotatedPeerNas = rotate_peer_nas(PeerNas),
            case Mode of
                normal ->
                    Pc = measure_path_cost(
                           NodeDb, RouteDb, UniqueId, Na, NumberOfEchoRequests,
                           AcceptableNumberOfEchoReplies,
                           DelayBetweenEchoRequests, EchoReplyTimeout, PeerNa),
                    ok = node_route_serv:update_path_cost(
                           NodeRouteServ, PeerNa, Pc),
                    timelib:start_timer(DelayBetweenMeasurements, measure),
                    loop(S#state{peer_nas = RotatedPeerNas,
                                 unique_id = UniqueId+1});
                %% see doc/small_simulation.jpg
                simulation ->
                    {_NaIpAddress, NaPort} = Na,
                    {_PeerNaIpAddress, PeerNaPort} = PeerNa,
                    {value, {_, StoredPc}} =
                        lists:keysearch({NaPort, PeerNaPort}, 1, Pcs),
                    Pc = nudge_path_cost(StoredPc, ?PERCENT_NUDGE),
                    ok = node_route_serv:update_path_cost(
                           NodeRouteServ, PeerNa, Pc),
                    timelib:start_timer(DelayBetweenMeasurements, measure),
                    loop(S#state{peer_nas = RotatedPeerNas})
            end;
	{From, stop} ->
	    From ! {self(), ok};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        %% ignore stale echo replies
        #echo_reply{} ->
            loop(S);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

rotate_peer_nas([PeerNa|Rest]) ->
    lists:reverse([PeerNa|lists:reverse(Rest)]).

measure_path_cost(NodeDb, RouteDb, UniqueId, Na, NumberOfEchoRequests,
                  AcceptableNumberOfEchoReplies, DelayBetweenEchoRequests,
                  EchoReplyTimeout, PeerNa) ->
    {ok, NodeSendServ} =
        node_route:lookup_node_send_serv(NodeDb, RouteDb, PeerNa),
    proc_lib:spawn(?MODULE, send_echo_requests,
                   [UniqueId, NumberOfEchoRequests, DelayBetweenEchoRequests,
                    PeerNa, NodeSendServ]),
    EchoReplyLatencies = wait_for_echo_replies(UniqueId, EchoReplyTimeout),
    case length(EchoReplyLatencies) of
        NumberOfEchoReplies
          when NumberOfEchoReplies > AcceptableNumberOfEchoReplies ->
            AverageEchoReplyLatency =
                lists:foldl(fun(EchoReplyLatency, Sum) ->
                                    EchoReplyLatency+Sum end,
                            0, EchoReplyLatencies)/NumberOfEchoReplies,
            trunc(AverageEchoReplyLatency);
        NumberOfEchoReplies ->
            {NaIpAddress, _NaPort} = Na,
            {PeerNaIpAddress, _PeerNaPort} = PeerNa,
            PacketLoss = trunc(NumberOfEchoReplies/NumberOfEchoRequests*100),
            ?daemon_log("Echo requests sent from ~s to ~s resulted in more "
                        "than ~w% packet loss",
                        [net_tools:string_address(NaIpAddress),
                         net_tools:string_address(PeerNaIpAddress),
                         PacketLoss]),
            -1
    end.

send_echo_requests(_UniqueId, 0, _DelayBetweenEchoRequests, _PeerNa,
                   _NodeSendServ) ->
    ok;
send_echo_requests(UniqueId, NumberOfEchoRequests, DelayBetweenEchoRequests,
                   PeerNa, NodeSendServ) ->
    EchoRequest = #echo_request{
      sequence_number = NumberOfEchoRequests,
      unique_id = UniqueId,
      timestamp = timelib:mk_timestamp()},
    ok = node_send_serv:send(NodeSendServ, EchoRequest),
    timer:sleep(DelayBetweenEchoRequests),
    send_echo_requests(UniqueId, NumberOfEchoRequests-1,
                       DelayBetweenEchoRequests, PeerNa, NodeSendServ).

wait_for_echo_replies(UniqueId, EchoReplyTimeout) ->
    wait_for_echo_replies(UniqueId, EchoReplyTimeout, []).

wait_for_echo_replies(UniqueId, EchoReplyTimeout, EchoReplyLatencies) ->
    receive
        #echo_reply{sequence_number = _SeqNumber, unique_id = UniqueId,
                    timestamp = Timestamp} ->
            NewEchoReplyLatency = timelib:mk_timestamp()-Timestamp,
            wait_for_echo_replies(UniqueId, EchoReplyTimeout,
                                  [NewEchoReplyLatency|EchoReplyLatencies]);
        %% ignore echo replies with old id
        #echo_reply{sequence_number = _SeqNumber, unique_id = _AnotherUniqueId,
                    timestamp = _Timestamp} ->
            wait_for_echo_replies(UniqueId, EchoReplyTimeout,
                                  EchoReplyLatencies)
    after EchoReplyTimeout ->
            EchoReplyLatencies
    end.

nudge_path_cost(-1, _Percent) ->
    -1;
nudge_path_cost(Pc, Percent) ->
    trunc(random:uniform(Percent)/100*Pc+Pc).

%%%
%%% init
%%%

read_config(S) ->
    Mode = ?config([mode]),
    NodeInstance = ?config([nodes, {'node-address', S#state.na}]),
    {value, {'path-cost', PathCost}} =
        lists:keysearch('path-cost', 1, NodeInstance),
    read_config(S#state{mode = Mode}, PathCost).

read_config(S, []) ->
    S;
read_config(S, [{'number-of-echo-requests', Value}|Rest]) ->
    read_config(S#state{number_of_echo_requests = Value}, Rest);
read_config(S, [{'acceptable-number-of-echo-replies', Value}|Rest]) ->
    read_config(S#state{acceptable_number_of_echo_replies = Value}, Rest);
read_config(S, [{'delay-between-echo-requests', Value}|Rest]) ->
    read_config(S#state{delay_between_echo_requests = Value}, Rest);
read_config(S, [{'delay-between-measurements', Value}|Rest]) ->
    read_config(S#state{delay_between_measurements = Value}, Rest);
read_config(S, [{'echo-reply-timeout', Value}|Rest]) ->
    read_config(S#state{echo_reply_timeout = Value}, Rest).
