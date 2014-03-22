-module(node_path_cost_serv).

%%% external exports
-export([start_link/2, stop/1, stop/2]).
-export([updated_neighbour_nas/2]).
-export([echo_reply/2]).

%%% internal exports
-export([init/3]).
-export([send_echo_requests/6]).

%%% include files
-include_lib("node/include/node.hrl").
-include_lib("node/include/node_path_cost.hrl").
-include_lib("node/include/node_route.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants
-define(PERCENT_NUDGE, 20).

%%% records

-record(state, {
          parent                            :: pid(),
          node_db                           :: node_db(),
          route_db                          :: route_db(),
          node_route_serv                   :: pid(),
          neighbour_nas = []                :: [na()],
          path_costs = []                   :: [{{inet:port_number(),
                                                  inet:port_number()},
                                                 path_cost()}],
          unique_id = 0                     :: non_neg_integer(),
          %% anond.conf parameters
          mode                              :: common_config_json_serv:mode(),
          my_na                             :: na(),
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

start_link(MyNa, NodeInstanceSup) ->
    Args = [self(), MyNa, NodeInstanceSup],
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
%%% exported: updated_neighbour_nas
%%%

-spec updated_neighbour_nas(pid() | 'undefined', [na()]) -> 'ok'.

updated_neighbour_nas(undefined, _UpdatedNeighbourNas) ->
    ok;
updated_neighbour_nas(NodePathCostServ, UpdatedNeighbourNas) ->
    NodePathCostServ ! {updated_neighbour_nas, UpdatedNeighbourNas},
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

init(Parent, MyNa, NodeInstanceSup) ->
    process_flag(trap_exit, true),
    ok = config_json_serv:subscribe(),
    S = read_config(#state{parent = Parent, path_costs = ?NON_RANDOM_PATH_COSTS,
                           my_na = MyNa}),
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
            neighbour_nas = NeighbourNas,
            path_costs = Pcs,
            unique_id = UniqueId,
            mode = Mode,
            my_na = MyNa,
            number_of_echo_requests = NumberOfEchoRequests,
            acceptable_number_of_echo_replies = AcceptableNumberOfEchoReplies,
            delay_between_echo_requests = DelayBetweenEchoRequests,
            delay_between_measurements = DelayBetweenMeasurements,
            echo_reply_timeout = EchoReplyTimeout} = S) ->
    receive
        config_updated ->
            ?daemon_log("Configuration changed...", []),
            loop(read_config(S));
        {updated_neighbour_nas, UpdatedNeighbourNas} ->
            loop(S#state{neighbour_nas = UpdatedNeighbourNas});
        measure when NeighbourNas == [] ->
            timelib:start_timer(DelayBetweenMeasurements, measure),
            loop(S);
        measure ->
            NeighbourNa = hd(NeighbourNas),
            RotatedNeighbourNas = rotate_neighbour_nas(NeighbourNas),
            case Mode of
                normal ->
                    Pc = measure_path_cost(
                           NodeDb, RouteDb, UniqueId, MyNa,
                           NumberOfEchoRequests, AcceptableNumberOfEchoReplies,
                           DelayBetweenEchoRequests, EchoReplyTimeout,
                           NeighbourNa),
                    ok = node_route_serv:update_path_cost(
                           NodeRouteServ, NeighbourNa, Pc),
                    timelib:start_timer(DelayBetweenMeasurements, measure),
                    loop(S#state{neighbour_nas = RotatedNeighbourNas,
                                 unique_id = UniqueId+1});
                %% see doc/small_simulation.jpg
                simulation ->
                    {_MyIpAddress, MyPort} = MyNa,
                    {_NeighbourIpAddress, NeighbourPort} = NeighbourNa,
                    {value, {_, StoredPc}} =
                        lists:keysearch({MyPort, NeighbourPort}, 1, Pcs),
                    Pc = nudge_path_cost(StoredPc, ?PERCENT_NUDGE),
                    ok = node_route_serv:update_path_cost(
                           NodeRouteServ, NeighbourNa, Pc),
                    timelib:start_timer(DelayBetweenMeasurements, measure),
                    loop(S#state{neighbour_nas = RotatedNeighbourNas})
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

rotate_neighbour_nas([NeighbourNa|Rest]) ->
    lists:reverse([NeighbourNa|lists:reverse(Rest)]).

measure_path_cost(
  NodeDb, RouteDb, UniqueId, MyNa, NumberOfEchoRequests,
  AcceptableNumberOfEchoReplies, DelayBetweenEchoRequests, EchoReplyTimeout,
  NeighbourNa) ->
    {ok, NodeSendServ} =
        node_route:lookup_node_send_serv(NodeDb, RouteDb, NeighbourNa),
    StartTimestamp = timelib:mk_timestamp(),
    proc_lib:spawn(?MODULE, send_echo_requests,
                   [UniqueId, NumberOfEchoRequests, DelayBetweenEchoRequests,
                    NeighbourNa, NodeSendServ, StartTimestamp]),
    EchoReplyLatencies =
        wait_for_echo_replies(UniqueId, EchoReplyTimeout, StartTimestamp),
    case length(EchoReplyLatencies) of
        NumberOfEchoReplies
          when NumberOfEchoReplies > AcceptableNumberOfEchoReplies ->
            AverageEchoReplyLatency =
                lists:foldl(fun(EchoReplyLatency, Sum) ->
                                    EchoReplyLatency+Sum end,
                            0, EchoReplyLatencies)/NumberOfEchoReplies,
            trunc(AverageEchoReplyLatency);
        NumberOfEchoReplies ->
            {MyIpAddress, _MyPort} = MyNa,
            {NeighbourIpAddress, _NeighbourPort} = NeighbourNa,
            PacketLoss = trunc(NumberOfEchoReplies/NumberOfEchoRequests*100),
            ?daemon_log("Echo requests sent from ~s to ~s resulted in more "
                        "than ~w% packet loss",
                        [net_tools:string_address(MyIpAddress),
                         net_tools:string_address(NeighbourIpAddress),
                         PacketLoss]),
            -1
    end.

send_echo_requests(_UniqueId, 0, _DelayBetweenEchoRequests, _NeighbourNa,
                   _NodeSendServ, _StartTimestamp) ->
    ok;
send_echo_requests(UniqueId, NumberOfEchoRequests, DelayBetweenEchoRequests,
                   NeighbourNa, NodeSendServ, StartTimestamp) ->
    EchoRequest = #echo_request{
      sequence_number = NumberOfEchoRequests,
      unique_id = UniqueId,
      timestamp = timelib:mk_timestamp()-StartTimestamp},
    ok = node_send_serv:send(NodeSendServ, {?MODULE, EchoRequest}),
    timer:sleep(DelayBetweenEchoRequests),
    send_echo_requests(UniqueId, NumberOfEchoRequests-1,
                       DelayBetweenEchoRequests, NeighbourNa, NodeSendServ,
                       StartTimestamp).

wait_for_echo_replies(UniqueId, EchoReplyTimeout, StartTimestamp) ->
    wait_for_echo_replies(UniqueId, EchoReplyTimeout, StartTimestamp, []).

wait_for_echo_replies(UniqueId, EchoReplyTimeout, StartTimestamp,
                      EchoReplyLatencies) ->
    receive
        #echo_reply{sequence_number = _SeqNumber, unique_id = UniqueId,
                    timestamp = Timestamp} ->
            NewEchoReplyLatency =
                timelib:mk_timestamp()-(StartTimestamp+Timestamp),
            wait_for_echo_replies(UniqueId, EchoReplyTimeout, StartTimestamp,
                                  [NewEchoReplyLatency|EchoReplyLatencies]);
        %% ignore echo replies with old id
        #echo_reply{sequence_number = _SeqNumber, unique_id = _AnotherUniqueId,
                    timestamp = _Timestamp} ->
            wait_for_echo_replies(UniqueId, EchoReplyTimeout, StartTimestamp,
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
    NodeInstance = ?config([nodes, {'node-address', S#state.my_na}]),
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
