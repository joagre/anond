-module(node_path_cost_serv).

%%% external exports
-export([start_link/2, stop/1, stop/2]).
-export([set_neighbour_node_ids/2]).
-export([echo_reply/2]).

%%% system exports
-export([system_continue/3, system_terminate/4, system_code_change/4,
         system_get_state/1, system_replace_state/2]).

%%% internal exports
-export([init/3]).
-export([send_echo_requests/5]).

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
          my_node_id = 0                    :: node_id(),
          neighbour_node_ids = []           :: [node_id()],
          unique_id = 0                     :: non_neg_integer(),
          %% anond.conf parameters
          my_na                             :: na(),
	  simulated_path_costs = []         :: [{node_id(), path_cost()}],
          logging                           :: boolean(),
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

stop(Pid) ->
    stop(Pid, 15000).

-spec stop(pid(), timeout()) -> 'ok'.

stop(Pid, Timeout) ->
    serv:call(Pid, stop, Timeout).

%%%
%%% exported: set_neighbour_node_ids
%%%

-spec set_neighbour_node_ids(pid() | 'undefined', [node_id()]) -> 'ok'.

set_neighbour_node_ids(undefined, _NewNeighbourNodeIds) ->
    ok;
set_neighbour_node_ids(NodePathCostServ, NewNeighbourNodeIds) ->
    NodePathCostServ ! {set_neighbour_node_ids, NewNeighbourNodeIds},
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
    S = read_config(#state{parent = Parent, my_na = MyNa}),
    Parent ! {self(), started},
    %% Note: The supervisor will not be available until all its children
    %% have been started, i.e. calls to node_instance_sup:lookup_child/2
    %% must be delayed until now
    {ok, NodeRouteServ} =
        node_instance_sup:lookup_child(NodeInstanceSup, node_route_serv),
    {ok, NodeRecvServ} =
        node_instance_sup:lookup_child(NodeInstanceSup, node_recv_serv),
    {ok, NodeDb, RouteDb} =
        node_route_serv:handshake(NodeRouteServ, {?MODULE, self()}),
    ok = node_recv_serv:handshake(NodeRecvServ, {?MODULE, self()}),
    self() ! measure,
    ok = log_serv:toggle_logging(self(), S#state.logging),
    loop(S#state{node_db = NodeDb, route_db = RouteDb,
                 node_route_serv = NodeRouteServ}).

loop(#state{parent = Parent,
            node_db = NodeDb,
            route_db = RouteDb,
            node_route_serv = NodeRouteServ,
            my_node_id = MyNodeId,
            neighbour_node_ids = NeighbourNodeIds,
            unique_id = UniqueId,
            my_na = MyNa,
	    simulated_path_costs = SimulatedPcs,
            logging = _Logging,
            number_of_echo_requests = NumberOfEchoRequests,
            acceptable_number_of_echo_replies = AcceptableNumberOfEchoReplies,
            delay_between_echo_requests = DelayBetweenEchoRequests,
            delay_between_measurements = DelayBetweenMeasurements,
            echo_reply_timeout = EchoReplyTimeout} = S) ->
    receive
        config_updated ->
            ?daemon_log("Node ~w (~s) starts to update its configuration",
                        [MyNodeId, net_tools:string_address(MyNa)]),
            loop(read_config(S));
        {set_neighbour_node_ids, NewNeighbourNodeIds} ->
            loop(S#state{neighbour_node_ids = NewNeighbourNodeIds});
        measure when NeighbourNodeIds == [] ->
            timelib:start_timer(DelayBetweenMeasurements, measure),
            loop(S);
        measure when MyNodeId == 0 ->
            NewMyNodeId = node_route_serv:get_my_node_id(NodeRouteServ),
            timelib:start_timer(DelayBetweenMeasurements, measure),
	    NewSimulatedPcs =
		read_simulated_path_costs(NewMyNodeId, ?config(['nodes'])),
            loop(S#state{my_node_id = NewMyNodeId,
			 simulated_path_costs = NewSimulatedPcs});
        measure ->
            NeighbourNodeId = hd(NeighbourNodeIds),
            RotatedNeighbourNodeIds =
                rotate_neighbour_node_ids(NeighbourNodeIds),
	    SimulatedPc = get_simulated_path_cost(SimulatedPcs, NeighbourNodeId),
            Pc = measure_path_cost(
                   NodeDb, RouteDb, UniqueId, MyNodeId, NumberOfEchoRequests,
                   AcceptableNumberOfEchoReplies, DelayBetweenEchoRequests,
                   EchoReplyTimeout, NeighbourNodeId, SimulatedPc),
            ok = node_route_serv:update_node_path_cost(
                   NodeRouteServ, NeighbourNodeId, Pc),
            timelib:start_timer(DelayBetweenMeasurements, measure),
            loop(S#state{neighbour_node_ids = RotatedNeighbourNodeIds,
                         unique_id = UniqueId+1});
	{From, stop} ->
	    From ! {self(), ok};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        %% ignore stale echo replies
        #echo_reply{} ->
            ?dbg_log(ignore_stale_echo_reply),
            loop(S);
        {system, From, Msg} ->
            sys:handle_system_msg(Msg, From, Parent, ?MODULE, [], S);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

system_continue(_Parent, _Debug, S) ->
    loop(S).

system_terminate(Reason, _Parent, _Debug, _S) ->
    exit(Reason).

system_code_change(S, _Module, _OldVsn, _Extra) ->
    {ok, S}.

system_get_state(S) ->
    {ok, S}.

system_replace_state(StateFun, S) ->
    NewS = StateFun(S),
    {ok, NewS, NewS}.

rotate_neighbour_node_ids([NeighbourNodeId|Rest]) ->
    lists:reverse([NeighbourNodeId|lists:reverse(Rest)]).

measure_path_cost(
  NodeDb, RouteDb, UniqueId, MyNodeId, NumberOfEchoRequests,
  AcceptableNumberOfEchoReplies, DelayBetweenEchoRequests, EchoReplyTimeout,
  NeighbourNodeId, SimulatedPc) ->
    {ok, NodeSendServ} =
        node_route:lookup_node_send_serv(NodeDb, RouteDb, NeighbourNodeId),
    StartTimestamp = timelib:mk_timestamp(),
    proc_lib:spawn(?MODULE, send_echo_requests,
                   [UniqueId, NumberOfEchoRequests, DelayBetweenEchoRequests,
                    NodeSendServ, StartTimestamp]),
    EchoReplyLatencies =
        wait_for_echo_replies(UniqueId, EchoReplyTimeout, StartTimestamp),
    case length(EchoReplyLatencies) of
        NumberOfEchoReplies
          when NumberOfEchoReplies > AcceptableNumberOfEchoReplies ->
            case SimulatedPc of
                not_simulated ->
                    AverageEchoReplyLatency =
                        lists:foldl(fun(EchoReplyLatency, Sum) ->
                                            EchoReplyLatency+Sum end,
                                    0, EchoReplyLatencies)/NumberOfEchoReplies,
                    truncate_path_cost(AverageEchoReplyLatency);
                _ ->
                    SimulatedPc
            end;
        NumberOfEchoReplies ->
            PacketLoss =
                100-trunc(NumberOfEchoReplies/NumberOfEchoRequests*100),
            ?daemon_log("Echo requests sent from ~w to ~w resulted in ~w% "
                        "packet loss",
                        [MyNodeId, NeighbourNodeId, PacketLoss]),
            ?NODE_UNREACHABLE
    end.

truncate_path_cost(Pc) ->
    case trunc(Pc) of
        TruncatedPc when TruncatedPc >= ?NODE_UNREACHABLE ->
            ?NODE_UNREACHABLE-1;
        TruncatedPc ->
            TruncatedPc
    end.

send_echo_requests(_UniqueId, 0, _DelayBetweenEchoRequests, _NodeSendServ,
                   _StartTimestamp) ->
    ok;
send_echo_requests(UniqueId, NumberOfEchoRequests, DelayBetweenEchoRequests,
                   NodeSendServ, StartTimestamp) ->
    EchoRequest = #echo_request{
      sequence_number = NumberOfEchoRequests,
      unique_id = UniqueId,
      timestamp = timelib:mk_timestamp()-StartTimestamp},
    ok = node_send_serv:send(NodeSendServ, {?MODULE, EchoRequest}),
    timer:sleep(DelayBetweenEchoRequests),
    send_echo_requests(UniqueId, NumberOfEchoRequests-1,
                       DelayBetweenEchoRequests, NodeSendServ, StartTimestamp).

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

%%%
%%% init
%%%

read_config(S) ->
    NodeInstancePath = [nodes, {'node-address', S#state.my_na}],
    try
        NodeInstance = ?config(NodeInstancePath),
        SimulatedPcs = read_simulated_path_costs(S#state.my_node_id,
                                                 ?config(['nodes'])),
        NewS = read_config(S#state{simulated_path_costs = SimulatedPcs},
                           NodeInstance),
        {value, {'path-cost', Pc}} =
            lists:keysearch('path-cost', 1, NodeInstance),
        read_config_path_cost(NewS, Pc)
    catch
        throw:{unknown_config_parameter, NodeInstancePath} ->
            S
    end.

read_simulated_path_costs(0, _Nodes) ->
    [];
read_simulated_path_costs(MyNodeId, Nodes) ->
    read_simulated_path_costs(MyNodeId, Nodes, []).

read_simulated_path_costs(_MyNodeId, [], Acc) ->
    Acc;
%% this node picked the following neighbour nodes
read_simulated_path_costs(
  MyNodeId,
  [[{'node-address', _Na},
    {simulation, [{'node-id', MyNodeId},
		  {neighbours, Neighbours}|_]}|_]|Rest], Acc) ->
    NeighbourNodePcs = [{NeighbourNodeId, Pc} ||	
			   [{'node-id', NeighbourNodeId},
			    {'path-cost', Pc}|_] <- Neighbours],
    read_simulated_path_costs(MyNodeId, Rest, merge(NeighbourNodePcs, Acc));
%% another node picked this node as neighbour
read_simulated_path_costs(
  MyNodeId,
  [[{'node-address', _Na},
    {simulation, [{'node-id', NeighbourNodeId},
		  {neighbours, Neighbours}|_]}|_]|Rest], Acc) ->
    NeighbourNodePcs = [{NeighbourNodeId, Pc} ||
			   [{'node-id', NodeId},
			    {'path-cost', Pc}|_] <- Neighbours,
			   NodeId == MyNodeId],
    read_simulated_path_costs(MyNodeId, Rest, merge(Acc, NeighbourNodePcs)).

merge([], NeighbourNodePcs) ->
    NeighbourNodePcs;
merge([{NeighbourNodeId, Pc}|Rest], NeighbourNodePcs) ->
    case lists:keymember(NeighbourNodeId, 1, NeighbourNodePcs) of
	true ->
	    merge(Rest, lists:keyreplace(NeighbourNodeId, 1, NeighbourNodePcs,
					 {NeighbourNodeId, Pc}));
	false ->
	    merge(Rest, [{NeighbourNodeId, Pc}|NeighbourNodePcs])
    end.

read_config(S, []) ->
    S;
read_config(S, [{'logging', Value}|Rest]) ->
    read_config(S#state{logging = Value}, Rest);
read_config(S, [_|Rest]) ->
    read_config(S, Rest).

read_config_path_cost(S, []) ->
    S;
read_config_path_cost(S, [{'number-of-echo-requests', Value}|Rest]) ->
    read_config_path_cost(S#state{number_of_echo_requests = Value}, Rest);
read_config_path_cost(S, [{'acceptable-number-of-echo-replies', Value}|Rest]) ->
    read_config_path_cost(
      S#state{acceptable_number_of_echo_replies = Value}, Rest);
read_config_path_cost(S, [{'delay-between-echo-requests', Value}|Rest]) ->
    read_config_path_cost(S#state{delay_between_echo_requests = Value}, Rest);
read_config_path_cost(S, [{'delay-between-measurements', Value}|Rest]) ->
    read_config_path_cost(S#state{delay_between_measurements = Value}, Rest);
read_config_path_cost(S, [{'echo-reply-timeout', Value}|Rest]) ->
    read_config_path_cost(S#state{echo_reply_timeout = Value}, Rest).

%%%
%%% measure
%%%

get_simulated_path_cost(SimulatedPcs, NeighbourNodeId) ->
    case lists:keysearch(NeighbourNodeId, 1, SimulatedPcs) of
	{value, {_, SimulatedPc}} ->
	    nudge_path_cost(SimulatedPc, ?PERCENT_NUDGE);
	false ->
	    not_simulated
    end.

nudge_path_cost(Pc, Percent) ->
    trunc(random:uniform(Percent)/100*Pc+Pc).
