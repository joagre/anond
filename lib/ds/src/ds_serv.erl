-module(ds_serv).

%%% external exports
-export([start_link/0, stop/0, stop/1]).
-export([housekeeping/0]).
-export([get_ds_id/0]).
-export([get_number_of_nodes/0, get_node/1, get_all_nodes/0,
         get_random_nodes/2]).
-export([publish_node/1, unpublish_node/1, still_published_nodes/1]).
-export([update_node/2]).
-export([reserve_oa/2, reserved_oas/1]).
-export([lookup_node/1]).

%%% system exports
-export([system_continue/3, system_terminate/4, system_code_change/4,
         system_get_state/1, system_replace_state/2]).

%%% internal exports
-export([init/1]).

%%% include files
-include_lib("ds/include/ds.hrl").
-include_lib("node/include/node.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/bits.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants
-define(NODE_DB, node_db).
-define(OA_DB, oa_db).
-define(LARGEST_NODE_ID, 2147483647).

%%% records
-record(state, {
	  parent                       :: pid(),
	  node_db                      :: atom(),
          oa_db                        :: dets:tid(),
          latest_node_id = 1           :: non_neg_integer(),
          ds_id                        :: node_id(),
          %% anond.conf parameters
          db_directory                 :: binary(),
          db_clear_on_start            :: boolean(),
          node_ttl                     :: non_neg_integer(),
          hard_node_ttl                :: non_neg_integer(),
          max_random_nodes             :: non_neg_integer(),
          max_oas_per_node             :: non_neg_integer(),
	  simulated_node_ids           :: [{na(), node_id()}],
	  simulated_neighbour_node_ids :: [{node_id(), [node_id()]}]
	 }).

%%% types
-define(HOUSEKEEPING_INTERVAL, {4, minutes}).
-define(ONE_MINUTE, 60000).

%%%
%%% exported: start_link
%%%

-spec start_link() -> {'ok', pid()} |
                      {'error',
                       'already_started' |
                       {'node_db_not_available', term()} |
                       {'oa_db_not_available', term()}}.

start_link() ->
    Args = [self()],
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

stop() ->
    stop(15000).

-spec stop(timeout()) -> 'ok'.

stop(Timeout) ->
    serv:call(?MODULE, stop, Timeout).

%%%
%%% exported: housekeeping
%%%

-spec housekeeping() -> 'ok'.

housekeeping() ->
    ?MODULE ! {housekeeping, false},
    ok.

%%%
%%% exported: get_ds_id
%%%

-spec get_ds_id() -> node_id().

get_ds_id() ->
    serv:call(?MODULE, get_ds_id).

%%%
%%% exported: get_number_of_nodes
%%%

-spec get_number_of_nodes() -> {'ok', non_neg_integer()}.

get_number_of_nodes() ->
    serv:call(?MODULE, get_number_of_nodes).

%%%
%%% exported: get_node
%%%

-spec get_node(node_id()) -> {'ok', #node_descriptor{}} |
                             {'error', 'no_such_node'}.

get_node(NodeId) ->
    serv:call(?MODULE, {get_node, NodeId}).

%%%
%%% exported: get_all_nodes
%%%

-spec get_all_nodes() -> {'ok', [#node_descriptor{}]}.

get_all_nodes() ->
    serv:call(?MODULE, get_all_nodes).

%%%
%%% exported: get_random_nodes
%%%

-spec get_random_nodes(node_id(), non_neg_integer()) ->
                              {'ok', [#node_descriptor{}]} |
                              {'error',
                               'too_few_nodes' |
                               {'too_many_nodes', non_neg_integer()}}.

get_random_nodes(MyNodeId, N) ->
    serv:call(?MODULE, {get_random_nodes, MyNodeId, N}).

%%%
%%% exported: publish_node
%%%

-spec publish_node(#node_descriptor{}) ->
                          {'ok', ds_id(), node_id(), SharedKey :: binary(),
                           NodeTTL :: non_neg_integer()}.

publish_node(Nd) ->
    serv:call(?MODULE, {publish_node, Nd}).

%%%
%%% exported: unpublish_node
%%%

-spec unpublish_node(node_id()) -> 'ok'.

unpublish_node(NodeId) ->
    serv:call(?MODULE, {unpublish_node, NodeId}).

%%%
%%% exported: still_published_nodes
%%%

-spec still_published_nodes([node_id()]) -> {'ok', [node_id()]}.

still_published_nodes(NodeIds) ->
    serv:call(?MODULE, {still_published_nodes, NodeIds}).

%%%
%%% exported: update_node
%%%

-spec update_node(node_id(), na()) -> 'ok'.

update_node(NodeId, Na) ->
    ?MODULE ! {update_node, NodeId, Na},
    ok.

%%%
%%% exported: reserve_oa
%%%

-spec reserve_oa(node_id(), oa()) ->
                        'ok' | {'error', 'no_such_node' | 'too_many_oas' |
                                'already_taken'}.

reserve_oa(NodeId, Oa) ->
    serv:call(?MODULE, {reserve_oa, NodeId, Oa}).

%%%
%%% exported: reserved_oas
%%%

-spec reserved_oas(node_id()) -> {'ok', [oa()]} | {'error', 'no_reserved_oas'}.

reserved_oas(NodeId) ->
    serv:call(?MODULE, {reserved_oas, NodeId}).

%%%
%%% exported: lookup_node
%%%

-spec lookup_node(node_id()) -> [#node_descriptor{}].

lookup_node(NodeId) ->
    dets:lookup(?NODE_DB, NodeId).

%%%
%%% server loop
%%%

init(Parent) ->
    process_flag(trap_exit, true),
    ok = config_json_serv:subscribe(),
    {MegaSecs, Secs, MicroSecs} = erlang:now(),
    random:seed(MegaSecs, Secs, MicroSecs),
    case catch register(?MODULE, self()) of
        true ->
            S = read_config(#state{}),
            NodeDbFilename =
                filename:join([S#state.db_directory, <<"node.db">>]),
            OaDbFilename =
                filename:join([S#state.db_directory, <<"oa.db">>]),
            if
                S#state.db_clear_on_start ->
                    file:delete(NodeDbFilename),
                    file:delete(OaDbFilename);
                true ->
                    ok
            end,
            case dets:open_file(?NODE_DB,
                                [{file, NodeDbFilename}, {keypos, 2}]) of
                {ok, NodeDb} ->
                    case dets:open_file(?OA_DB,
                                        [{file, OaDbFilename}, {type, bag}]) of
                        {ok, OaDb} ->
                            timelib:start_timer(
                              ?HOUSEKEEPING_INTERVAL, {housekeeping, true}),
                            DsId = mk_random_ds_id(S#state.simulated_node_ids),
                            Parent ! {self(), started},
                            loop(S#state{parent = Parent, node_db = NodeDb,
                                         oa_db = OaDb, ds_id = DsId});
                        {error, Reason} ->
                            ?daemon_log("~s: ~p", [OaDbFilename, Reason]),
                            Parent ! {self(), {oa_db_not_available, Reason}}
                    end;
                {error, Reason} ->
                    ?daemon_log("~s: ~p", [OaDbFilename, Reason]),
                    Parent ! {self(), {node_db_not_available, Reason}}
            end;
        _ ->
            Parent ! {self(), already_started}
    end.

loop(#state{parent = Parent,
            node_db = NodeDb,
            oa_db = OaDb,
            latest_node_id = LatestNodeId,
            ds_id = DsId,
            db_directory = _DbDirectory,
            db_clear_on_start = _DbCLearOnStart,
            node_ttl = NodeTTL,
            hard_node_ttl = HardNodeTTL,
            max_random_nodes = MaxRandomNodes,
            max_oas_per_node = MaxOasPerNode,
	    simulated_node_ids = SimulatedNodeIds,
	    simulated_neighbour_node_ids = SimulatedNeighbourNodeIds} = S) ->
    receive
        config_updated ->
            ?daemon_log(
               "Directory server starts to update its configuration", []),
            loop(read_config(S));
        {housekeeping, Repeat} ->
            ?daemon_log("Looking for stale and not republished nodes", []),
            StaleNodeIds =
                dets:foldl(
                  fun(#node_descriptor{node_id = NodeId, na = Na,
                                       last_updated = LastUpdated}, Acc) ->
                          Delta = timelib:ugnow_delta({minus, LastUpdated}),
                          if
                              Na == undefined andalso Delta > ?ONE_MINUTE ->
                                  [NodeId|Acc];
                              Delta > HardNodeTTL ->
                                  [NodeId|Acc];
                              true ->
                                  Acc
                          end
                  end, [], NodeDb),
            lists:foreach(
              fun(NodeId) ->
                      ok = dets:delete(NodeDb, NodeId),
                      ok = dets:match_delete(OaDb, {'_', NodeId}),
                      ?daemon_log(
                         "Stale node ~w has been removed and its "
                         "overlay-addresses have been retaken", [NodeId])
              end, StaleNodeIds),
            NotRepublishedNodeIds =
                dets:foldl(
                  fun(#node_descriptor{node_id = NodeId,
                                       last_updated = LastUpdated}, Acc) ->
                          Delta = timelib:ugnow_delta({minus, LastUpdated}),
                          if
                              Delta > NodeTTL ->
                                  [NodeId|Acc];
                              true ->
                                  Acc
                          end
                  end, [], NodeDb),
            lists:foreach(fun(NodeId) ->
                                  [Nd] = dets:lookup(NodeDb, NodeId),
                                  UpdatedNd =
                                      Nd#node_descriptor{
                                        flags = ?bit_set(
                                                   Nd#node_descriptor.flags,
                                                   ?F_DS_NOT_REPUBLISHED)},
                                  ok = dets:insert(NodeDb, UpdatedNd),
                                  ok = dets:match_delete(OaDb, {'_', NodeId}),
                                  ?daemon_log(
                                     "Non-republished node ~w will no longer "
                                     "be published as a possible neighbour "
                                     "node and its overlay-addresses have been "
                                     "retaken", [NodeId])
                          end, NotRepublishedNodeIds),
            if
                Repeat ->
                    timelib:start_timer(
                      ?HOUSEKEEPING_INTERVAL, {housekeeping, true}),
                    loop(S);
                true ->
                    loop(S)
            end;
	{From, stop} ->
	    dets:close(NodeDb),
	    dets:close(OaDb),
	    From ! {self(), ok};
        {From, get_ds_id} ->
            From ! {self(), DsId},
            loop(S);
        {From, get_number_of_nodes} ->
            NumberOfNodes = dets:info(NodeDb, size),
            From ! {self(), {ok, NumberOfNodes}},
            loop(S);
        {From, {get_node, NodeId}} ->
            case dets:lookup(NodeDb, NodeId) of
                [Nd] ->
                    From ! {self(), {ok, Nd}},
                    loop(S);
                [] ->
                    From ! {self(), {error, no_such_node}},
                    loop(S)
            end;
        {From, get_all_nodes} ->
            AllNds = dets:foldl(fun(Nd, Acc) -> [Nd|Acc] end, [], NodeDb),
            From ! {self(), {ok, AllNds}},
            loop(S);
        {From, {get_random_nodes, MyNodeId, N}}
          when is_integer(N) andalso N < MaxRandomNodes ->
            case get_simulated_nodes(SimulatedNeighbourNodeIds, MyNodeId) of
                {ok, NeighbourNodeIds} ->
                    ?daemon_log(
                       "Node ~w was presented with the following simulated "
                       "neighbours: ~w", [MyNodeId, NeighbourNodeIds]),
                    From ! {self(), {ok, NeighbourNodeIds}},
                    loop(S);
		not_simulated ->
		    case dets:info(NodeDb, size) of
			Size when N >= Size ->
			    ?daemon_log(
			       "Node ~w could not be presented with ~w random "
			       "neighbours. Only ~w are available.",
			       [MyNodeId, N, Size]),
			    From ! {self(), {error, too_few_nodes}},
			    loop(S);
			_Size ->
			    RandomNodeIds =
				get_random_nodes(NodeDb, DsId, MyNodeId, N),
			    ?daemon_log(
			       "Node ~w was presented with the following "
			       "random neighbours: ~w",
			       [MyNodeId, RandomNodeIds]),
			    From ! {self(), {ok, RandomNodeIds}},
			    loop(S)
		    end
	    end;
        {From, {get_random_nodes, _MyNodeId, N}} when is_integer(N) ->
            From ! {self(), {error, {too_many_nodes, MaxRandomNodes}}},
            loop(S);
        {From, {publish_node, #node_descriptor{node_id = 0, na = Na} = Nd}} ->
	    {ok, NewNodeId} =
		allocate_node_id(NodeDb, LatestNodeId, DsId, SimulatedNodeIds,
				 Na),
	    SharedKey = salt:crypto_random_bytes(32),
	    UpdatedNd =
		Nd#node_descriptor{node_id = NewNodeId,
				   na = undefined,
				   shared_key = SharedKey,
				   last_updated = timelib:ugnow()},
	    ok = dets:insert(NodeDb, UpdatedNd),
	    ?daemon_log("Node ~w has been published", [NewNodeId]),
	    From ! {self(), {ok, DsId, NewNodeId, SharedKey, NodeTTL}},
	    loop(S#state{latest_node_id = NewNodeId});
        {From, {publish_node, #node_descriptor{node_id = NodeId} = Nd}} ->
            SharedKey = salt:crypto_random_bytes(32),
            UpdatedNd = Nd#node_descriptor{shared_key = SharedKey,
                                           na = undefined,
                                           last_updated = timelib:ugnow()},
            ok = dets:insert(NodeDb, UpdatedNd),
            ?daemon_log("Node ~w has been republished", [NodeId]),
            From ! {self(), {ok, DsId, NodeId, SharedKey, NodeTTL}},
            loop(S);
        {update_node, NodeId, Na} ->
            case dets:lookup(NodeDb, NodeId) of
                [Nd] ->
                    ok = dets:insert(NodeDb, Nd#node_descriptor{na = Na}),
                    loop(S);
                [] ->
                    loop(S)
            end;
        {From, {unpublish_node, NodeId}} ->
            ok = dets:delete(NodeDb, NodeId),
            ?daemon_log("Node ~w has been unpublished", [NodeId]),
            From ! {self(), ok},
            loop(S);
        {From, {still_published_nodes, NodeIds}} ->
            StillPublishedNodeIds =
                lists:foldl(
                  fun(NodeId, Acc) ->
                          case dets:lookup(NodeDb, NodeId) of
                              [] ->
                                  Acc;
                              [Nd] ->
                                  if
                                      ?bit_is_clr(Nd#node_descriptor.flags,
                                                  ?F_DS_NOT_REPUBLISHED) ->
                                          [NodeId|Acc];
                                      true ->
                                          Acc
                                  end
                          end
                  end, [], NodeIds),
            From ! {self(), {ok, StillPublishedNodeIds}},
            loop(S);
        {From, {reserve_oa, NodeId, Oa}} ->
            case dets:lookup(NodeDb, NodeId) of
                [] ->
                    From ! {self(), {error, no_such_node}},
                    loop(S);
                _ ->
                    case dets:lookup(OaDb, Oa) of
                        [] ->
                            NumberOfOas =
                                length(dets:match(OaDb, {'$1', NodeId})),
                            if
                                NumberOfOas =< MaxOasPerNode ->
                                    ok = dets:insert(OaDb, {Oa, NodeId}),
                                    ?daemon_log(
                                       "Node ~w has reserved overlay address "
                                       "~s", [NodeId,
                                              net_tools:string_address(Oa)]),
                                    From ! {self(), ok},
                                    loop(S);
                                true ->
                                    ?daemon_log("Node ~w tried to reserve more "
                                                "than ~w overlay addresses",
                                                [NodeId, MaxOasPerNode]),
                                    From ! {self(), {error, too_many_oas}},
                                    loop(S)
                            end;
                        [{Oa, NodeId}] ->
                            ?daemon_log(
                               "Node ~w has reserved overlay address "
                               "~s again", [NodeId,
                                            net_tools:string_address(Oa)]),
                            From ! {self(), ok},
                            loop(S);
                        [{Oa, ReservingNodeId}] ->
                            ?daemon_log(
                               "Node ~w tried to reserve overlay-address ~s "
                               "already taken by ~w",
                               [NodeId, net_tools:string_address(Oa),
                                ReservingNodeId]),
                            From ! {self(), {error, already_taken}},
                            loop(S)
                    end
            end;
        {From, {reserved_oas, NodeId}} ->
            case dets:match(OaDb, {'$1', NodeId}) of
                [Oas] ->
                    From ! {self(), {ok, Oas}},
                    loop(S);
                [] ->
                    From ! {self(), {error, no_reserved_oas}},
                    loop(S)
            end;
        {'EXIT', Parent, Reason} ->
            dets:close(NodeDb),
            dets:close(OaDb),
            exit(Reason);
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

%%%
%%% init
%%%

read_config(S) ->
    DbDirectory = ?config(['directory-server', db, directory]),
    DbClearOnStart = ?config(['directory-server', db, 'clear-on-start']),
    NodeTTL = ?config(['directory-server', 'node-ttl']),
    HardNodeTTL = ?config(['directory-server', 'hard-node-ttl']),
    MaxRandomNodes = ?config(['directory-server', 'max-random-nodes']),
    MaxOasPerNode = ?config(['directory-server', 'max-oas-per-node']),
    SimulatedNodeIds =
	[{Na, NodeId} ||
	    [{'node-address', Na},
	     {simulation, [{'node-id', NodeId}|_]}|_] <- ?config(['nodes'])],
    SimulatedNeighbourNodeIds =
	[{NodeId,
	  [NeighbourNodeId ||
	      [{'node-id', NeighbourNodeId}|_] <- Neighbours]} ||
	    [{'node-address', _Na},
	     {simulation, [{'node-id', NodeId},
			   {'neighbours', Neighbours}|_]}|_] <-
		?config(['nodes'])],
    S#state{db_directory = DbDirectory, db_clear_on_start = DbClearOnStart,
	    node_ttl = NodeTTL, hard_node_ttl = HardNodeTTL,
	    max_random_nodes = MaxRandomNodes, max_oas_per_node = MaxOasPerNode,
	    simulated_node_ids = SimulatedNodeIds,
	    simulated_neighbour_node_ids = SimulatedNeighbourNodeIds
	   }.

mk_random_ds_id(SimulatedNodeIds) ->
    DsId = random:uniform(?LARGEST_NODE_ID-1)+1,
    case lists:keymember(DsId, 2, SimulatedNodeIds) of
        true ->
            mk_random_ds_id(SimulatedNodeIds);
        false ->
            DsId
    end.

%%%
%%% get_random_nodes
%%%

get_simulated_nodes(SimulatedNeighbourNodeIds, MyNodeId) ->
    case lists:keysearch(MyNodeId, 1, SimulatedNeighbourNodeIds) of
        {value, {MyNodeId, NeighbourNodeIds}} ->
            {ok, NeighbourNodeIds};
        false ->
            not_simulated
    end.

%% http://en.wikipedia.org/wiki/Reservoir_sampling
get_random_nodes(NodeDb, DsId, MyNodeId, N) ->
    Tid = ets:new(sample_db, []),
    NextKey = init_sample(NodeDb, DsId, MyNodeId, dets:first(NodeDb), N, Tid),
    Sample = extract_node_sample(NodeDb, DsId, MyNodeId, NextKey, N, Tid, N+1),
    ets:delete(Tid),
    Sample.

init_sample(_NodeDb, _DsId, _MyNodeId, NodeId, 0, _Tid) ->
    NodeId;
init_sample(NodeDb, DsId, MyNodeId, MyNodeId, N, Tid) ->
    init_sample(NodeDb, DsId, MyNodeId, dets:next(NodeDb, MyNodeId), N, Tid);
init_sample(NodeDb, DsId, MyNodeId, DsId, N, Tid) ->
    init_sample(NodeDb, DsId, MyNodeId, dets:next(NodeDb, DsId), N, Tid);
init_sample(NodeDb, DsId, MyNodeId, NodeId, N, Tid) ->
    case dets:lookup(NodeDb, NodeId) of
        [#node_descriptor{na = undefined}] ->
            init_sample(
              NodeDb, DsId, MyNodeId, dets:next(NodeDb, NodeId), N-1, Tid);
        [_Nd] ->
            true = ets:insert(Tid, {N, NodeId}),
            init_sample(
              NodeDb, DsId, MyNodeId, dets:next(NodeDb, NodeId), N-1, Tid)
    end.

extract_node_sample(_NodeDb, _DsId, _MyNodeId, '$end_of_table', _N, Tid, _M) ->
    ets:foldl(fun({_, NodeId}, Acc) -> [NodeId|Acc] end, [], Tid);
extract_node_sample(NodeDb, DsId, MyNodeId, MyNodeId, N, Tid, M) ->
    extract_node_sample(
      NodeDb, DsId, MyNodeId, dets:next(NodeDb, MyNodeId), N, Tid, M);
extract_node_sample(NodeDb, DsId, MyNodeId, DsId, N, Tid, M) ->
    extract_node_sample(
      NodeDb, DsId, MyNodeId, dets:next(NodeDb, DsId), N, Tid, M);
extract_node_sample(NodeDb, DsId, MyNodeId, NodeId, N, Tid, M) ->
    case random:uniform(M) of
        RandomN when RandomN =< N ->
            case dets:lookup(NodeDb, NodeId) of
                [#node_descriptor{na = Na, flags = Flags}]
                  when ?bit_is_clr(Flags, ?F_DS_NOT_REPUBLISHED) andalso
                       Na /= undefined ->
                    true = ets:insert(Tid, {RandomN, NodeId}),
                    extract_node_sample(NodeDb, DsId, MyNodeId,
                                        dets:next(NodeDb, NodeId), N, Tid, M+1);
                [_Nd] ->
                    extract_node_sample(NodeDb, DsId, MyNodeId,
                                        dets:next(NodeDb, NodeId), N, Tid, M)
            end;
        _ ->
            extract_node_sample(NodeDb, DsId, MyNodeId,
                                dets:next(NodeDb, NodeId), N, Tid, M+1)
    end.

%%%
%%% publish_node
%%%

allocate_node_id(NodeDb, LatestNodeId, DsId, SimulatedNodeIds, Na) ->
    case lists:keysearch(Na, 1, SimulatedNodeIds) of
        {value, {Na, NodeId}} ->
            {ok, NodeId};
        false ->
	    do_allocate_node_id(NodeDb, LatestNodeId, DsId, SimulatedNodeIds,
				Na)
    end.

do_allocate_node_id(NodeDb, LatestNodeId, DsId, SimulatedNodeIds, Na)
  when LatestNodeId == ?LARGEST_NODE_ID ->
    do_allocate_node_id(NodeDb, 1, DsId, SimulatedNodeIds, Na);
do_allocate_node_id(NodeDb, LatestNodeId, DsId, SimulatedNodeIds, Na) ->
    NextNodeId = LatestNodeId+1,
    if
        NextNodeId == DsId ->
            do_allocate_node_id(NodeDb, NextNodeId, DsId, SimulatedNodeIds, Na);
        true ->
            case dets:lookup(NodeDb, NextNodeId) of
                [] ->
		    case lists:keymember(NextNodeId, 2, SimulatedNodeIds) of
			true ->
			    do_allocate_node_id(NodeDb, NextNodeId, DsId,
						SimulatedNodeIds, Na);
			false ->
			    {ok, NextNodeId}
		    end;
                _ ->
                    do_allocate_node_id(NodeDb, NextNodeId, DsId,
					SimulatedNodeIds, Na)
            end
    end.
