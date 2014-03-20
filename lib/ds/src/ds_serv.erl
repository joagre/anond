-module(ds_serv).

%%% external exports
-export([start_link/0, stop/0, stop/1]).
-export([housekeeping/0]).
-export([get_number_of_nodes/0, get_node/1, get_all_nodes/0,
         get_random_nodes/2]).
-export([publish_node/1, unpublish_node/1, published_nodes/1]).
-export([reserve_oa/2, reserved_oas/1]).
-export([lookup_node/1]).

%%% internal exports
-export([init/1]).

%%% include files
-include_lib("ds/include/ds.hrl").
-include_lib("node/include/node.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants
-define(NODE_DB, node_db).

%%% records
-record(state, {
	  parent           :: pid(),
	  node_db          :: atom(),
          oa_db            :: ets:tid(),
          %% anond.conf parameters
          mode                  :: common_config_jsonrpc_serv:mode(),
          db_path               :: binary(),
          db_clear_on_start     :: boolean(),
          node_ttl              :: non_neg_integer(),
          max_oas_per_node      :: non_neg_integer()
	 }).

%%% types
-define(TEN_MINUTES, (1000*60*10)).

%%%
%%% exported: start_link
%%%

-spec start_link() -> {'ok', pid()} |
                      {'error', 'already_started' |
                                {'node_db_not_available', term()}}.

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

-spec stop() -> 'ok'.

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
%%% exported: get_number_of_nodes
%%%

-spec get_number_of_nodes() -> {'ok', non_neg_integer()}.

get_number_of_nodes() ->
    serv:call(?MODULE, get_number_of_nodes).

%%%
%%% exported: get_node
%%%

-spec get_node(na()) -> {'ok', #node_descriptor{}} | {'error', 'no_such_node'}.

get_node(Na) ->
    serv:call(?MODULE, {get_node, Na}).

%%%
%%% exported: get_all_nodes
%%%

-spec get_all_nodes() -> {'ok', [#node_descriptor{}]}.

get_all_nodes() ->
    serv:call(?MODULE, get_all_nodes).

%%%
%%% exported: get_random_nodes
%%%

-spec get_random_nodes(na(), non_neg_integer()) ->
                              {'ok', [#node_descriptor{}]} |
                              {'error', 'too_few_nodes'}.
get_random_nodes(MyNa, N) ->
    serv:call(?MODULE, {get_random_nodes, MyNa, N}).

%%%
%%% exported: publish_node
%%%

-spec publish_node(#node_descriptor{}) -> {'ok', NodeTTL :: non_neg_integer()}.

publish_node(NodeDescriptor) ->
    serv:call(?MODULE, {publish_node, NodeDescriptor}).

%%%
%%% exported: unpublish_node
%%%

-spec unpublish_node(na()) -> 'ok'.

unpublish_node(Na) ->
    serv:call(?MODULE, {unpublish_node, Na}).

%%%
%%% exported: published_nodes
%%%

-spec published_nodes([na()]) -> {'ok', [na()]}.

published_nodes(Nas) ->
    serv:call(?MODULE, {published_nodes, Nas}).

%%%
%%% exported: reserve_oa
%%%

-spec reserve_oa(oa(), na()) ->
                        'ok' | {'error', 'no_such_node' | 'too_many_oas'}.

reserve_oa(Oa, Na) ->
    serv:call(?MODULE, {reserve_oa, Oa, Na}).

%%%
%%% exported: reserved_oas
%%%

-spec reserved_oas(na()) -> {'ok', [oa()]} | {'error', 'no_reserved_oas'}.

reserved_oas(Na) ->
    serv:call(?MODULE, {reserved_oas, Na}).

%%%
%%% exported: lookup_node
%%%

-spec lookup_node(na()) -> [#node_descriptor{}].

lookup_node(Na) ->
    dets:lookup(?NODE_DB, Na).

%%%
%%% server loop
%%%

init(Parent) ->
    process_flag(trap_exit, true),
    case catch register(?MODULE, self()) of
        true ->
            S = read_config(#state{}),
            if
                S#state.db_clear_on_start ->
                    file:delete(S#state.db_path);
                true ->
                    keep
            end,
            case dets:open_file(?NODE_DB,
                                [{file, S#state.db_path}, {keypos, 2}]) of
                {ok, NodeDb} ->
                    {A1, A2, A3} = erlang:now(),
                    random:seed({A1, A2, A3}),
                    ok = config_json_serv:subscribe(),
                    OaDb = ets:new(oa_db, [bag]),
                    timelib:start_timer(?TEN_MINUTES, {housekeeping, true}),
                    Parent ! {self(), started},
                    loop(S#state{parent = Parent, node_db = NodeDb,
                                 oa_db = OaDb});
                {error, Reason} ->
                    ?daemon_log("~s: Not available (~p)",
                                [S#state.db_path, Reason]),
                    Parent ! {self(), {node_db_not_available, Reason}}
            end;
        _ ->
            Parent ! {self(), already_started}
    end.

loop(#state{parent = Parent,
            node_db = NodeDb,
            oa_db = OaDb,
            mode = Mode,
            db_path = _DbPath,
            db_clear_on_start = _DbCLearOnStart,
            node_ttl = NodeTTL,
            max_oas_per_node = MaxOasPerNode} = S) ->
    receive
        config_updated ->
            ?daemon_log("Configuration changed...", []),
            loop(read_config(S));
        {housekeeping, Repeat} ->
            ?daemon_log("Looking for stale nodes...", []),
            StaleNas =
                dets:foldl(
                  fun(#node_descriptor{na = Na, last_updated = LastUpdated},
                      Acc) ->
                          Delta = timelib:ugnow_delta({minus, LastUpdated}),
                          if
                              Delta > NodeTTL ->
                                  [Na|Acc];
                              true ->
                                  []
                          end
                  end, [], NodeDb),
            lists:foreach(fun(Na) ->
                                  ok = dets:delete(NodeDb, Na),
                                  true = ets:match_delete(OaDb, {'_', Na}),
                                  ?daemon_log("Removed stale node ~s",
                                              [net_tools:string_address(Na)])
                          end, StaleNas),
            if
                Repeat ->
                    timelib:start_timer(?TEN_MINUTES, {housekeeping, true}),
                    loop(S);
                true ->
                    loop(S)
            end;
	{From, stop} ->
            ?daemon_log("Stopping directory server...", []),
	    dets:close(NodeDb),
	    ets:delete(OaDb),
	    From ! {self(), ok};
        {From, get_number_of_nodes} ->
            NumberOfNodes = dets:info(NodeDb, size),
            ?daemon_log("Calculated number of nodes (~w)", [NumberOfNodes]),
            From ! {self(), {ok, NumberOfNodes}},
            loop(S);
        {From, {get_node, Na}} ->
            case dets:lookup(NodeDb, Na) of
                [NodeDescriptor] ->
                    From ! {self(), {ok, NodeDescriptor}},
                    loop(S);
                [] ->
                    From ! {self(), {error, no_such_node}},
                    loop(S)
            end;
        {From, get_all_nodes} ->
            AllNodeDescriptors =
                dets:foldl(fun(NodeDescriptor, Acc) ->
                                   [NodeDescriptor|Acc]
                           end, [], NodeDb),
            ?daemon_log("Extracted all (~w) nodes",
                        [length(AllNodeDescriptors)]),
            From ! {self(), {ok, AllNodeDescriptors}},
            loop(S);
        %% see doc/small_simulation.jpg; return two non-random nodes
        {From, {get_random_nodes, MyNa, 2}} when Mode == simulation ->
            case get_simulated_nodes(NodeDb, MyNa) of
                {ok, SimulatedNas, SimulatedNodeDescriptors} ->
                    ?daemon_log(
                       "Extracted ~w simulated and non-random nodes: ~s",
                       [length(SimulatedNas),
                        net_tools:string_addresses(SimulatedNas)]),
                    From ! {self(), {ok, SimulatedNodeDescriptors}},
                    loop(S);
                {error, Reason} ->
                    ?daemon_log("2 random nodes could not be extracted", []),
                    From ! {self(), {error, Reason}},
                    loop(S)
            end;
        {From, {get_random_nodes, MyNa, N}} when is_integer(N) ->
            case dets:info(NodeDb, size) of
                Size when N >= Size ->
                    ?daemon_log("~w random nodes could not be extracted",
                                [Size]),
                    From ! {self(), {error, too_few_nodes}},
                    loop(S);
                _Size ->
                    RandomNodeDescriptors = get_random_nodes(NodeDb, MyNa, N),
                    RandomNas =
                        [RandomNodeDescriptor#node_descriptor.na ||
                            RandomNodeDescriptor <- RandomNodeDescriptors],
                    ?daemon_log("Extracted ~w random nodes: ~s",
                                [length(RandomNas),
                                 net_tools:string_addresses(RandomNas)]),
                    From ! {self(), {ok, RandomNodeDescriptors}},
                    loop(S)
            end;
        {From, {publish_node, #node_descriptor{na = Na} = NodeDescriptor}} ->
            UpdatedNodeDescriptor =
                NodeDescriptor#node_descriptor{last_updated = timelib:ugnow()},
            ok = dets:insert(NodeDb, UpdatedNodeDescriptor),
            ?daemon_log("Node ~s (re)published",
                        [net_tools:string_address(Na)]),
            From ! {self(), {ok, NodeTTL}},
            loop(S);
        {From, {unpublish_node, Na}} ->
            ok = dets:delete(NodeDb, Na),
            ?daemon_log("Node ~s unpublished", [net_tools:string_address(Na)]),
            From ! {self(), ok},
            loop(S);
        {From, {published_nodes, Nas}} ->
            StillPublishedNas =
                [Na || Na <- Nas, dets:member(NodeDb, Na) == true],
            ?daemon_log("Out of these nodes ~s, these are still published ~s",
                        [net_tools:string_addresses(Nas),
                         net_tools:string_addresses(StillPublishedNas)]),
            From ! {self(), {ok, StillPublishedNas}},
            loop(S);
        {From, {reserve_oa, Oa, Na}} ->
            case dets:lookup(NodeDb, Na) of
                [] ->
                    ?daemon_log(
                       "Rejected reservation of overlay address ~s to unknown "
                       "node ~s",
                       [net_tools:string_address(Oa),
                        net_tools:string_address(Na)]),
                    From ! {self(), {error, no_such_node}},
                    loop(S);
                _ ->
                    NumberOfOas = length(ets:lookup(OaDb, Oa)),
                    if
                        NumberOfOas =< MaxOasPerNode ->
                            true = ets:insert(OaDb, {Oa, Na}),
                            ?daemon_log(
                               "Reserved overlay address ~s to node ~s",
                               [net_tools:string_address(Oa),
                                net_tools:string_address(Na)]),
                            From ! {self(), ok},
                            loop(S);
                        true ->
                            ?daemon_log("Node ~s tried to reserve more than ~w "
                                        "overlay addresses",
                                        [net_tools:string_address(Na),
                                         MaxOasPerNode]),
                            From ! {self(), {error, too_many_oas}},
                            loop(S)
                    end
            end;
        {From, {reserved_oas, Na}} ->
            case ets:match(OaDb, {'$1', Na}) of
                [Oas] ->
                    ?daemon_log("~s has these overlay addresses reserved: ~s",
                                [net_tools:string_address(Na),
                                 net_tools:string_addresses(Oas)]),
                    From ! {self(), {ok, Oas}},
                    loop(S);
                [] ->
                    From ! {self(), {error, no_reserved_oas}},
                    loop(S)
            end;
        {'EXIT', Parent, Reason} ->
	    ok = dets:close(NodeDb),
            exit(Reason);
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            loop(S)
    end.

%%%
%%% init
%%%

read_config(S) ->
    Mode = ?config([mode]),
    DbPath = ?config(['directory-server', db, path]),
    DbClearOnStart = ?config(['directory-server', db, 'clear-on-start']),
    NodeTTL = ?config(['directory-server', 'node-ttl']),
    MaxOasPerNode = ?config(['directory-server', 'max-oas-per-node']),
    S#state{mode = Mode, db_path = DbPath, db_clear_on_start = DbClearOnStart,
            node_ttl = NodeTTL, max_oas_per_node = MaxOasPerNode}.

%%%
%%% get_random_nodes
%%%

get_simulated_nodes(NodeDb, {MyIpAddress, MyPort}) ->
    {value, {MyPort, NeighbourNodePorts}} =
        lists:keysearch(MyPort, 1, ?NON_RANDOM_NODES),
    NeighbourNodeNas = [{MyIpAddress, NeighbourNodePort} ||
                           NeighbourNodePort <- NeighbourNodePorts],
    case lookup_simulated_nodes(NodeDb, NeighbourNodeNas) of
        {ok, NodeDescriptors} ->
            {ok, NeighbourNodeNas, NodeDescriptors};
        {error, Reason} ->
            {error, Reason}
    end.

lookup_simulated_nodes(NodeDb, NeighbourNodeNas) ->
    lookup_simulated_nodes(NodeDb, NeighbourNodeNas, []).

lookup_simulated_nodes(_NodeDb, [], Acc) ->
    {ok, lists:reverse(Acc)};
lookup_simulated_nodes(NodeDb, [NeighbourNodeNa|Rest], Acc) ->
    case dets:lookup(NodeDb, NeighbourNodeNa) of
        [NodeDescriptor] ->
            lookup_simulated_nodes(NodeDb, Rest, [NodeDescriptor|Acc]);
        [] ->
            {error, too_few_nodes}
    end.

%% http://en.wikipedia.org/wiki/Reservoir_sampling
get_random_nodes(NodeDb, MyNa, N) ->
    Tid = ets:new(sample_db, []),
    NextKey = init_sample(NodeDb, MyNa, dets:first(NodeDb), N, Tid),
    Sample = extract_node_sample(NodeDb, MyNa, NextKey, N, Tid, N+1),
    ets:delete(Tid),
    Sample.

init_sample(_NodeDb, _MyNa, Na, 0, _Tid) ->
    Na;
init_sample(NodeDb, MyNa, MyNa, N, Tid) ->
    init_sample(NodeDb, MyNa, dets:next(NodeDb, MyNa), N, Tid);
init_sample(NodeDb, MyNa, Na, N, Tid) ->
    [NodeDescriptor] = dets:lookup(NodeDb, Na),
    true = ets:insert(Tid, {N, NodeDescriptor}),
    init_sample(NodeDb, MyNa, dets:next(NodeDb, Na), N-1, Tid).

extract_node_sample(_NodeDb, _MyNa, '$end_of_table', _N, Tid, _M) ->
    ets:foldl(fun({_, NodeDescriptor}, Acc) ->
                      [NodeDescriptor|Acc]
              end, [], Tid);
extract_node_sample(NodeDb, MyNa, MyNa, N, Tid, M) ->
    extract_node_sample(NodeDb, MyNa, dets:next(NodeDb, MyNa), N, Tid, M);
extract_node_sample(NodeDb, MyNa, Na, N, Tid, M) ->
    case random:uniform(M) of
        RandomN when RandomN =< N ->
            [NodeDescriptor] = dets:lookup(NodeDb, Na),
            true = ets:insert(Tid, {RandomN, NodeDescriptor}),
            extract_node_sample(NodeDb, MyNa, dets:next(NodeDb, Na), N, Tid,
                                M+1);
        _ ->
            extract_node_sample(NodeDb, MyNa, dets:next(NodeDb, Na), N, Tid,
                                M+1)
    end.
