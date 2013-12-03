-module(overseer_serv).

%%% external exports
-export([start_link/0, stop/0, stop/1]).
-export([get_global_route_table/0, get_route_table/1]).
-export([get_neighbours/0, get_neighbours/1]).
-export([enable_recalc/0, enable_recalc/1, disable_recalc/0, disable_recalc/1]).
-export([recalc/0, recalc/1]).
-export([get_path_cost/2, update_path_cost/3]).

%%% internal exports
-export([init/1]).

%%% include files
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").
-include_lib("util/include/config.hrl").
-include_lib("node/include/node.hrl").
-include_lib("node/include/node_route.hrl").
-include_lib("overseer/include/simulation.hrl").

%%% constants
-define(NUMBER_OF_SIMULATION_NODES, 10).
-define(MAX_PATH_COST, 100).
-define(PERCENT_NUDGE, 5).

%%% records
-record(state, {
	  parent          :: pid(),
	  path_costs  :: [{{oa(), oa()}, path_cost()}],
	  nodes           :: [{oa(), ip()}],
          simulation      :: boolean(),
          number_of_nodes :: integer()
	 }).

%%% types
-type global_route_table() :: [{oa(), {oa(), path_cost(), [oa()]}}].
-type route_table() :: [{oa(), path_cost(), [oa()]}].
-type neighbours() :: [{oa(), path_cost()}].

%%%
%%% exported: start_link
%%%

-spec start_link() -> {'ok', pid()} |
                      {'error', 'already_started'}.

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
%%% exported: get_global_route_table
%%%

-spec get_global_route_table() -> global_route_table().

get_global_route_table() ->
    serv:call(?MODULE, get_global_route_table).

%%%
%%% exported: get_route_table
%%%

-spec get_route_table(oa()) -> {'ok', route_table()} | 'unknown_oa'.

get_route_table(Oa) ->
    serv:call(?MODULE, {get_route_table, Oa}).

%%%
%%% exported: get_neighbours
%%%

-spec get_neighbours() -> {'ok', [{oa(), neighbours()}]}.

get_neighbours() ->
    serv:call(?MODULE, get_neighbours).

-spec get_neighbours(oa()) -> {'ok', neighbours()} | 'unknown_oa'.

get_neighbours(Oa) ->
    serv:call(?MODULE, {get_neighbours, Oa}).

%%%
%%% exported: enable_recalc
%%%

-spec enable_recalc() -> 'ok'.

enable_recalc() ->
    serv:call(?MODULE, enable_recalc).

-spec enable_recalc(oa()) -> 'ok'.

enable_recalc(Oa) ->
    serv:call(?MODULE, {enable_recalc, Oa}).

%%%
%%% exported: disable_recalc
%%%

-spec disable_recalc() -> 'ok'.

disable_recalc() ->
    serv:call(?MODULE, disable_recalc).

-spec disable_recalc(oa()) -> 'ok'.

disable_recalc(Oa) ->
    serv:call(?MODULE, {disable_recalc, Oa}).

%%%
%%% exported: recalc
%%%

-spec recalc() -> 'ok'.

recalc() ->
    serv:call(?MODULE, recalc).

-spec recalc(oa()) -> 'ok' | 'unknown_oa'.

recalc(Oa) ->
    serv:call(?MODULE, {recalc, Oa}).

%%%
%%% exported: get_path_cost
%%%

-spec get_path_cost(ip(), ip()) -> {'ok', path_cost()}.

get_path_cost(Ip, PeerIp) ->
    serv:call(?MODULE, {get_path_cost, Ip, PeerIp}).

%%%
%%% exported: update_path_cost
%%%

-spec update_path_cost(oa(), oa(), path_cost()) ->
                                 'ok' | 'unknown_path_cost'.

update_path_cost(Oa, PeerOa, Pc) ->
    serv:call(?MODULE, {update_path_cost, Oa, PeerOa, Pc}).

%%%
%%% server loop
%%%

init(Parent) ->
    process_flag(trap_exit, true),
    case catch register(?MODULE, self()) of
        true ->
            S = read_config(#state{}),
            ok = config_serv:subscribe(),
            case S#state.simulation of
                true ->
                    Nodes = start_nodes(?NUMBER_OF_SIMULATION_NODES);
                false ->
                    Nodes = start_nodes(S#state.number_of_nodes)
            end,
	    Parent ! {self(), started},
	    loop(S#state{parent = Parent,
                         path_costs = ?NON_RANDOM_PATH_COSTS,
                         nodes = Nodes});
        _ ->
            Parent ! {self(), already_started}
    end.

loop(#state{parent = Parent,
	    path_costs = Pcs,
	    nodes = Nodes,
            simulation = Simulation} = S) ->
    receive
        config_updated ->
            loop(read_config(S));
	{From, stop} ->
	    From ! {self(), ok};
	{From, get_global_route_table} ->
	    From ! {self(), get_global_route_table(Nodes)},
	    loop(S);
	{From, {get_route_table, Oa}} ->
            case lookup_ip(Nodes, Oa) of
                unknown_oa ->
                    From ! {self(), unknown_oa},
                    loop(S);
                Ip ->
                    {ok, Res} = node_serv:get_route_entries(Ip),
                    RouteTable =
                        [{ReOa, Pc, lookup_oa(Nodes, Hops)} ||
                            #route_entry{oa = ReOa,
                                           path_cost = Pc,
                                           hops = Hops} <- Res],
                    From ! {self(), {ok, RouteTable}},
                    loop(S)
            end;
	{From, get_neighbours} ->
            AllNeighbours =
                lists:map(
                  fun({Oa, Ip}) ->
                          {ok, ActualNodes} = node_serv:get_nodes(Ip),
                          {Oa, [{lookup_oa(Nodes, ActualNode#node.ip),
                                 ActualNode#node.path_cost} ||
                                   ActualNode <- ActualNodes]}
                  end, Nodes),
            From ! {self(), {ok, AllNeighbours}},
            loop(S);
	{From, {get_neighbours, Oa}} ->
            case lookup_ip(Nodes, Oa) of
                unknown_oa ->
                    From ! {self(), unknown_oa},
                    loop(S);
                Ip ->
                    {ok, ActualNodes} = node_serv:get_nodes(Ip),
                    Neighbours =
                        [{lookup_oa(Nodes, ActualNode#node.ip),
                          ActualNode#node.path_cost} ||
                            ActualNode <- ActualNodes],
                    From ! {self(), {ok, Neighbours}},
                    loop(S)
            end;
	{From, enable_recalc} ->
            lists:foreach(fun({_Oa, Ip}) ->
                                  ok = node_serv:enable_recalc(Ip)
                          end, Nodes),
            From ! {self(), ok},
            loop(S);
        {From, {enable_recalc, Oa}} ->
            case lookup_ip(Nodes, Oa) of
                unknown_oa ->
                    From ! {self(), unknown_oa},
                    loop(S);
                Ip ->
                    From ! {self(), node_serv:enable_recalc(Ip)},
                    loop(S)
            end;
	{From, disable_recalc} ->
            lists:foreach(fun({_Oa, Ip}) ->
                                  ok = node_serv:disable_recalc(Ip)
                          end, Nodes),
            From ! {self(), ok},
            loop(S);
        {From, {disable_recalc, Oa}} ->
            case lookup_ip(Nodes, Oa) of
                unknown_oa ->
                    From ! {self(), unknown_oa},
                    loop(S);
                Ip ->
                    From ! {self(), node_serv:disable_recalc(Ip)},
                    loop(S)
            end;
	{From, recalc} ->
            lists:foreach(fun({_Oa, Ip}) ->
                                  ok = node_serv:recalc(Ip)
                          end, Nodes),
            From ! {self(), ok},
            loop(S);
        {From, {recalc, Oa}} ->
            case lookup_ip(Nodes, Oa) of
                unknown_oa ->
                    From ! {self(), unknown_oa},
                    loop(S);
                Ip ->
                    From ! {self(), node_serv:recalc(Ip)},
                    loop(S)
            end;
        {From, {get_path_cost, Ip, PeerIp}} when Simulation == true ->
            Oa = lookup_oa(Nodes, Ip),
            PeerOa = lookup_oa(Nodes, PeerIp),
            {value, {_, Pc}} = lists:keysearch({Oa, PeerOa}, 1, Pcs),
            From ! {self(), {ok, nudge_path_cost(Pc, ?PERCENT_NUDGE)}},
            loop(S);
        {From, {get_path_cost, Ip, PeerIp}} ->
            Oa = lookup_oa(Nodes, Ip),
            PeerOa = lookup_oa(Nodes, PeerIp),
            case lists:keysearch({Oa, PeerOa}, 1, Pcs) of
                {value, {_, Pc}} ->
                    From ! {self(),
                            {ok, nudge_path_cost(Pc, ?PERCENT_NUDGE)}},
                    loop(S);
                false ->
                    RandomPc = random:uniform(?MAX_PATH_COST),
                    From ! {self(), {ok, RandomPc}},
                    UpdatedPcs = [{{Oa, PeerOa}, RandomPc},
                                  {{PeerOa, Oa}, RandomPc}|Pcs],
                    loop(S#state{path_costs = UpdatedPcs})
            end;
	{From, {update_path_cost, Oa, PeerOa, Pc}} ->
            case update_path_costs(Oa, PeerOa, Pc, Pcs) of
                unknown_path_cost ->
                    From ! {self(), unknown_path_cost},
                    loop(S);
                UpdatedPcs ->
                    From ! {self(), ok},
                    loop(S#state{path_costs = UpdatedPcs})
            end;
        {'EXIT', Parent, Reason} ->
            exit(Reason);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

%%%
%%% init
%%%

read_config(S) ->
    [Simulation] = ?cfg(['simulation']),
    [NumberOfNodes] = ?cfg(['number-of-nodes']),
    S#state{simulation = Simulation, number_of_nodes = NumberOfNodes}.

start_nodes(0) ->
    [];
start_nodes(N) ->
    %% patrik: public_key:generate_key/1 is hard to understand in the
    %% manual page public_key(3). Does it even generate RSA keys?
    %%{PublicKey, PrivateKey} = public_key:generate_key(),
    PublicKey = <<"foo">>,
    PrivateKey = <<"baz">>,
    Oa = {1,1,1,1,1,1,1,N},
    Na = {{0,0,0,0}, 50000+N},
    {ok, NodeSup} =
        node_root_sup:start_node(Oa, Na, PublicKey, PrivateKey, true),
    Children = supervisor:which_children(NodeSup),
    {value, {_Id, Ip, _Type, _Modules}} =
        lists:keysearch(node_serv, 1, Children),
    [{N, Ip}|start_nodes(N-1)].

%%%
%%% get_global_route_table
%%%

get_global_route_table(Nodes) ->
    AllRouteEntries = get_all_route_entries(Nodes),
    merge_route_entries(AllRouteEntries, AllRouteEntries).

get_all_route_entries([]) ->
    [];
get_all_route_entries([{Oa, Ip}|Rest]) ->
    {ok, RouteEntries} = node_serv:get_route_entries(Ip),
    [{Oa, Ip, RouteEntries}|get_all_route_entries(Rest)].

merge_route_entries([], _AllRouteEntries) ->
    [];
merge_route_entries([{Oa, _Ip, RouteEntries}|Rest], AllRouteEntries) ->
    [traverse_each_destination(Oa, RouteEntries, AllRouteEntries)|
     merge_route_entries(Rest, AllRouteEntries)].

traverse_each_destination(_FromOa, [], _AllRouteEntries) ->
    [];
traverse_each_destination(Oa, [#route_entry{oa = Oa}|Rest],
                          AllRouteEntries) ->
    traverse_each_destination(Oa, Rest, AllRouteEntries);
traverse_each_destination(FromOa, [#route_entry{oa = ToOa, ip = Ip,
                                                  path_cost = Pc}|Rest],
                          AllRouteEntries) ->
    OaTrail = walk_to_destination(ToOa, Ip, AllRouteEntries, []),
    [{FromOa, ToOa, Pc, OaTrail}|
     traverse_each_destination(FromOa, Rest, AllRouteEntries)].

walk_to_destination(ToOa, Ip, AllRouteEntries, Acc) ->
    case lists:keysearch(Ip, 2, AllRouteEntries) of
        false ->
            [];
        {value, {NextOa, Ip, RouteEntries}} ->
            case lists:keysearch(ToOa, 2, RouteEntries) of
                {value, #route_entry{oa = ToOa, path_cost = 0}} ->
                    lists:reverse([NextOa|Acc]);
                {value, #route_entry{oa = ToOa, ip = NextIp}} ->
                    walk_to_destination(ToOa, NextIp, AllRouteEntries,
                                        [NextOa|Acc]);
                false ->
                    []
            end
    end.

%%%
%%% update_path_cost
%%%

update_path_costs(_Oa, _PeerOa, _Pc, []) ->
    unknown_path_cost;
update_path_costs(Oa, PeerOa, NewPc,
                  [{{Oa, PeerOa}, _OldPc}, {{PeerOa, Oa}, _OldPc}|Rest]) ->
    [{{Oa, PeerOa}, NewPc}, {{PeerOa, Oa}, NewPc}|Rest];
update_path_costs(Oa, PeerOa, NewPc,
                  [{{PeerOa, Oa}, _OldPc}, {{Oa, PeerOa}, _OldPc}|Rest]) ->
    [{{PeerOa, Oa}, NewPc}, {{Oa, PeerOa}, NewPc}|Rest];
update_path_costs(Oa, PeerOa, NewPc, [Pc, Cp|Rest]) ->
    [Pc, Cp|update_path_costs(Oa, PeerOa, NewPc, Rest)].

%%%
%%% get_path_cost 
%%%

nudge_path_cost(-1, _Percent) ->
    -1;
nudge_path_cost(Pc, Percent) ->
    random:uniform(Percent)/100*Pc+Pc.

%%%
%%% node lookup functions
%%%

lookup_oa(_Nodes, []) ->
    [];
lookup_oa(Nodes, [Ip|Rest]) ->
    [lookup_oa(Nodes, Ip)|lookup_oa(Nodes, Rest)];
lookup_oa([], Ip) ->
    Ip;
lookup_oa([{Oa, Ip}|_Rest], Ip) ->
    Oa;
lookup_oa([_Node|Rest], Ip) ->
    lookup_oa(Rest, Ip).

lookup_ip([], _Oa) ->
    unknown_oa;
lookup_ip([{Oa, Ip}|_Rest], Oa) ->
    Ip;
lookup_ip([_Node|Rest], Oa) ->
    lookup_ip(Rest, Oa).
