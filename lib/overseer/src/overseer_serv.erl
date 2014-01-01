-module(overseer_serv).

%%% external exports
-export([start_link/0, stop/0, stop/1]).
-export([get_global_route_table/0, get_route_table/1]).
-export([get_neighbours/0, get_neighbours/1]).
-export([enable_recalc/0, enable_recalc/1, disable_recalc/0, disable_recalc/1]).
-export([recalc/0, recalc/1]).

%%% internal exports
-export([init/1]).

%%% include files
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").
-include_lib("util/include/config.hrl").
-include_lib("node/include/node.hrl").
-include_lib("node/include/node_route.hrl").
-include_lib("ds/include/ds.hrl").

%%% constants
-define(BOOTSTRAP_TIMEOUT, 3*1000).

%%% records
-record(state, {
	  parent          :: pid(),
	  nodes = []      :: [{oa(), na()}]
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
%%% server loop
%%%

init(Parent) ->
    process_flag(trap_exit, true),
    case catch register(?MODULE, self()) of
        true ->
            S = read_config(#state{}),
            ok = config_json_serv:subscribe(),
            timelib:start_timer(?BOOTSTRAP_TIMEOUT, get_all_published_nodes),
	    Parent ! {self(), started},
	    loop(S#state{parent = Parent});
        _ ->
            Parent ! {self(), already_started}
    end.

loop(#state{parent = Parent, nodes = Nodes} = S) ->
    receive
        get_all_published_nodes ->
            UpdatedNodes = get_all_published_nodes(),
            loop(S#state{nodes = UpdatedNodes});
        config_updated ->
            loop(read_config(S));
	{From, stop} ->
	    From ! {self(), ok};
	{From, get_global_route_table} ->
	    From ! {self(), get_global_route_table(Nodes)},
	    loop(S);
	{From, {get_route_table, Oa}} ->
            case lookup_na(Nodes, Oa) of
                unknown_oa ->
                    From ! {self(), unknown_oa},
                    loop(S);
                Na ->
                    {ok, Res} = node_route_serv:get_route_entries(Na),
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
                  fun({Oa, Na}) ->
                          {ok, ActualNodes} = node_route_serv:get_nodes(Na),
                          {Oa, [{lookup_oa(Nodes, ActualNode#node.na),
                                 ActualNode#node.path_cost} ||
                                   ActualNode <- ActualNodes]}
                  end, Nodes),
            From ! {self(), {ok, AllNeighbours}},
            loop(S);
	{From, {get_neighbours, Oa}} ->
            case lookup_na(Nodes, Oa) of
                unknown_oa ->
                    From ! {self(), unknown_oa},
                    loop(S);
                Na ->
                    {ok, ActualNodes} = node_route_serv:get_nodes(Na),
                    Neighbours =
                        [{lookup_oa(Nodes, ActualNode#node.na),
                          ActualNode#node.path_cost} ||
                            ActualNode <- ActualNodes],
                    From ! {self(), {ok, Neighbours}},
                    loop(S)
            end;
	{From, enable_recalc} ->
            lists:foreach(fun({_Oa, Na}) ->
                                  ok = node_route_serv:enable_recalc(Na)
                          end, Nodes),
            From ! {self(), ok},
            loop(S);
        {From, {enable_recalc, Oa}} ->
            case lookup_na(Nodes, Oa) of
                unknown_oa ->
                    From ! {self(), unknown_oa},
                    loop(S);
                Na ->
                    From ! {self(), node_route_serv:enable_recalc(Na)},
                    loop(S)
            end;
	{From, disable_recalc} ->
            lists:foreach(fun({_Oa, Na}) ->
                                  ok = node_route_serv:disable_recalc(Na)
                          end, Nodes),
            From ! {self(), ok},
            loop(S);
        {From, {disable_recalc, Oa}} ->
            case lookup_na(Nodes, Oa) of
                unknown_oa ->
                    From ! {self(), unknown_oa},
                    loop(S);
                Na ->
                    From ! {self(), node_route_serv:disable_recalc(Na)},
                    loop(S)
            end;
	{From, recalc} ->
            lists:foreach(fun({_Oa, Na}) ->
                                  ok = node_route_serv:recalc(Na)
                          end, Nodes),
            From ! {self(), ok},
            loop(S);
        {From, {recalc, Oa}} ->
            case lookup_na(Nodes, Oa) of
                unknown_oa ->
                    From ! {self(), unknown_oa},
                    loop(S);
                Na ->
                    From ! {self(), node_route_serv:recalc(Na)},
                    loop(S)
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
    S.

get_all_published_nodes() ->
    {ok, Peers} = ds_serv:get_all_peers(),
    get_all_published_nodes(Peers).

get_all_published_nodes([]) ->
    [];
get_all_published_nodes([#peer{na = Na}|Rest]) ->
    {ok, [Oa]} = ds_serv:reserved_oas(Na),
    [{Oa, Na}|get_all_published_nodes(Rest)].

%%%
%%% get_global_route_table
%%%

get_global_route_table(Nodes) ->
    AllRouteEntries = get_all_route_entries(Nodes),
    merge_route_entries(AllRouteEntries, AllRouteEntries).

get_all_route_entries([]) ->
    [];
get_all_route_entries([{Oa, Na}|Rest]) ->
    {ok, RouteEntries} = node_route_serv:get_route_entries(Na),
    [{Oa, Na, RouteEntries}|get_all_route_entries(Rest)].

merge_route_entries([], _AllRouteEntries) ->
    [];
merge_route_entries([{Oa, _Na, RouteEntries}|Rest], AllRouteEntries) ->
    [traverse_each_destination(Oa, RouteEntries, AllRouteEntries)|
     merge_route_entries(Rest, AllRouteEntries)].

traverse_each_destination(_FromOa, [], _AllRouteEntries) ->
    [];
traverse_each_destination(Oa, [#route_entry{oa = Oa}|Rest],
                          AllRouteEntries) ->
    traverse_each_destination(Oa, Rest, AllRouteEntries);
traverse_each_destination(FromOa, [#route_entry{oa = ToOa, na = Na,
                                                  path_cost = Pc}|Rest],
                          AllRouteEntries) ->
    OaTrail = walk_to_destination(ToOa, Na, AllRouteEntries, []),
    [{FromOa, ToOa, Pc, OaTrail}|
     traverse_each_destination(FromOa, Rest, AllRouteEntries)].

walk_to_destination(ToOa, Na, AllRouteEntries, Acc) ->
    case lists:keysearch(Na, 2, AllRouteEntries) of
        false ->
            [];
        {value, {NextOa, Na, RouteEntries}} ->
            case lists:keysearch(ToOa, 2, RouteEntries) of
                {value, #route_entry{oa = ToOa, path_cost = 0}} ->
                    lists:reverse([NextOa|Acc]);
                {value, #route_entry{oa = ToOa, na = NextNa}} ->
                    walk_to_destination(ToOa, NextNa, AllRouteEntries,
                                        [NextOa|Acc]);
                false ->
                    []
            end
    end.

%%%
%%% node lookup functions
%%%

lookup_oa(_Nodes, []) ->
    [];
lookup_oa(Nodes, [Na|Rest]) ->
    [lookup_oa(Nodes, Na)|lookup_oa(Nodes, Rest)];
lookup_oa([], Na) ->
    Na;
lookup_oa([{Oa, Na}|_Rest], Na) ->
    Oa;
lookup_oa([_Node|Rest], Na) ->
    lookup_oa(Rest, Na).

lookup_na([], _Oa) ->
    unknown_oa;
lookup_na([{Oa, Na}|_Rest], Oa) ->
    Na;
lookup_na([_Node|Rest], Oa) ->
    lookup_na(Rest, Oa).
