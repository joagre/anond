-module(overseer_serv).

%%% external exports
-export([start_link/0, stop/0, stop/1]).
-export([get_global_routing_table/0, get_routing_table/1]).
-export([get_neighbours/0, get_neighbours/1]).
-export([enable_recalc/0, enable_recalc/1, disable_recalc/0, disable_recalc/1]).
-export([recalc/0, recalc/1]).
-export([get_link_quality/2, update_link_quality/3]).

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
-define(MAX_LINK_QUALITY, 100).

%%% records
-record(state, {
	  parent          :: pid(),
	  link_qualities  :: [{{oa(), oa()}, link_quality()}],
	  nodes           :: [{oa(), ip()}],
          simulation      :: boolean(),
          number_of_nodes :: integer()
	 }).

%%% types
-type global_routing_table() :: [{oa(), {oa(), link_quality(), [oa()]}}].
-type routing_table() :: [{oa(), link_quality(), [oa()]}].
-type neighbours() :: [{oa(), link_quality()}].

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
%%% exported: get_global_routing_table
%%%

-spec get_global_routing_table() -> global_routing_table().

get_global_routing_table() ->
    serv:call(?MODULE, get_global_routing_table).

%%%
%%% exported: get_routing_table
%%%

-spec get_routing_table(oa()) -> {'ok', routing_table()} | 'unknown_oa'.

get_routing_table(Oa) ->
    serv:call(?MODULE, {get_routing_table, Oa}).

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
%%% exported: get_link_quality
%%%

-spec get_link_quality(ip(), ip()) -> {'ok', link_quality()}.

get_link_quality(Ip, PeerIp) ->
    serv:call(?MODULE, {get_link_quality, Ip, PeerIp}).

%%%
%%% exported: update_link_quality
%%%

-spec update_link_quality(oa(), oa(), link_quality()) ->
                                 'ok' | 'unknown_link_quality'.

update_link_quality(Oa, PeerOa, Lq) ->
    serv:call(?MODULE, {update_link_quality, Oa, PeerOa, Lq}).

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
                         link_qualities = ?NON_RANDOM_LINK_QUALITIES,
                         nodes = Nodes});
        _ ->
            Parent ! {self(), already_started}
    end.

loop(#state{parent = Parent,
	    link_qualities = Lqs,
	    nodes = Nodes,
            simulation = Simulation} = S) ->
    receive
        config_updated ->
            loop(read_config(S));
	{From, stop} ->
	    From ! {self(), ok};
	{From, get_global_routing_table} ->
	    From ! {self(), get_global_routing_table(Nodes)},
	    loop(S);
	{From, {get_routing_table, Oa}} ->
            case lookup_ip(Nodes, Oa) of
                unknown_oa ->
                    From ! {self(), unknown_oa},
                    loop(S);
                Ip ->
                    {ok, Res} = node_serv:get_routing_entries(Ip),
                    RoutingTable =
                        [{ReOa, Lq, lookup_oa(Nodes, Hops)} ||
                            #routing_entry{oa = ReOa,
                                           link_quality = Lq,
                                           hops = Hops} <- Res],
                    From ! {self(), {ok, RoutingTable}},
                    loop(S)
            end;
	{From, get_neighbours} ->
            AllNeighbours =
                lists:map(
                  fun({Oa, Ip}) ->
                          {ok, ActualNodes} = node_serv:get_nodes(Ip),
                          {Oa, [{lookup_oa(Nodes, ActualNode#node.ip),
                                 ActualNode#node.link_quality} ||
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
                          ActualNode#node.link_quality} ||
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
        {From, {get_link_quality, Ip, PeerIp}} when Simulation == true ->
            Oa = lookup_oa(Nodes, Ip),
            PeerOa = lookup_oa(Nodes, PeerIp),
            {value, {_, Lq}} = lists:keysearch({Oa, PeerOa}, 1, Lqs),
            From ! {self(), {ok, Lq}},
            loop(S);
        {From, {get_link_quality, Ip, PeerIp}} ->
            Oa = lookup_oa(Nodes, Ip),
            PeerOa = lookup_oa(Nodes, PeerIp),
            case lists:keysearch({Oa, PeerOa}, 1, Lqs) of
                {value, {_, Lq}} ->
                    From ! {self(), {ok, Lq}},
                    loop(S);
                false ->
                    RandomLq = random:uniform(?MAX_LINK_QUALITY),
                    From ! {self(), {ok, RandomLq}},
                    UpdatedLqs = [{{Oa, PeerOa}, RandomLq},
                                  {{PeerOa, Oa}, RandomLq}|Lqs],
                    loop(S#state{link_qualities = UpdatedLqs})
            end;
	{From, {update_link_quality, Oa, PeerOa, Lq}} ->
            case update_link_qualities(Oa, PeerOa, Lq, Lqs) of
                unknown_link_quality ->
                    From ! {self(), unknown_link_quality},
                    loop(S);
                UpdatedLqs ->
                    From ! {self(), ok},
                    loop(S#state{link_qualities = UpdatedLqs})
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
start_nodes(Oa) ->
    {ok, Ip} = node_serv:start_link(Oa, undefined, false),
    [{Oa, Ip}|start_nodes(Oa-1)].

%%
%% get_global_routing_table
%%

get_global_routing_table(Nodes) ->
    AllRoutingEntries = get_all_routing_entries(Nodes),
    merge_routing_entries(AllRoutingEntries, AllRoutingEntries).

get_all_routing_entries([]) ->
    [];
get_all_routing_entries([{Oa, Ip}|Rest]) ->
    {ok, RoutingEntries} = node_serv:get_routing_entries(Ip),
    [{Oa, Ip, RoutingEntries}|get_all_routing_entries(Rest)].

merge_routing_entries([], _AllRoutingEntries) ->
    [];
merge_routing_entries([{Oa, _Ip, RoutingEntries}|Rest], AllRoutingEntries) ->
    [traverse_each_destination(Oa, RoutingEntries, AllRoutingEntries)|
     merge_routing_entries(Rest, AllRoutingEntries)].

traverse_each_destination(_FromOa, [], _AllRoutingEntries) ->
    [];
traverse_each_destination(Oa, [#routing_entry{oa = Oa}|Rest],
                          AllRoutingEntries) ->
    traverse_each_destination(Oa, Rest, AllRoutingEntries);
traverse_each_destination(FromOa, [#routing_entry{oa = ToOa, ip = Ip,
                                                  link_quality = Lq}|Rest],
                          AllRoutingEntries) ->
    OaTrail = walk_to_destination(ToOa, Ip, AllRoutingEntries, []),
    [{FromOa, ToOa, Lq, OaTrail}|
     traverse_each_destination(FromOa, Rest, AllRoutingEntries)].

walk_to_destination(ToOa, Ip, AllRoutingEntries, Acc) ->
    case lists:keysearch(Ip, 2, AllRoutingEntries) of
        false ->
            [];
        {value, {NextOa, Ip, RoutingEntries}} ->
            case lists:keysearch(ToOa, 2, RoutingEntries) of
                {value, #routing_entry{oa = ToOa, link_quality = 0}} ->
                    lists:reverse([NextOa|Acc]);
                {value, #routing_entry{oa = ToOa, ip = NextIp}} ->
                    walk_to_destination(ToOa, NextIp, AllRoutingEntries,
                                        [NextOa|Acc]);
                false ->
                    []
            end
    end.

%%
%% update_link_quality
%%

update_link_qualities(_Oa, _PeerOa, _Lq, []) ->
    unknown_link_quality;
update_link_qualities(Oa, PeerOa, NewLq,
                      [{{Oa, PeerOa}, _OldLq}, {{PeerOa, Oa}, _OldLq}|Rest]) ->
    [{{Oa, PeerOa}, NewLq}, {{PeerOa, Oa}, NewLq}|Rest];
update_link_qualities(Oa, PeerOa, NewLq,
                      [{{PeerOa, Oa}, _OldLq}, {{Oa, PeerOa}, _OldLq}|Rest]) ->
    [{{PeerOa, Oa}, NewLq}, {{Oa, PeerOa}, NewLq}|Rest];
update_link_qualities(Oa, PeerOa, NewLq, [_, _|Rest]) ->
    update_link_qualities(Oa, PeerOa, NewLq, Rest).
    


%%
%% node lookup functions
%%

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
