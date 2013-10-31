-module(overseer_serv).

%%% external exports
-export([start_link/0, stop/0, stop/1]).
-export([get_global_routing_table/0, get_routing_entries/1]).
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

%%% records
-record(state, {
	  parent          :: pid(),
	  link_qualities :: [{{oa(), oa()}, link_quality()}],
	  nodes           :: [{oa(), ip()}]
	 }).

%%% types
-type global_routing_table() :: [{oa(), oa(), [oa()]}].

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
%%% exported: get_routing_entries
%%%

-spec get_routing_entries(oa()) -> {'ok', [#routing_entry{}]} | 'unknown_oa'.

get_routing_entries(Oa) ->
    serv:call(?MODULE, {get_routing_entries, Oa}).

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

-spec update_link_quality(oa(), oa(), link_quality()) -> 'ok'.

update_link_quality(Oa, PeerOa, LinkQuality) ->
    ?MODULE ! {update_link_quality, Oa, PeerOa, LinkQuality},
    ok.

%%%
%%% server loop
%%%

init(Parent) ->
    process_flag(trap_exit, true),
    case catch register(?MODULE, self()) of
        true ->
            S = read_config(#state{}),
            ok = config_serv:subscribe(),
            Nodes = start_nodes(?NUMBER_OF_SIMULATION_NODES),
	    Parent ! {self(), started},
	    loop(S#state{parent = Parent,
                         link_qualities = ?NON_RANDOM_LINK_QUALITIES,
                         nodes = Nodes});
        _ ->
            Parent ! {self(), already_started}
    end.

loop(#state{parent = Parent,
	    link_qualities = LinkQualities,
	    nodes = Nodes} = S) ->
    receive
        config_updated ->
            loop(read_config(S));
	{From, stop} ->
	    From ! {self(), ok};
	{From, get_global_routing_table} ->
	    From ! {self(), get_global_routing_table(Nodes)},
	    loop(S);
	{From, {get_routing_entries, Oa}} ->
            case lists:keysearch(Oa, 1, Nodes) of
                {value, {Oa, Ip}} ->
                    {ok, RoutingEntries} = node_serv:get_routing_entries(Ip),
                    From ! {self(),
                            {ok, prettify_routing_entries(
                                   Nodes, RoutingEntries)}},
                    loop(S);
                false ->
                    From ! {self(), unknown_oa},
                    loop(S)
            end;
	{From, enable_recalc} ->
            lists:foreach(fun({_Oa, Ip}) ->
                                  ok = node_serv:enable_recalc(Ip)
                          end, Nodes),
            From ! {self(), ok},
            loop(S);
        {From, {enable_recalc, Oa}} ->
            case lists:keysearch(Oa, 1, Nodes) of
                {value, {Oa, Ip}} ->
                    From ! {self(), node_serv:enable_recalc(Ip)},
                    loop(S);
                _ ->
                    From ! {self(), unknown_oa},
                    loop(S)
            end;
	{From, disable_recalc} ->
            lists:foreach(fun({_Oa, Ip}) ->
                                  ok = node_serv:disable_recalc(Ip)
                          end, Nodes),
            From ! {self(), ok},
            loop(S);
        {From, {disable_recalc, Oa}} ->
            case lists:keysearch(Oa, 1, Nodes) of
                {value, {Oa, Ip}} ->
                    From ! {self(), node_serv:disable_recalc(Ip)},
                    loop(S);
                _ ->
                    From ! {self(), unknown_oa},
                    loop(S)
            end;
	{From, recalc} ->
            lists:foreach(fun({_Oa, Ip}) ->
                                  ok = node_serv:recalc(Ip)
                          end, Nodes),
            From ! {self(), ok},
            loop(S);
        {From, {recalc, Oa}} ->
            case lists:keysearch(Oa, 1, Nodes) of
                {value, {Oa, Ip}} ->
                    From ! {self(), node_serv:recalc(Ip)},
                    loop(S);
                _ ->
                    From ! {self(), unknown_oa},
                    loop(S)
            end;
        {From, {get_link_quality, Ip, PeerIp}} ->
            Oa = lookup_oa(Nodes, Ip),
            PeerOa = lookup_oa(Nodes, PeerIp),
            {value, {_, LinkQuality}} =
                lists:keysearch({Oa, PeerOa}, 1, LinkQualities),
            From ! {self(), {ok, LinkQuality}},
            loop(S);
	{update_link_quality, Oa, PeerOa, LinkQuality} ->
            UpdatedLinkQualities0 =
                lists:keyreplace({Oa, PeerOa}, 1, LinkQualities,
                                 {{Oa, PeerOa}, LinkQuality}),
            UpdatedLinkQualities =
                lists:keyreplace({PeerOa, Oa}, 1, UpdatedLinkQualities0,
                                 {{PeerOa, Oa}, LinkQuality}),
            loop(S#state{link_qualities = UpdatedLinkQualities});
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
traverse_each_destination(FromOa, [#routing_entry{oa = ToOa, ip = Ip}|Rest],
                          AllRoutingEntries) ->
    OaTrail = walk_to_destination(ToOa, Ip, AllRoutingEntries, []),
    [{FromOa, ToOa, OaTrail}|
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

%%%
%%% get_routing_entries
%%%

prettify_routing_entries(_Nodes, []) ->
    [];
prettify_routing_entries(Nodes,
                         [#routing_entry{ip = Ip, hops = Hops} = Re|Rest]) ->
    [Re#routing_entry{ip = lookup_oa(Nodes, Ip),
                      hops = [lookup_oa(Nodes, HopIp) || HopIp <- Hops]}|
     prettify_routing_entries(Nodes, Rest)].

lookup_oa([], Ip) ->
    Ip;
lookup_oa([{Oa, Ip}|_Rest], Ip) ->
    Oa;
lookup_oa([_Node|Rest], Ip) ->
    lookup_oa(Rest, Ip).
