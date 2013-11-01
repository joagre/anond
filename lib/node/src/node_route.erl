-module(node_route).

%%% external exports
-export([create_node_db/0, delete_node_db/1, add_node/2, delete_node/2,
         member_node/2, get_nodes/1]).
-export([create_routing_db/0, delete_routing_db/1, get_routing_entries/1,
         update_routing_entry/2, delete_routing_entry/2]).
-export([recalc/3]).
-export([update_link_quality/3]).

%%% internal exports

%%% include files
-include_lib("util/include/bits.hrl").
-include_lib("node/include/node_route.hrl").

%%% constants

%%% records

%%% types

%%%
%%% exported: create_node_db
%%%

-spec create_node_db() -> {'ok', node_db()}.

create_node_db() ->
    {ok, ets:new(node_db, [{keypos, 2}])}.

%%%
%%% exported: delete_node_db
%%%
	
-spec delete_node_db(node_db()) -> 'ok'.

delete_node_db(NodeDb) ->
    true = ets:delete(NodeDb),
    ok.

%%%
%%% exported: add_node
%%%

-spec add_node(node_db(), #node{}) -> 'ok'.

add_node(NodeDb, Node) ->
    true = ets:insert(NodeDb, Node),
    ok.

%%%
%%% exported: delete_node
%%%

-spec delete_node(node_db(), ip()) -> 'ok'.

delete_node(NodeDb, Ip) ->
    true = ets:delete(NodeDb, Ip),
    ok.

%%%
%%% exported: member_node
%%%

-spec member_node(node_db(), ip()) -> boolean().

member_node(NodeDb, Ip) ->
    ets:member(NodeDb, Ip).

%%%
%%% exported: get_nodes
%%%

-spec get_nodes(node_db()) -> {'ok', [#node{}]}.

get_nodes(NodeDb) ->
    Nodes = ets:foldl(fun(Node, Acc) -> [Node|Acc] end, [], NodeDb),
    {ok, Nodes}.

%%%
%%% exported: create_routing_db
%%%

-spec create_routing_db() -> {'ok', routing_db()}.

create_routing_db() ->
    {ok, ets:new(routing_db, [{keypos, 2}])}.

%%%
%%% exported: delete_routing_db
%%%
	
-spec delete_routing_db(routing_db()) -> 'ok'.

delete_routing_db(RoutingDb) ->
    true = ets:delete(RoutingDb),
    ok.

%%%
%%% exported: get_routing_entries
%%%

-spec get_routing_entries(routing_db()) -> {'ok', [#routing_entry{}]}.

get_routing_entries(RoutingDb) ->
    Res = ets:foldl(fun(Re, Acc) -> [Re|Acc] end, [], RoutingDb),
    {ok, Res}.

%%%
%%% exported: update_routing_entry
%%%

-spec update_routing_entry(routing_db(), #routing_entry{}) ->
                                  {'replaced', #routing_entry{}} |
                                  'new' |
                                  {'kept', #routing_entry{}}.

update_routing_entry(RoutingDb, Re) ->
    case ets:lookup(RoutingDb, Re#routing_entry.oa) of
        [#routing_entry{link_quality = LinkQuality} = CurrentRe]
          when Re#routing_entry.link_quality < LinkQuality ->
	    UpdatedFlags = ?bit_set(Re#routing_entry.flags, ?F_RE_UPDATED),
	    true =
                ets:insert(RoutingDb, Re#routing_entry{flags = UpdatedFlags}),
            {replaced, CurrentRe};
        [] ->
            UpdatedFlags = ?bit_set(Re#routing_entry.flags, ?F_RE_UPDATED),
	    true =
                ets:insert(RoutingDb, Re#routing_entry{flags = UpdatedFlags}),
            new;
        [CurrentRe] ->            
            {kept, CurrentRe}
    end.

%%%
%%% exported: delete_routing_entry
%%%

-spec delete_routing_entry(routing_db(), {'ip', ip()} | oa()) -> 'ok'.

delete_routing_entry(RoutingDb, {ip, Ip}) ->
    true = ets:match_delete(RoutingDb, #routing_entry{ip = Ip, _ = '_'}),
    ok;
delete_routing_entry(RoutingDb, Oa) ->
    true = ets:delete(RoutingDb, Oa),
    ok.

%%%
%%% exported: recalc
%%%

-spec recalc(ip(), node_db(), routing_db()) -> 'ok'.

recalc(Ip, NodeDb, RoutingDb) ->
    traverse_peers(Ip, NodeDb, ets:first(NodeDb), RoutingDb),
    clear_node_flags(NodeDb),
    clear_routing_entry_flags(RoutingDb).

traverse_peers(_Ip, _NodeDb, '$end_of_table', _RoutingDb) ->
    ok;
traverse_peers(Ip, NodeDb, PeerIp, RoutingDb) ->
    [Node] = ets:lookup(NodeDb, PeerIp),
    update_routing_entries(Ip, NodeDb, RoutingDb, ets:first(RoutingDb), Node),
    traverse_peers(Ip, NodeDb, ets:next(NodeDb, PeerIp), RoutingDb).

update_routing_entries(_Ip, _NodeDb, _RoutingDb, '$end_of_table', _Node) ->
    ok;
update_routing_entries(Ip, NodeDb, RoutingDb, Oa,
                       #node{ip = PeerIp,
                             link_quality = PeerLinkQuality,
                             flags = PeerFlags} = Node) ->
    [#routing_entry{ip = ReIp,
                    link_quality = ReLinkQuality,
                    flags = ReFlags,
                    hops = Hops} = Re] =
        ets:lookup(RoutingDb, Oa),
    if
        PeerIp /= ReIp andalso
        (?bit_is_set(PeerFlags, ?F_NODE_UPDATED) orelse
         ?bit_is_set(ReFlags, ?F_RE_UPDATED)) andalso
        PeerLinkQuality /= undefined andalso
        PeerLinkQuality /= -1 ->
            UpdatedRe =
                Re#routing_entry{
                  ip = Ip,
                  link_quality = ReLinkQuality+PeerLinkQuality,
                  hops = [Ip|Hops]},
            ok = node_serv:update_routing_entry(PeerIp, UpdatedRe),
            update_routing_entries(Ip, NodeDb, RoutingDb,
                                   ets:next(RoutingDb, Oa), Node);
        true ->
            update_routing_entries(Ip, NodeDb, RoutingDb,
                                   ets:next(RoutingDb, Oa), Node)
    end.

clear_node_flags(NodeDb) ->
    clear_node_flags(NodeDb, ets:first(NodeDb)).

clear_node_flags(_NodeDb, '$end_of_table') ->
    ok;
clear_node_flags(NodeDb, Ip) ->
    case ets:lookup(NodeDb, Ip) of
        [#node{flags = Flags} = Node]
          when ?bit_is_set(Flags, ?F_NODE_UPDATED) ->
            UpdatedFlags = ?bit_clr(Flags, ?F_NODE_UPDATED),
            true = ets:insert(NodeDb, Node#node{flags = UpdatedFlags}),
            clear_node_flags(NodeDb, ets:next(NodeDb, Ip));
        _ ->
            clear_node_flags(NodeDb, ets:next(NodeDb, Ip))
    end.

clear_routing_entry_flags(RoutingDb) ->
    clear_routing_entry_flags(RoutingDb, ets:first(RoutingDb)).

clear_routing_entry_flags(_RoutingDb, '$end_of_table') ->
    ok;
clear_routing_entry_flags(RoutingDb, Oa) ->
    case ets:lookup(RoutingDb, Oa) of
        [#routing_entry{flags = Flags} = Re]
          when ?bit_is_set(Flags, ?F_RE_UPDATED) ->
            UpdatedFlags = ?bit_clr(Flags, ?F_RE_UPDATED),
            true = ets:insert(RoutingDb,
                              Re#routing_entry{flags = UpdatedFlags}),
            clear_routing_entry_flags(RoutingDb, ets:next(RoutingDb, Oa));
        _ ->
            clear_routing_entry_flags(RoutingDb, ets:next(RoutingDb, Oa))
    end.

%%%
%%% exported: update_link_quality
%%%

-spec update_link_quality(node_db(), ip(), link_quality()) -> ok.

update_link_quality(NodeDb, PeerIp, UpdatedLinkQuality) ->
    case ets:lookup(NodeDb, PeerIp) of
        [#node{link_quality = LinkQuality} = Node]
          when LinkQuality /= UpdatedLinkQuality ->
	    UpdatedFlags = ?bit_set(Node#node.flags, ?F_NODE_UPDATED),
	    true = ets:insert(NodeDb,
                              Node#node{link_quality = UpdatedLinkQuality,
                                        flags = UpdatedFlags}),
            ok;
        _ ->
            ok
    end.
