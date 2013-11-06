-module(node_route).

%%% external exports
-export([create_node_db/0, delete_node_db/1, add_node/2, delete_node/2,
         get_nodes/1, foreach_node/2, is_member_node/2, unreachable_nodes/1]).
-export([create_routing_db/0, delete_routing_db/1, get_routing_entries/1,
         foreach_routing_entry/2, update_routing_entry/2]).
-export([recalc/3]).
-export([update_link_quality/3, update_link_qualities/3]).

%%% internal exports

%%% include files
-include_lib("util/include/bits.hrl").
-include_lib("util/include/shorthand.hrl").
-include_lib("util/include/log.hrl").
-include_lib("node/include/node_route.hrl").

%%% constants

%%% records

%%% types
-type foreach_node_fun() :: fun((#node{}) -> any()).
-type foreach_routing_entry_fun() :: fun((#routing_entry{}) -> any()).

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
%%% exported: is_member_node
%%%

-spec is_member_node(node_db(), ip()) -> boolean().

is_member_node(NodeDb, Ip) ->
    ets:member(NodeDb, Ip).

%%%
%%% exported: get_nodes
%%%

-spec get_nodes(node_db()) -> {'ok', [#node{}]}.

get_nodes(NodeDb) ->
    Nodes = ets:foldl(fun(Node, Acc) -> [Node|Acc] end, [], NodeDb),
    {ok, Nodes}.

%%%
%%% exported: foreach_node
%%%

-spec foreach_node(foreach_node_fun(), node_db()) -> any().

foreach_node(Fun, NodeDb) ->
    ets:foldl(fun(Node, []) ->
                      Fun(Node),
                      []
              end, [], NodeDb).

%%%
%%% exported: unreachable_nodes
%%%

unreachable_nodes(NodeDb) ->
    ets:match_object(NodeDb, #node{link_quality = -1, _ = '_'}).

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
%%% exported: foreach_routing_entry
%%%

-spec foreach_routing_entry(foreach_routing_entry_fun(), routing_db()) ->
                                   any().

foreach_routing_entry(Fun, RoutingDb) ->
    ets:foldl(fun(Re, []) ->
                      Fun(Re),
                      []
              end, [], RoutingDb).

%%%
%%% exported: update_routing_entry
%%%

-spec update_routing_entry(routing_db(), #routing_entry{}) ->
                                  {'updated', #routing_entry{}} |
                                  {'kept', #routing_entry{}} |
                                  'got_new' |
                                  {'got_better', #routing_entry{}} |
                                  {'got_worse', #routing_entry{}}.

update_routing_entry(RoutingDb,
                     #routing_entry{oa = ReOa, ip = ReIp, link_quality = ReLq,
                                    flags = ReFlags} = Re) ->
    case ets:lookup(RoutingDb, ReOa) of
        %% existing routing entry got new link quality
        [#routing_entry{ip = ReIp, link_quality = CurrentLq} = CurrentRe]
          when ReLq /= CurrentLq ->
	    UpdatedFlags = ?bit_set(ReFlags, ?F_RE_UPDATED),
	    true =
                ets:insert(RoutingDb, Re#routing_entry{flags = UpdatedFlags}),
            {updated, CurrentRe};
        %% existing routing entry not changed (same link quality as before)
        [#routing_entry{ip = ReIp} = CurrentRe] ->
            {kept, CurrentRe};
        %% new routing entry
        [] ->
            UpdatedFlags = ?bit_set(ReFlags, ?F_RE_UPDATED),
            true =
                ets:insert(RoutingDb, Re#routing_entry{flags = UpdatedFlags}),
            got_new;
        %% better routing entry
        [#routing_entry{link_quality = Lq} = CurrentRe]
          when ReLq /= -1 andalso ReLq < Lq ->
	    UpdatedFlags = ?bit_set(ReFlags, ?F_RE_UPDATED),
	    true =
                ets:insert(RoutingDb, Re#routing_entry{flags = UpdatedFlags}),
            {got_better, CurrentRe};
        %% worse routing entry
        [CurrentRe] ->
            {got_worse, CurrentRe}
    end.

%%%
%%% exported: recalc
%%%

-spec recalc(ip(), node_db(), routing_db()) -> 'ok'.

recalc(Ip, NodeDb, RoutingDb) ->
    touch_routing_entries(NodeDb, RoutingDb),
    propagate_routing_entries(Ip, NodeDb, RoutingDb),
    clear_node_flags(NodeDb),
    clear_routing_entry_flags(RoutingDb),
    true = ets:match_delete(RoutingDb,
                            #routing_entry{link_quality = -1, _ = '_'}),
    ok.

touch_routing_entries(NodeDb, RoutingDb) ->
    foreach_node(
      fun(#node{ip = Ip, flags = Flags})
            when ?bit_is_set(Flags, ?F_NODE_UPDATED) ->
              foreach_routing_entry(
                fun(#routing_entry{ip = ReIp, flags = ReFlags} = Re)
                      when ReIp == Ip ->
                        UpdatedReFlags = ?bit_set(ReFlags, ?F_RE_UPDATED),
                        true = ets:insert(RoutingDb,
                                          Re#routing_entry{
                                            flags = UpdatedReFlags});
                   (_) ->
                        ok
                end, RoutingDb);
         (_) ->
              ok
      end, NodeDb).

propagate_routing_entries(Ip, NodeDb, RoutingDb) ->
    foreach_node(
      fun(Node) ->
              update_routing_entries(
                Ip, NodeDb, RoutingDb, ets:first(RoutingDb), Node)
      end, NodeDb).

update_routing_entries(_Ip, _NodeDb, _RoutingDb, '$end_of_table', _Node) ->
    ok;
update_routing_entries(Ip, NodeDb, RoutingDb, Oa,
                       #node{ip = PeerIp,
                             link_quality = PeerLq,
                             flags = PeerFlags} = Node) ->
    [#routing_entry{ip = ReIp,
                    link_quality = ReLq,
                    flags = ReFlags,
                    hops = Hops} = Re] =
        ets:lookup(RoutingDb, Oa),
    if
        PeerIp /= ReIp andalso
        PeerLq /= undefined andalso
        PeerLq /= -1 andalso
        (?bit_is_set(PeerFlags, ?F_NODE_UPDATED) orelse
         ?bit_is_set(ReFlags, ?F_RE_UPDATED)) ->
            if
                PeerLq == -1 ->
                    UpdatedLq = -1;
                true ->
                    UpdatedLq = ReLq+PeerLq
            end,
            UpdatedRe =
                Re#routing_entry{
                  ip = Ip,
                  link_quality = UpdatedLq,
                  hops = [Ip|Hops]},
            ok = node_serv:update_routing_entry(PeerIp, UpdatedRe),
            update_routing_entries(Ip, NodeDb, RoutingDb,
                                   ets:next(RoutingDb, Oa), Node);
        true ->
            update_routing_entries(Ip, NodeDb, RoutingDb,
                                   ets:next(RoutingDb, Oa), Node)
    end.

clear_node_flags(NodeDb) ->
    foreach_node(
      fun(#node{link_quality = undefined}) ->
              ok;
         (#node{flags = Flags} = Node)
            when ?bit_is_set(Flags, ?F_NODE_UPDATED) ->
              UpdatedFlags = ?bit_clr(Flags, ?F_NODE_UPDATED),
              true = ets:insert(NodeDb, Node#node{flags = UpdatedFlags});
         (_) ->
              ok
      end, NodeDb).

clear_routing_entry_flags(RoutingDb) ->
    foreach_routing_entry(
      fun(#routing_entry{flags = Flags} = Re)
            when ?bit_is_set(Flags, ?F_RE_UPDATED) ->
              UpdatedFlags = ?bit_clr(Flags, ?F_RE_UPDATED),
              true = ets:insert(RoutingDb,
                                Re#routing_entry{flags = UpdatedFlags});
         (_) ->
              ok
      end, RoutingDb).

%%%
%%% exported: update_link_quality
%%%

-spec update_link_quality(node_db(), ip(), link_quality()) -> ok.

update_link_quality(NodeDb, PeerIp, UpdatedLq) ->
    case ets:lookup(NodeDb, PeerIp) of
        [#node{link_quality = Lq} = Node]
          when Lq /= UpdatedLq ->
            UpdatedFlags = ?bit_set(Node#node.flags, ?F_NODE_UPDATED),
	    true = ets:insert(NodeDb,
                              Node#node{link_quality = UpdatedLq,
                                        flags = UpdatedFlags}),
            ok;
        _ ->
            ok
    end.

%%%
%%% exported: update_link_qualities
%%%

-spec update_link_qualities(routing_db(), ip(), link_quality()) -> ok.

update_link_qualities(RoutingDb, PeerIp, UpdatedLq) ->
    update_link_qualities(RoutingDb, ets:first(RoutingDb), PeerIp, UpdatedLq).

update_link_qualities(_RoutingDb, '$end_of_table', _PeerIp, _UpdatedLq) ->
    ok;
update_link_qualities(RoutingDb, Oa, PeerIp, UpdatedLq) ->
    case ets:lookup(RoutingDb, Oa) of
        [#routing_entry{ip = PeerIp} = Re] ->
            UpdatedFlags = ?bit_set(Re#routing_entry.flags, ?F_RE_UPDATED),
	    true = ets:insert(RoutingDb,
                              Re#routing_entry{link_quality = UpdatedLq,
                                               flags = UpdatedFlags}),
            update_link_qualities(RoutingDb, ets:next(RoutingDb, Oa),
                                  PeerIp, UpdatedLq);
        _ ->
            update_link_qualities(RoutingDb, ets:next(RoutingDb, Oa),
                                  PeerIp, UpdatedLq)
    end.
