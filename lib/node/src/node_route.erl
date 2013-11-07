-module(node_route).

%%% external exports
-export([create_node_db/0, delete_node_db/1, add_node/2, delete_node/2,
         get_nodes/1, foreach_node/2, is_member_node/2, unreachable_nodes/1]).
-export([create_routing_db/0, delete_routing_db/1, get_routing_entries/1,
         foreach_routing_entry/2, update_routing_entry/2]).
-export([recalc/3]).
-export([update_path_cost/3, update_path_costs/3]).

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
    ets:match_object(NodeDb, #node{path_cost = -1, _ = '_'}).

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
                     #routing_entry{oa = ReOa, ip = ReIp, path_cost = RePc,
                                    flags = ReFlags} = Re) ->
    case ets:lookup(RoutingDb, ReOa) of
        %% existing routing entry got new path cost
        [#routing_entry{ip = ReIp, path_cost = CurrentPc} = CurrentRe]
          when RePc /= CurrentPc ->
	    UpdatedFlags = ?bit_set(ReFlags, ?F_RE_UPDATED),
	    true =
                ets:insert(RoutingDb, Re#routing_entry{flags = UpdatedFlags}),
            {updated, CurrentRe};
        %% existing routing entry not changed (same path cost as before)
        [#routing_entry{ip = ReIp} = CurrentRe] ->
            {kept, CurrentRe};
        %% new routing entry
        [] ->
            UpdatedFlags = ?bit_set(ReFlags, ?F_RE_UPDATED),
            true =
                ets:insert(RoutingDb, Re#routing_entry{flags = UpdatedFlags}),
            got_new;
        %% better routing entry
        [#routing_entry{path_cost = Pc} = CurrentRe]
          when RePc /= -1 andalso RePc < Pc ->
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
    true = ets:match_delete(RoutingDb, #routing_entry{path_cost = -1, _ = '_'}),
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
              send_routing_entries(Ip, RoutingDb, Node)
      end, NodeDb).

send_routing_entries(Ip, RoutingDb, #node{ip = PeerIp,
                                          path_cost = PeerPc,
                                          flags = PeerFlags}) ->
    foreach_routing_entry(
      fun(#routing_entry{ip = ReIp,
                         path_cost = RePc,
                         flags = ReFlags,
                         hops = Hops} = Re) ->
              if
                  PeerIp /= ReIp andalso
                  PeerPc /= undefined andalso
                  (?bit_is_set(PeerFlags, ?F_NODE_UPDATED) orelse
                   ?bit_is_set(ReFlags, ?F_RE_UPDATED)) ->
                      if
                          PeerPc == -1 orelse RePc == -1 ->
                              UpdatedPc = -1;
                          true ->
                              UpdatedPc = RePc+PeerPc
                      end,
                      UpdatedRe =
                          Re#routing_entry{
                            ip = Ip,
                            path_cost = UpdatedPc,
                            hops = [Ip|Hops]
                            %% patrik: increment psp?
                            %%psp = ...
                           },
                      ok = node_serv:send_routing_entry(PeerIp, UpdatedRe);
                  true ->
                      ok
              end
      end, RoutingDb).

clear_node_flags(NodeDb) ->
    foreach_node(
      fun(#node{path_cost = undefined}) ->
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
%%% exported: update_path_cost
%%%

-spec update_path_cost(node_db(), ip(), path_cost()) -> ok.

update_path_cost(NodeDb, PeerIp, UpdatedPc) ->
    case ets:lookup(NodeDb, PeerIp) of
        [#node{path_cost = Pc} = Node]
          when Pc /= UpdatedPc ->
            UpdatedFlags = ?bit_set(Node#node.flags, ?F_NODE_UPDATED),
	    true = ets:insert(NodeDb,
                              Node#node{path_cost = UpdatedPc,
                                        flags = UpdatedFlags}),
            ok;
        _ ->
            ok
    end.

%%%
%%% exported: update_path_costs
%%%

-spec update_path_costs(routing_db(), ip(), path_cost()) -> ok.

update_path_costs(RoutingDb, PeerIp, UpdatedPc) ->
    foreach_routing_entry(
      fun(#routing_entry{ip = ReIp} = Re) when ReIp == PeerIp ->
              UpdatedFlags = ?bit_set(Re#routing_entry.flags, ?F_RE_UPDATED),
              true = ets:insert(RoutingDb,
                                Re#routing_entry{path_cost = UpdatedPc,
                                                 flags = UpdatedFlags});
         (_) ->
              ok
      end, RoutingDb).
