-module(node_route).

%%% External exports
-export([create_node_db/0, delete_node_db/1, lookup_node/2, insert_node/2,
         delete_node/2, get_nodes/1, foreach_node/2, map_node/2, foldl_node/3,
         is_member_node/2, unreachable_nodes/1, lookup_node_send_serv/3]).
-export([create_route_db/0, delete_route_db/1, get_route_entries/1,
         foreach_route_entry/2, map_route_entry/2, foldl_route_entry/3,
         update_route_entry/2]).
-export([recalc/4]).
-export([update_path_cost/3, update_path_costs/3]).

%%% internal exports

%%% include files
-include_lib("util/include/bits.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").
-include_lib("node/include/node_route.hrl").

%%% constants

%%% records

%%% types
-type foreach_node_fun() :: fun((#node{}) -> any()).
-type map_node_fun() :: fun((#node{}) -> any()).
-type foldl_node_fun() :: fun((#node{}, any()) -> any()).
-type foreach_route_entry_fun() :: fun((#route_entry{}) -> any()).
-type map_route_entry_fun() :: fun((#route_entry{}) -> any()).
-type foldl_route_entry_fun() :: fun((#route_entry{}, any()) -> any()).

%%%
%%% exported: create_node_db
%%%

-spec create_node_db() -> {'ok', node_db()}.

create_node_db() ->
    {ok, ets:new(node_db, [{keypos, 2}, {read_concurrency, true}])}.

%%%
%%% exported: delete_node_db
%%%

-spec delete_node_db(node_db()) -> 'ok'.

delete_node_db(NodeDb) ->
    true = ets:delete(NodeDb),
    ok.

%%%
%%% exported: lookup_node
%%%

-spec lookup_node(node_db(), node_id()) -> [#node{}].

lookup_node(NodeDb, NodeId) ->
    ets:lookup(NodeDb, NodeId).

%%%
%%% exported: insert_node
%%%

-spec insert_node(node_db(), #node{}) -> 'ok'.

insert_node(NodeDb, Node) ->
    true = ets:insert(NodeDb, Node),
    ok.

%%%
%%% exported: delete_node
%%%

-spec delete_node(node_db(), node_id()) -> 'ok'.

delete_node(NodeDb, NodeId) ->
    true = ets:delete(NodeDb, NodeId),
    ok.

%%%
%%% exported: is_member_node
%%%

-spec is_member_node(node_db(), node_id()) -> boolean().

is_member_node(NodeDb, NodeId) ->
    ets:member(NodeDb, NodeId).

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
%%% exported: map_node
%%%

-spec map_node(map_node_fun(), node_db()) -> any().

map_node(Fun, NodeDb) ->
    ets:foldl(fun(Node, []) ->
                      Fun(Node)
              end, [], NodeDb).

%%%
%%% exported: foldl_node
%%%

-spec foldl_node(foldl_node_fun(), any(), node_db()) -> any().

foldl_node(Fun, InitAcc, NodeDb) ->
    ets:foldl(fun(Node, Acc) ->
                      Fun(Node, Acc)
              end, InitAcc, NodeDb).

%%%
%%% exported: unreachable_nodes
%%%

-spec unreachable_nodes(node_db()) -> [#node{}].

unreachable_nodes(NodeDb) ->
    ets:match_object(NodeDb, #node{path_cost = ?NODE_UNREACHABLE, _ = '_'}).

%%%
%%% exported: lookup_node_send_serv
%%%

-spec lookup_node_send_serv(node_db(), route_db(), node_id() | oa()) ->
                                   {'ok', pid()} | {'error', 'not_found'}.

lookup_node_send_serv(NodeDb, _RouteDb, NodeId) when is_integer(NodeId) ->
    case ets:lookup(NodeDb, NodeId) of
        [#node{node_send_serv = NodeSendServ}] ->
            {ok, NodeSendServ};
        [] ->
            {error, not_found}
    end;
lookup_node_send_serv(NodeDb, RouteDb, Oa) when is_tuple(Oa) ->
    case ets:lookup(RouteDb, Oa) of
        [#route_entry{node_id = NodeId}] ->
            lookup_node_send_serv(NodeDb, RouteDb, NodeId);
        [] ->
            {error, not_found}
    end.

%%%
%%% exported: create_route_db
%%%

-spec create_route_db() -> {'ok', route_db()}.

create_route_db() ->
    {ok, ets:new(route_db, [{keypos, 2}, {read_concurrency, true}])}.

%%%
%%% exported: delete_route_db
%%%

-spec delete_route_db(route_db()) -> 'ok'.

delete_route_db(RouteDb) ->
    true = ets:delete(RouteDb),
    ok.

%%%
%%% exported: get_route_entries
%%%

-spec get_route_entries(route_db()) -> {'ok', [#route_entry{}]}.

get_route_entries(RouteDb) ->
    Res = ets:foldl(fun(Re, Acc) -> [Re|Acc] end, [], RouteDb),
    {ok, Res}.

%%%
%%% exported: foreach_route_entry
%%%

-spec foreach_route_entry(foreach_route_entry_fun(), route_db()) -> any().

foreach_route_entry(Fun, RouteDb) ->
    ets:foldl(fun(Re, []) ->
                      Fun(Re),
                      []
              end, [], RouteDb).

%%%
%%% exported: map_route_entry
%%%

-spec map_route_entry(map_route_entry_fun(), route_db()) -> any().

map_route_entry(Fun, RouteDb) ->
    ets:foldl(fun(Re, []) ->
                      Fun(Re)
              end, [], RouteDb).

%%%
%%% exported: foldl_route_entry
%%%

-spec foldl_route_entry(foldl_route_entry_fun(), any(), route_db()) -> any().

foldl_route_entry(Fun, InitAcc, RouteDb) ->
    ets:foldl(fun(Re, Acc) ->
                      Fun(Re, Acc)
              end, InitAcc, RouteDb).

%%%
%%% exported: update_route_entry
%%%

-spec update_route_entry(route_db(), #route_entry{}) ->
                                {'updated', #route_entry{}} |
                                {'kept', #route_entry{}} |
                                'got_new' |
                                {'got_better', #route_entry{}} |
                                {'got_worse', #route_entry{}}.

update_route_entry(RouteDb,
                   #route_entry{oa = ReOa, node_id = ReNodeId,
                                path_cost = RePc, flags = ReFlags} = Re) ->
    case ets:lookup(RouteDb, ReOa) of
        %% existing route entry got new path cost
        [#route_entry{node_id = ReNodeId, path_cost = CurrentPc} = CurrentRe]
          when RePc /= CurrentPc ->
	    UpdatedFlags = ?bit_set(ReFlags, ?F_RE_UPDATED),
	    true = ets:insert(RouteDb, Re#route_entry{flags = UpdatedFlags}),
            {updated, CurrentRe};
        %% existing route entry not changed (same path cost as before)
        [#route_entry{node_id = ReNodeId} = CurrentRe] ->
            {kept, CurrentRe};
        %% new route entry
        [] ->
            UpdatedFlags = ?bit_set(ReFlags, ?F_RE_UPDATED),
            true = ets:insert(RouteDb, Re#route_entry{flags = UpdatedFlags}),
            got_new;
        %% better route entry
        [#route_entry{path_cost = Pc} = CurrentRe]
          when RePc /= ?NODE_UNREACHABLE andalso RePc < Pc ->
	    UpdatedFlags = ?bit_set(ReFlags, ?F_RE_UPDATED),
	    true = ets:insert(RouteDb, Re#route_entry{flags = UpdatedFlags}),
            {got_better, CurrentRe};
        %% worse route entry
        [CurrentRe] ->
            {got_worse, CurrentRe}
    end.

%%%
%%% exported: recalc
%%%

-spec recalc(node_id(), node_db(), route_db(), node_psp:psp_db()) -> 'ok'.

recalc(MyNodeId, NodeDb, RouteDb, PspDb) ->
    touch_route_entries(NodeDb, RouteDb),
    propagate_route_entries(MyNodeId, NodeDb, RouteDb, PspDb),
    clear_node_flags(NodeDb),
    clear_route_entry_flags(RouteDb),
    true = ets:match_delete(
             RouteDb, #route_entry{path_cost = ?NODE_UNREACHABLE, _ = '_'}),
    ok.

touch_route_entries(NodeDb, RouteDb) ->
    foreach_node(
      fun(#node{node_id = NodeId, flags = Flags})
            when ?bit_is_set(Flags, ?F_NODE_UPDATED) ->
              foreach_route_entry(
                fun(#route_entry{node_id = ReNodeId, flags = ReFlags} = Re)
                      when ReNodeId == NodeId ->
                        UpdatedReFlags = ?bit_set(ReFlags, ?F_RE_UPDATED),
                        true = ets:insert(RouteDb,
                                          Re#route_entry{
                                            flags = UpdatedReFlags});
                   (_) ->
                        ok
                end, RouteDb);
         (_) ->
              ok
      end, NodeDb).

propagate_route_entries(MyNodeId, NodeDb, RouteDb, PspDb) ->
    foreach_node(
      fun(Node) ->
              send_route_entries(MyNodeId, RouteDb, PspDb, Node)
      end, NodeDb).

send_route_entries(MyNodeId, _RouteDb, _PspDb,
                   #node{node_id = NodeId, na = Na,
                         node_send_serv = undefined}) ->
    ?dbg_log({no_send_serv, MyNodeId, NodeId, Na}),
    ok;
send_route_entries(MyNodeId, RouteDb, PspDb,
                   #node{node_id = NeighbourNodeId,
                         path_cost = NeighbourPc,
                         flags = NeighbourFlags,
                         node_send_serv = NodeSendServ}) ->
    foreach_route_entry(
      fun(#route_entry{node_id = ReNodeId, flags = ReFlags} = Re) ->
              if
                  NeighbourNodeId /= ReNodeId andalso
                  NeighbourPc /= ?NODE_UNREACHABLE andalso
                  (?bit_is_set(NeighbourFlags, ?F_NODE_UPDATED) orelse
                   ?bit_is_set(ReFlags, ?F_RE_UPDATED)) ->
                      send_route_entry(
                        MyNodeId, PspDb, NeighbourPc, NodeSendServ, Re);
                  true ->
                      ok
              end
      end, RouteDb).

send_route_entry(_MyNodeId, PspDb, NeighbourPc, NodeSendServ,
                 #route_entry{node_id = ReNodeId, path_cost = RePc,
                              path_cost_auth = PcAuth, psp = Psp} = Re) ->
% patrik: like this perhaps?
%    if
%        ReNodeId == MyNodeId ->
%            %% See include/node_route.hrl, i.e. #route:entry.path_cost_auth is
%            %% of type node_path_cost_auth:auth(). It could perhaps be:
%            %% auth() :: {costs(), signature(), r0_hash()}
%            {ok, NewPcAuth} =
%                node_path_cost_auth:new(NeighbourPublicKey, PrivateKey);
%        true ->
%            %% Verify r0
%            case node_path_cost_auth:verify_r0(PcAuth) of
%                true ->
%                    NewPcAuth = PcAuth;
%                false ->
%                    NewPcAuth = invalid_path_cost
%            end
%    end,
    NewPcAuth = PcAuth,
    if
        NewPcAuth == invalid_path_cost ->
            ?daemon_log("The route entry pointing to ~w has an invalid path "
                        "cost and will be ignored",
                        [ReNodeId]);
        true ->
            if
                NeighbourPc == ?NODE_UNREACHABLE orelse
                RePc == ?NODE_UNREACHABLE ->
                    UpdatedPc = ?NODE_UNREACHABLE,
% patrik: and like this?
%                    {ok, UpdatedPcAuth} =
%                        node_path_cost:add_cost(NewPcAuth, 256),
                    UpdatedPcAuth = PcAuth;
                true ->
                    case RePc+NeighbourPc of
                        AggregatedPc when AggregatedPc > ?NODE_UNREACHABLE ->
                            UpdatedPc = ?NODE_UNREACHABLE;
                        UpdatedPc ->
                            ok
                    end,
% patrik: and like this?
%                    {ok, UpdatedPcAuth} =
%                        node_path_cost:add_cost(NewPcAuth, NeighbourPc div 10)
                    UpdatedPcAuth = PcAuth
            end,
            UpdatedPsp = node_psp:add_me(PspDb, Psp),
            UpdatedRe =
                Re#route_entry{
                  path_cost = UpdatedPc,
                  path_cost_auth = UpdatedPcAuth,
                  psp = UpdatedPsp
                 },
            ok = node_send_serv:send(NodeSendServ, {node_route_serv, UpdatedRe})
    end.

clear_node_flags(NodeDb) ->
    foreach_node(
      fun(#node{path_cost = ?NODE_UNREACHABLE}) ->
              ok;
         (#node{flags = Flags} = Node)
            when ?bit_is_set(Flags, ?F_NODE_UPDATED) ->
              UpdatedFlags = ?bit_clr(Flags, ?F_NODE_UPDATED),
              true = ets:insert(NodeDb, Node#node{flags = UpdatedFlags});
         (_) ->
              ok
      end, NodeDb).

clear_route_entry_flags(RouteDb) ->
    foreach_route_entry(
      fun(#route_entry{flags = Flags} = Re)
            when ?bit_is_set(Flags, ?F_RE_UPDATED) ->
              UpdatedFlags = ?bit_clr(Flags, ?F_RE_UPDATED),
              true = ets:insert(RouteDb, Re#route_entry{flags = UpdatedFlags});
         (_) ->
              ok
      end, RouteDb).

%%%
%%% exported: update_path_cost
%%%

-spec update_path_cost(node_db(), node_id(), path_cost()) -> ok.

update_path_cost(NodeDb, NeighbourNodeId, UpdatedPc) ->
    case ets:lookup(NodeDb, NeighbourNodeId) of
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

-spec update_path_costs(route_db(), node_id(), path_cost()) -> ok.

update_path_costs(RouteDb, NeighbourNodeId, UpdatedPc) ->
    foreach_route_entry(
      fun(#route_entry{node_id = ReNodeId} = Re)
            when ReNodeId == NeighbourNodeId ->
              UpdatedFlags = ?bit_set(Re#route_entry.flags, ?F_RE_UPDATED),
              true = ets:insert(RouteDb,
                                Re#route_entry{path_cost = UpdatedPc,
                                               flags = UpdatedFlags});
         (_) ->
              ok
      end, RouteDb).
