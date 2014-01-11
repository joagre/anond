-module(node_route).

%%% external exports
-export([create_node_db/0, delete_node_db/1, add_node/2, delete_node/2,
         get_nodes/1, foreach_node/2, is_member_node/2, unreachable_nodes/1,
         add_node_send_serv/3, lookup_node_send_serv/3]).
-export([create_route_db/0, delete_route_db/1, get_route_entries/1,
         foreach_route_entry/2, update_route_entry/2]).
-export([recalc/5]).
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
-type foreach_route_entry_fun() :: fun((#route_entry{}) -> any()).

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
%%% exported: add_node
%%%

-spec add_node(node_db(), #node{}) -> 'ok'.

add_node(NodeDb, Node) ->
    true = ets:insert(NodeDb, Node),
    ok.

%%%
%%% exported: delete_node
%%%

-spec delete_node(node_db(), na()) -> 'ok'.

delete_node(NodeDb, Na) ->
    true = ets:delete(NodeDb, Na),
    ok.

%%%
%%% exported: is_member_node
%%%

-spec is_member_node(node_db(), na()) -> boolean().

is_member_node(NodeDb, Na) ->
    ets:member(NodeDb, Na).

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

-spec unreachable_nodes(node_db()) -> [#node{}].

unreachable_nodes(NodeDb) ->
    ets:match_object(NodeDb, #node{path_cost = -1, _ = '_'}).

%%%
%%% exported: add_node_send_serv
%%%

-spec add_node_send_serv(node_db(), na(), pid()) -> 'ok'.

add_node_send_serv(NodeDb, PeerNa, NodeSendServ) ->
    case ets:lookup(NodeDb, PeerNa) of
        [Node] ->
	    true = ets:insert(
                     NodeDb, Node#node{node_send_serv = NodeSendServ}),
            ok;
        _ ->
            ok
    end.

%%%
%%% exported: lookup_node_send_serv
%%%

-spec lookup_node_send_serv(node_db(), route_db(), na() | oa()) ->
                                   {'ok', pid()} | {'error', 'not_found'}.

lookup_node_send_serv(NodeDb, _RouteDb, {_IpAddress, _Port} = Na) ->
    case ets:lookup(NodeDb, Na) of
        [#node{node_send_serv = NodeSendServ}] ->
            {ok, NodeSendServ};
        [] ->
            {error, not_found}
    end;
lookup_node_send_serv(NodeDb, RouteDb, Oa) ->
    case ets:lookup(RouteDb, Oa) of
        [#route_entry{na = Na}] ->
            case ets:lookup(NodeDb, Na) of
                [#node{node_send_serv = NodeSendServ}] ->
                    {ok, NodeSendServ};
                [] ->
                    {error, not_found}
            end;
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
%%% exported: update_route_entry
%%%

-spec update_route_entry(route_db(), #route_entry{}) ->
                                {'updated', #route_entry{}} |
                                {'kept', #route_entry{}} |
                                'got_new' |
                                {'got_better', #route_entry{}} |
                                {'got_worse', #route_entry{}}.

update_route_entry(RouteDb,
                   #route_entry{oa = ReOa, na = ReNa, path_cost = RePc,
                                flags = ReFlags} = Re) ->
    case ets:lookup(RouteDb, ReOa) of
        %% existing route entry got new path cost
        [#route_entry{na = ReNa, path_cost = CurrentPc} = CurrentRe]
          when RePc /= CurrentPc ->
	    UpdatedFlags = ?bit_set(ReFlags, ?F_RE_UPDATED),
	    true =
                ets:insert(RouteDb, Re#route_entry{flags = UpdatedFlags}),
            {updated, CurrentRe};
        %% existing route entry not changed (same path cost as before)
        [#route_entry{na = ReNa} = CurrentRe] ->
            {kept, CurrentRe};
        %% new route entry
        [] ->
            UpdatedFlags = ?bit_set(ReFlags, ?F_RE_UPDATED),
            true = ets:insert(RouteDb, Re#route_entry{flags = UpdatedFlags}),
            got_new;
        %% better route entry
        [#route_entry{path_cost = Pc} = CurrentRe]
          when RePc /= -1 andalso RePc < Pc ->
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

-spec recalc(na(), node_db(), route_db(), node_psp:psp_db(),
             public_key:rsa_private_key()) -> 'ok'.

recalc(Na, NodeDb, RouteDb, PspDb, PrivateKey) ->
    touch_route_entries(NodeDb, RouteDb),
    propagate_route_entries(Na, NodeDb, RouteDb, PspDb, PrivateKey),
    clear_node_flags(NodeDb),
    clear_route_entry_flags(RouteDb),
    true = ets:match_delete(RouteDb, #route_entry{path_cost = -1, _ = '_'}),
    ok.

touch_route_entries(NodeDb, RouteDb) ->
    foreach_node(
      fun(#node{na = Na, flags = Flags})
            when ?bit_is_set(Flags, ?F_NODE_UPDATED) ->
              foreach_route_entry(
                fun(#route_entry{na = ReNa, flags = ReFlags} = Re)
                      when ReNa == Na ->
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

propagate_route_entries(Na, NodeDb, RouteDb, PspDb, PrivateKey) ->
    foreach_node(
      fun(Node) ->
              send_route_entries(Na, RouteDb, PspDb, PrivateKey, Node)
      end, NodeDb).

send_route_entries(Na, RouteDb, PspDb, PrivateKey,
                   #node{na = PeerNa,
                         public_key = PeerPublicKey,
                         path_cost = PeerPc,
                         flags = PeerFlags,
                         node_send_serv = NodeSendServ}) ->
    foreach_route_entry(
      fun(#route_entry{na = ReNa, flags = ReFlags} = Re) ->
              if
                  PeerNa /= ReNa andalso
                  PeerPc /= undefined andalso
                  (?bit_is_set(PeerFlags, ?F_NODE_UPDATED) orelse
                   ?bit_is_set(ReFlags, ?F_RE_UPDATED)) ->
                      send_route_entry(Na, PspDb, PeerPc, NodeSendServ, Re,
                                       PeerPublicKey, PrivateKey);
                  true ->
                      ok
              end
      end, RouteDb).

send_route_entry(Na, PspDb, PeerPc, NodeSendServ,
                 #route_entry{na = ReNa, path_cost = RePc,
                              path_cost_auth = PcAuth, hops = Hops,
                              psp = Psp} = Re,
                 PeerPublicKey, PrivateKey) ->
% patrik: like this perhaps?
%    if
%        ReNa == Na ->
%            %% See include/node_route.hrl, i.e. #route:entry.path_cost_auth is
%            %% of type node_path_cost_auth:auth(). It could perhaps be:
%            %% auth() :: {costs(), signature(), r0_hash()}
%            {ok, NewPcAuth} =
%                node_path_cost_auth:new(PeerPublicKey, PrivateKey);
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
            ?daemon_log("The route entry pointing to ~s has an invalid path "
                        "cost and will be ignored",
                        [net_tools:string_address(ReNa)]);
        true ->
            if
                PeerPc == -1 orelse RePc == -1 ->
                    UpdatedPc = -1,
% patrik: and like this
%                    {ok, UpdatedPcAuth} =
%                        node_path_cost:add_cost(NewPcAuth, 256),
                    UpdatedPcAuth = PcAuth;
                true ->
                    UpdatedPc = RePc+PeerPc,
% patrik: and like this
%                    {ok, UpdatedPcAuth} =
%                        node_path_cost:add_cost(NewPcAuth, PeerPc div 10)
                    UpdatedPcAuth = PcAuth
            end,
            {ok, UpdatedPsp} = node_psp:add_me(PspDb, Psp),
            UpdatedRe =
                Re#route_entry{
                  na = Na,
                  path_cost = UpdatedPc,
                  path_cost_auth = UpdatedPcAuth,
                  hops = [Na|Hops],
                  psp = UpdatedPsp
                 },
            ok = node_send_serv:send(NodeSendServ, UpdatedRe)
    end.

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

-spec update_path_cost(node_db(), na(), path_cost()) -> ok.

update_path_cost(NodeDb, PeerNa, UpdatedPc) ->
    case ets:lookup(NodeDb, PeerNa) of
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

-spec update_path_costs(route_db(), na(), path_cost()) -> ok.

update_path_costs(RouteDb, PeerNa, UpdatedPc) ->
    foreach_route_entry(
      fun(#route_entry{na = ReNa} = Re) when ReNa == PeerNa ->
              UpdatedFlags = ?bit_set(Re#route_entry.flags, ?F_RE_UPDATED),
              true = ets:insert(RouteDb,
                                Re#route_entry{path_cost = UpdatedPc,
                                               flags = UpdatedFlags});
         (_) ->
              ok
      end, RouteDb).
