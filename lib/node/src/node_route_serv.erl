-module(node_route_serv).

%%% external exports
-export([start_link/2, stop/1, stop/2]).
-export([handshake/2]).
-export([get_route_entries/1, route_entry/2]).
-export([get_nodes/1]).
-export([enable_recalc/1, disable_recalc/1, recalc/1]).
-export([update_path_cost/3]).

%%% internal exports
-export([init/3]).

%%% include files

-include_lib("ds/include/ds.hrl").
-include_lib("node/include/node.hrl").
-include_lib("node/include/node_route.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants
-define(FIVE_SECONDS, 5*1000).

%%% records
-record(state, {
          parent                     :: pid(),
          node_db                    :: node_db(),
          route_db                   :: route_db(),
          psp_db                     :: node_psp:psp_db(),
          ttl                        :: non_neg_integer(),
          neighbour_nas = []         :: [na()],
          node_instance_sup          :: supervisor:child(),
          node_path_cost_serv        :: pid(),
          %% anond.conf parameters
          my_na                      :: na(),
          experimental_api           :: boolean(),
          directory_server           :: {inet:ip4_address(),
                                         inet:port_number()},
          my_oa                      :: oa(),
	  public_key                 :: node_crypto:pki_key(),
	  private_key                :: node_crypto:pki_key(),
          number_of_neighbours       :: non_neg_integer(),
          refresh_neighbours_timeout :: non_neg_integer(),
          recalc_timeout             :: non_neg_integer(),
          auto_recalc                :: boolean()
	 }).

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link(na(), supervisor:sup_ref()) -> {'ok', pid()}.

start_link(MyNa, NodeInstanceSup) ->
    Args = [self(), MyNa, NodeInstanceSup],
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

-spec stop(pid()) -> 'ok'.

stop(Pid) ->
    stop(Pid, 15000).

-spec stop(pid(), timeout()) -> 'ok'.

stop(Pid, Timeout) ->
    serv:call(Pid, stop, Timeout).

%%%
%%% exported: handshake
%%%

-spec handshake(pid(),
                {'node_send_serv', na(), pid()} |
                'node_recv_serv' |
                {'node_path_cost_serv', pid()} |
                'node_tun_serv') ->
                       'ok' | {'ok', node_db(), route_db()}.

handshake(Pid, node_recv_serv) ->
    serv:call(Pid, {handshake, node_recv_serv});
handshake(Pid, {node_path_cost_serv, NodePathCostServ}) ->
    serv:call(Pid, {handshake, {node_path_cost_serv, NodePathCostServ}});
handshake(Pid, {node_send_serv, Na, NodeSendServ}) ->
    Pid ! {handshake, {node_send_serv, Na, NodeSendServ}},
    ok;
handshake(Pid, node_tun_serv) ->
    serv:call(Pid, {handshake, node_tun_serv}).

%%%
%%% exported: get_route_entries
%%%

-spec get_route_entries(pid()) -> {'ok', [#route_entry{}]}.

get_route_entries(Pid) ->
    serv:call(Pid, get_route_entries).

%%%
%%% exported: route_entry
%%%

-spec route_entry(pid(), #route_entry{}) -> 'ok'.

route_entry(Pid, Re) ->
    Pid ! Re,
    ok.

%%%
%%% exported: get_nodes
%%%

-spec get_nodes(pid()) -> {'ok', [#node{}]}.

get_nodes(Pid) ->
    serv:call(Pid, get_nodes).

%%%
%%% exported: enable_recalc
%%%

-spec enable_recalc(pid()) -> 'ok'.

enable_recalc(Pid) ->
    Pid ! enable_recalc,
    ok.

%%%
%%% exported: disable_recalc
%%%

-spec disable_recalc(pid()) -> 'ok'.

disable_recalc(Pid) ->
    Pid ! disable_recalc,
    ok.

%%%
%%% exported: recalc
%%%

-spec recalc(pid()) -> 'ok'.

recalc(Pid) ->
    Pid ! recalc,
    ok.

%%%
%%% exported: update_path_cost
%%%

-spec update_path_cost(pid(), na(), path_cost()) -> 'ok'.

update_path_cost(Pid, Na, Pc) ->
    Pid ! {path_cost, Na, Pc},
    ok.

%%%
%%% server loop
%%%

init(Parent, MyNa, NodeInstanceSup) ->
    process_flag(trap_exit, true),
    {A1, A2, A3} = erlang:now(),
    random:seed({A1, A2, A3}),
    S = read_config(#state{my_na = MyNa}),
    ok = config_json_serv:subscribe(),
    {ok, NodeDb} = node_route:create_node_db(),
    {ok, RouteDb} = node_route:create_route_db(),
    {ok, PspDb} = node_psp:init(),
    self() ! bootstrap,
    Parent ! {self(), started},
    loop(S#state{parent = Parent, node_db = NodeDb, route_db = RouteDb,
                 psp_db = PspDb, node_instance_sup = NodeInstanceSup}).

loop(#state{parent = Parent,
            node_db = NodeDb,
	    route_db = RouteDb,
            psp_db = PspDb,
            ttl = _TTL,
            neighbour_nas = NeighbourNas,
            node_instance_sup = NodeInstanceSup,
            node_path_cost_serv = NodePathCostServ,
            my_na = MyNa,
            experimental_api = ExperimentalApi,
            directory_server = DsIpAddressPort,
	    my_oa = MyOa,
	    public_key = PublicKey,
	    private_key = PrivateKey,
            number_of_neighbours = NumberOfNeighbours,
            refresh_neighbours_timeout = RefreshNeighboursTimeout,
            recalc_timeout = RecalcTimeout,
            auto_recalc = AutoRecalc} = S) ->
    receive
        config_updated ->
            ?daemon_log("Configuration changed...", []),
            loop(read_config(S));
        bootstrap ->
            if
                ExperimentalApi ->
                    Flags = ?F_DS_EXPERIMENTAL_API;
                true ->
                    Flags = 0
            end,
            NodeDescriptor =
                #node_descriptor{public_key = PublicKey, flags = Flags},
            case ds_jsonrpc:publish_node(
                   MyNa, DsIpAddressPort, PrivateKey, NodeDescriptor) of
                {ok, UpdatedTTL} ->
                    ?daemon_log("Published node address ~s",
                                [net_tools:string_address(MyNa)]),
                    case ds_jsonrpc:reserve_oa(
                           MyNa, DsIpAddressPort, PrivateKey, MyOa) of
                        ok ->
                            ?daemon_log("Reserved overlay address ~s",
                                        [net_tools:string_address(MyOa)]),
                            {ok, Psp} = node_psp:new(PspDb),
                            Re =
                                #route_entry{oa = MyOa, na = MyNa,
                                             path_cost = 0, psp = Psp},
                            got_new =
                                node_route:update_route_entry(RouteDb, Re),
                            self() ! refresh_neighbours,
                            timelib:start_timer(trunc(UpdatedTTL/2),
                                                republish_self),
                            if
                                S#state.auto_recalc ->
                                    RandomRecalcTimeout =
                                        random:uniform(RecalcTimeout),
                                    timelib:start_timer(
                                      RandomRecalcTimeout, recalc),
                                    loop(S#state{ttl = UpdatedTTL});
                                true ->
                                    loop(S#state{ttl = UpdatedTTL})
                            end;
                        {error, Reason} ->
                            ?dbg_log(Reason),
                            ?daemon_log(
                               "Could not reserve overlay address ~s. Retrying "
                               "in five seconds...",
                               [net_tools:string_address(MyOa)]),
                            timelib:start_timer(?FIVE_SECONDS, bootstrap),
                            loop(S)
                    end;
                {error, Reason}->
                    ?dbg_log(Reason),
                    ?daemon_log(
                       "Could not publish node address ~s. Retrying in five "
                       "seconds...", [net_tools:string_address(MyNa)]),
                    timelib:start_timer(?FIVE_SECONDS, bootstrap),
                    loop(S)
            end;
        republish_self ->
            if
                ExperimentalApi ->
                    Flags = ?F_DS_EXPERIMENTAL_API;
                true ->
                    Flags = 0
            end,
            NodeDescriptor = #node_descriptor{public_key = PublicKey,
                                              flags = Flags},
            case ds_jsonrpc:publish_node(
                   MyNa, DsIpAddressPort, PrivateKey, NodeDescriptor) of
                {ok, UpdatedTTL} ->
                    ?daemon_log("Republished node address ~s",
                                [net_tools:string_address(MyNa)]),
                    timelib:start_timer(trunc(UpdatedTTL/2), republish_self),
                    loop(S#state{ttl = UpdatedTTL});
                {error, Reason}->
                    ?dbg_log(Reason),
                    ?daemon_log(
                       "Could not publish node address ~s. Retrying in five "
                       "seconds...", [net_tools:string_address(MyNa)]),
                    timelib:start_timer(?FIVE_SECONDS, republish_self),
                    loop(S)
            end;
        refresh_neighbours ->
            ?daemon_log("Neighbour refresh started", []),
            case refresh_neighbours(
                   NodeDb, RouteDb, PspDb, NeighbourNas, NodeInstanceSup,
                   DsIpAddressPort, MyNa, PrivateKey, NumberOfNeighbours,
                   AutoRecalc) of
                {ok, UpdatedNeighbourNas} ->
                    timelib:start_timer(RefreshNeighboursTimeout,
                                        refresh_neighbours),
                    ok = node_path_cost_serv:updated_neighbour_nas(
                           NodePathCostServ, UpdatedNeighbourNas),
                    loop(S#state{neighbour_nas = UpdatedNeighbourNas});
                {error, Reason} ->
                    ?dbg_log(Reason),
                    ?daemon_log(
                       "Could not refresh neighbours. "
                       "Retrying in five seconds...", []),
                    timelib:start_timer(?FIVE_SECONDS, refresh_neighbours),
                    loop(S)
            end;
	{From, stop} ->
	    ok = node_route:delete_node_db(NodeDb),
	    ok = node_route:delete_route_db(RouteDb),
	    From ! {self(), ok};
        {handshake, {node_send_serv, NeighbourNa, NodeSendServ}} ->
            ok = node_route:add_node_send_serv(
                   NodeDb, NeighbourNa, NodeSendServ),
            loop(S);
        {From, {handshake, node_recv_serv}} ->
	    From ! {self(), {ok, NodeDb, RouteDb}},
            loop(S);
        {From, {handshake, {node_path_cost_serv, NewNodePathCostServ}}} ->
	    From ! {self(), {ok, NodeDb, RouteDb}},
            loop(S#state{node_path_cost_serv = NewNodePathCostServ});
        {From, {handshake, node_tun_serv}} ->
	    From ! {self(), {ok, NodeDb, RouteDb}},
            loop(S);
	{From, get_route_entries} ->
	    {ok, Res} = node_route:get_route_entries(RouteDb),
	    From ! {self(), {ok, Res}},
	    loop(S);
	#route_entry{oa = MyOa} ->
	    loop(S);
        #route_entry{na = ViaNa} = Re ->
            case handle_route_entry(
                   NodeDb, RouteDb, PspDb, NeighbourNas, NodeInstanceSup,
                   NodePathCostServ, DsIpAddressPort, MyNa, MyOa, PrivateKey,
                   Re) of
                {ok, UpdatedNeighbourNas} ->
                    loop(S#state{neighbour_nas = UpdatedNeighbourNas});
                {error, Reason} ->
                    ?dbg_log(Reason),
                    ?daemon_log("Discard route entry from ~s",
                                [net_tools:string_address(ViaNa)]),
                    loop(S)
            end;
	{From, get_nodes} ->
	    {ok, Nodes} = node_route:get_nodes(NodeDb),
	    From ! {self(), {ok, Nodes}},
	    loop(S);
	enable_recalc ->
            self() ! recalc,
            loop(S#state{auto_recalc = true});
	disable_recalc ->
            loop(S#state{auto_recalc = false});
	recalc ->
	    ?daemon_log("** ~s recalculates its route table.",
                        [net_tools:string_address(MyNa)]),
            ok = node_route:recalc(MyNa, NodeDb, RouteDb, PspDb, PrivateKey),
            if
                AutoRecalc ->
                    RandomRecalcTimeout = random:uniform(RecalcTimeout),
                    timelib:start_timer(RandomRecalcTimeout, recalc),
                    loop(S);
                true ->
                    loop(S)
            end;
	{path_cost, NeighbourNa, Pc}  ->
	    ok = node_route:update_path_cost(NodeDb, NeighbourNa, Pc),
	    loop(S);
	{'EXIT', Parent, Reason} ->
	    ok = node_route:delete_node_db(NodeDb),
	    ok = node_route:delete_route_db(RouteDb),
            exit(Reason);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

%%%
%%% init
%%%

read_config(S) ->
    NodeInstance = ?config([nodes, {'node-address', S#state.my_na}]),
    read_config(S, NodeInstance).

read_config(S, []) ->
    S;
read_config(S, [{'experimental-api', Value}|Rest]) ->
    read_config(S#state{experimental_api = Value}, Rest);
read_config(S, [{'directory-server', Value}|Rest]) ->
    read_config(S#state{directory_server = Value}, Rest);
read_config(S, [{'overlay-addresses', [MyOa]}|Rest]) ->
    read_config(S#state{my_oa = MyOa}, Rest);
read_config(_S, [{'overlay-addresses', _MyOas}|_Rest]) ->
    throw(nyi);
read_config(S, [{'public-key', Value}|Rest]) ->
    Key = node_crypto:read_pki_key(Value),
    read_config(S#state{public_key = Key}, Rest);
read_config(S, [{'private-key', Value}|Rest]) ->
    Key = node_crypto:read_pki_key(Value),
    read_config(S#state{private_key = Key}, Rest);
read_config(S, [{'number-of-neighbours', Value}|Rest]) ->
    read_config(S#state{number_of_neighbours = Value}, Rest);
read_config(S, [{'refresh-neighbours-timeout', Value}|Rest]) ->
    read_config(S#state{refresh_neighbours_timeout = Value}, Rest);
read_config(S, [{'recalc-timeout', Value}|Rest]) ->
    read_config(S#state{recalc_timeout = Value}, Rest);
read_config(S, [{'auto-recalc', Value}|Rest]) ->
    read_config(S#state{auto_recalc = Value}, Rest);
read_config(S, [{'path-cost', _}|Rest]) ->
    read_config(S, Rest);
read_config(S, [_|Rest]) ->
    read_config(S, Rest).

%%%
%%% refresh_neighbours
%%%

refresh_neighbours(
  NodeDb, RouteDb, PspDb, NeighbourNas, NodeInstanceSup, DsIpAddressPort,
  MyNa, PrivateKey, NumberOfNeighbours, AutoRecalc) ->
    ?daemon_log("Known neighbours: ~s",
                [net_tools:string_addresses(NeighbourNas)]),
    case ds_jsonrpc:published_nodes(
           MyNa, DsIpAddressPort, PrivateKey, NeighbourNas) of
        {ok, PublishedNeighbourNas} ->
            ?daemon_log("Still published neighbours: ~s",
                        [net_tools:string_addresses(PublishedNeighbourNas)]),
            UnreachableNodes = node_route:unreachable_nodes(NodeDb),
            UnreachableNeighbourNas =
                [Node#node.na || Node <- UnreachableNodes],
            ?daemon_log("Unreachable neighbours: ~s",
                        [net_tools:string_addresses(UnreachableNeighbourNas)]),
            RemainingNeighbourNas =
                PublishedNeighbourNas--UnreachableNeighbourNas,
            ?daemon_log("Remaining published and reachable neighbours: ~s",
                        [net_tools:string_addresses(RemainingNeighbourNas)]),
            case NumberOfNeighbours-length(RemainingNeighbourNas) of
                NumberOfMissingNeighbours when NumberOfMissingNeighbours > 0 ->
                    ?daemon_log("Need ~w additional neighbours...",
                                [NumberOfMissingNeighbours]),
                    get_more_neighbours(
                      NodeDb, RouteDb, PspDb, NeighbourNas, NodeInstanceSup,
                      DsIpAddressPort, MyNa, PrivateKey, AutoRecalc,
                      RemainingNeighbourNas, NumberOfMissingNeighbours);
                _ ->
                    {ok, RemainingNeighbourNas}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_more_neighbours(
  NodeDb, RouteDb, PspDb, NeighbourNas, NodeInstanceSup, DsIpAddressPort,
  MyNa, PrivateKey, AutoRecalc, RemainingNeighbourNas,
  NumberOfMissingNeighbours) ->
    case ds_jsonrpc:get_random_nodes(
           MyNa, DsIpAddressPort, PrivateKey, NumberOfMissingNeighbours) of
        {ok, NewNeighbours} ->
            NewNeighbourNas = [NewNeighbour#node_descriptor.na ||
                                  NewNeighbour <- NewNeighbours],
            ?daemon_log("Found ~w new neighbours: ~s",
                        [NumberOfMissingNeighbours,
                         net_tools:string_addresses(NewNeighbourNas)]),
            purge_neighbours(NodeDb, RouteDb, NodeInstanceSup,
                             RemainingNeighbourNas, NeighbourNas),
            lists:foreach(
              fun(#node_descriptor{na = NeighbourNa,
                                   public_key = NeighbourPublicKey}) ->
                      {ok, NodeSendServ} =
                          node_send_sup:start_node_send_serv(
                            MyNa, NeighbourNa, NodeInstanceSup),
                      Node = #node{
                        na = NeighbourNa,
                        public_key = NeighbourPublicKey,
                        flags = ?F_NODE_UPDATED,
                        node_send_serv = NodeSendServ},
                      ok = node_route:add_node(NodeDb, Node)
              end, NewNeighbours),
            UpdatedNeighbourNas = NewNeighbourNas++RemainingNeighbourNas,
            if
                AutoRecalc ->
                    ok = node_route:recalc(
                           MyNa, NodeDb, RouteDb, PspDb, PrivateKey),
                    {ok, UpdatedNeighbourNas};
                true ->
                    {ok, UpdatedNeighbourNas}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

purge_neighbours(_NodeDb, _RouteDb, _NodeInstanceSup, _RemainingNeighbourNas,
                 []) ->
    ok;
purge_neighbours(NodeDb, RouteDb, NodeInstanceSup, RemainingNeighbourNas,
                 [NeighbourNa|Rest]) ->
    case lists:member(NeighbourNa, RemainingNeighbourNas) of
        false ->
            ok = node_route:delete_node(NodeDb, NeighbourNa),
            ok = node_send_sup:stop_node_send_serv(NeighbourNa,
                                                   NodeInstanceSup),
            ok = node_route:update_path_costs(RouteDb, NeighbourNa, -1),
            purge_neighbours(NodeDb, RouteDb, NodeInstanceSup,
                             RemainingNeighbourNas, Rest);
        true ->
            purge_neighbours(NodeDb, RouteDb, NodeInstanceSup,
                             lists:delete(NeighbourNa, RemainingNeighbourNas),
                             Rest)
    end.

%%%
%%% handle incoming #route_entry{}
%%%

handle_route_entry(
  NodeDb, RouteDb, PspDb, NeighbourNas, NodeInstanceSup, NodePathCostServ,
  DsIpAddressPort, MyNa, MyOa, PrivateKey,
  #route_entry{na = ViaNa, path_cost = Pc} = Re) ->
    case node_route:is_member_node(NodeDb, ViaNa) of
        true ->
            update_route_entry(RouteDb, PspDb, NeighbourNas, MyNa, MyOa, Re);
        false ->
            case ds_jsonrpc:get_node(
                   MyNa, DsIpAddressPort, PrivateKey, ViaNa) of
                {ok, #node_descriptor{public_key = PublicKey}} ->
                    ?daemon_log("~s adding neighbour ~s",
                                [net_tools:string_address(MyOa),
                                 net_tools:string_address(ViaNa)]),
                    UpdatedNeighbourNas = [ViaNa|NeighbourNas],
                    ok = node_path_cost_serv:updated_neighbour_nas(
                           NodePathCostServ, UpdatedNeighbourNas),
                    {ok, NodeSendServ} =
                        node_send_sup:start_node_send_serv(
                          MyNa, ViaNa, NodeInstanceSup),
                    Node = #node{na = ViaNa, public_key = PublicKey,
                                 path_cost = Pc,
                                 flags = ?F_NODE_UPDATED bor
                                     ?F_NODE_IS_INCOMING_NEIGHBOUR,
                                 node_send_serv = NodeSendServ},
                    ok = node_route:add_node(NodeDb, Node),
                    update_route_entry(RouteDb, PspDb, UpdatedNeighbourNas,
                                       MyNa, MyOa, Re);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

update_route_entry(RouteDb, PspDb, UpdatedNeighbourNas, MyNa, MyOa,
                   #route_entry{oa = DestOa, na = ViaNa, path_cost = Pc,
                                hops = Hops, psp = Psp} = Re) ->
    PspLoop = node_psp:is_loop(PspDb, Psp),
    HopsLoop = lists:member(MyNa, Hops),
    if
        PspLoop == HopsLoop ->
            ok;
        true ->
            ?dbg_log({psp_loop_mismatch, MyNa, PspLoop, HopsLoop, Hops})
    end,
    case HopsLoop of
        true ->
            ?dbg_log({loop_rejected, MyNa, Hops}),
            ?daemon_log(
               "~s rejected looping route entry: ~s -> ~s (~w)",
               [net_tools:string_address(MyOa),
                net_tools:string_address(DestOa),
                net_tools:string_address(ViaNa), Pc]),
            {ok, UpdatedNeighbourNas};
        false ->
            case node_route:update_route_entry(RouteDb, Re) of
                {updated, #route_entry{path_cost = CurrentPc}} ->
                    ?daemon_log(
                       "~s updated existing route: ~s -> ~s (~w, ~w) with new "
                       "path cost ~w",
                       [net_tools:string_address(MyOa),
                        net_tools:string_address(DestOa),
                        net_tools:string_address(ViaNa), CurrentPc, Hops, Pc]),
                    {ok, UpdatedNeighbourNas};
                {kept, _CurrentRe} ->
                    ?daemon_log(
                       "~s kept existing route: ~s -> ~s (~w, ~w)",
                       [net_tools:string_address(MyOa),
                        net_tools:string_address(DestOa),
                        net_tools:string_address(ViaNa), Pc, Hops]),
                    {ok, UpdatedNeighbourNas};
                got_new ->
                    ?daemon_log(
                       "~s got new route: ~s -> ~s (~w, ~w)",
                       [net_tools:string_address(MyOa),
                        net_tools:string_address(DestOa),
                        net_tools:string_address(ViaNa), Pc, Hops]),
                    {ok, UpdatedNeighbourNas};
                {got_better,
                 #route_entry{
                   na = CurrentViaNa, path_cost = CurrentPc,
                   hops = CurrentHops}} ->
                    ?daemon_log(
                       "~s got better route: ~s -> ~s (~w, ~w) "
                       "replacing ~s -> ~s (~w, ~w)",
                       [net_tools:string_address(MyOa),
                        net_tools:string_address(DestOa),
                        net_tools:string_address(ViaNa), Pc, Hops,
                        net_tools:string_address(DestOa),
                        net_tools:string_address(CurrentViaNa),
                        CurrentPc, CurrentHops]),
                    {ok, UpdatedNeighbourNas};
                {got_worse,
                 #route_entry{
                   na = CurrentViaNa, path_cost = CurrentPc,
                   hops = CurrentHops}} ->
                    ?daemon_log(
                       "~s got worse route: ~s -> ~s (~w, ~w) "
                       "not replacing ~s -> ~s (~w, ~w)",
                       [net_tools:string_address(MyOa),
                        net_tools:string_address(DestOa),
                        net_tools:string_address(ViaNa), Pc, Hops,
                        net_tools:string_address(DestOa),
                        net_tools:string_address(CurrentViaNa), CurrentPc,
                        CurrentHops]),
                    {ok, UpdatedNeighbourNas}
            end
    end.
