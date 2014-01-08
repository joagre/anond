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
-define(FIVE_SECONDS_TIMEOUT, 5*1000).

%%% records
-record(state, {
          parent                    :: pid(),
          node_db                   :: node_db(),
          route_db                  :: route_db(),
          ttl                       :: non_neg_integer(),
          peer_nas = []             :: [na()],
          node_instance_sup         :: supervisor:child(),
          node_path_cost_serv       :: pid(),
          %% anond.conf parameters
          directory_server          :: {inet:ip4_address(), inet:port_number()},
          na                        :: na(),
          oa                        :: oa(),
	  public_key                :: public_key:rsa_public_key(),
	  private_key               :: public_key:rsa_private_key(),
          number_of_peers           :: non_neg_integer(),          
          refresh_peers_timeout     :: non_neg_integer(),
          recalc_timeout            :: non_neg_integer(),
          auto_recalc               :: boolean()
	 }).

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link(na(), supervisor:sup_ref()) -> {'ok', pid()}.

start_link(Na, NodeInstanceSup) ->
    Args = [self(), Na, NodeInstanceSup],
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
                {'node_path_cost_serv', pid()}) ->
                       'ok' | {'ok', node_db(), route_db()}.

handshake(Pid, node_recv_serv) ->
    serv:call(Pid, {handshake, node_recv_serv});
handshake(Pid, {node_path_cost_serv, NodePathCostServ}) ->
    serv:call(Pid, {handshake, {node_path_cost_serv, NodePathCostServ}});
handshake(Pid, {node_send_serv, Na, NodeSendServ}) ->
    Pid ! {handshake, {node_send_serv, Na, NodeSendServ}},
    ok.

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

init(Parent, Na, NodeInstanceSup) ->
    process_flag(trap_exit, true),
    {A1, A2, A3} = erlang:now(),
    random:seed({A1, A2, A3}),
    S = read_config(#state{na = Na}),
    ok = config_json_serv:subscribe(),
    {ok, NodeDb} = node_route:create_node_db(),
    {ok, RouteDb} = node_route:create_route_db(),
    self() ! bootstrap,
    Parent ! {self(), started},
    loop(S#state{parent = Parent, node_db = NodeDb, route_db = RouteDb,
                 node_instance_sup = NodeInstanceSup}).

loop(#state{parent = Parent,
            node_db = NodeDb,
	    route_db = RouteDb,
            ttl = _TTL,
            peer_nas = PeerNas,
            node_instance_sup = NodeInstanceSup,
            node_path_cost_serv = NodePathCostServ,
            directory_server = DsIpAddressPort,
            na = {NaIpAddress, _NaPort} = Na,
	    oa = Oa,
	    public_key = PublicKey,
	    private_key = _PrivateKey,
            number_of_peers = NumberOfPeers,
            refresh_peers_timeout = RefreshPeersTimeout,
            recalc_timeout = RecalcTimeout,
            auto_recalc = AutoRecalc} = S) ->
    receive
        config_updated ->
            ?daemon_log("Configuration changed...", []),
            loop(read_config(S));
        bootstrap ->
            case ds_jsonrpc:publish_peer(
                   NaIpAddress, DsIpAddressPort,
                   #peer{na = Na, public_key = PublicKey}) of
                {ok, UpdatedTTL} ->
                    ?daemon_log("Published node address ~s",
                                [net_tools:string_address(Na)]),
                    case ds_jsonrpc:reserve_oa(
                           NaIpAddress, DsIpAddressPort, Oa, Na) of
                        ok ->
                            ?daemon_log("Reserved overlay address ~s",
                                        [net_tools:string_address(Oa)]),
                            %% patrik: init psp
                            Re =
                                #route_entry{oa = Oa, na = Na, path_cost = 0,
                                             psp = <<"foo">>},
                            got_new =
                                node_route:update_route_entry(RouteDb, Re),
                            self() ! refresh_peers,
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
                               [net_tools:string_address(Oa)]),
                            timelib:start_timer(
                              ?FIVE_SECONDS_TIMEOUT, bootstrap),
                            loop(S)
                    end;
                {error, Reason}->
                    ?dbg_log(Reason),
                    ?daemon_log(
                       "Could not publish node address ~s. Retrying in five "
                       "seconds...", [net_tools:string_address(Na)]),
                    timelib:start_timer(?FIVE_SECONDS_TIMEOUT, bootstrap),
                    loop(S)
            end;
        republish_self ->
            case ds_jsonrpc:publish_peer(
                   NaIpAddress, DsIpAddressPort,
                   #peer{na = Na, public_key = PublicKey}) of
                {ok, UpdatedTTL} ->
                    ?daemon_log("Republished node address ~s",
                                [net_tools:string_address(Na)]),
                    timelib:start_timer(trunc(UpdatedTTL/2), republish_self),
                    loop(S#state{ttl = UpdatedTTL});
                {error, Reason}->
                    ?dbg_log(Reason),
                    ?daemon_log(
                       "Could not publish node address ~s. Retrying in five "
                       "seconds...", [net_tools:string_address(Na)]),
                    timelib:start_timer(?FIVE_SECONDS_TIMEOUT, republish_self),
                    loop(S)
            end;
        refresh_peers ->
            ?daemon_log("Peer refresh started", []),
            case refresh_peers(NodeDb, RouteDb, PeerNas, NodeInstanceSup,
                               DsIpAddressPort, Na, NumberOfPeers,
                               AutoRecalc) of
                {ok, UpdatedPeerNas} ->
                    timelib:start_timer(RefreshPeersTimeout, refresh_peers),
                    ok = node_path_cost_serv:updated_peer_nas(
                           NodePathCostServ, UpdatedPeerNas),
                    loop(S#state{peer_nas = UpdatedPeerNas});
                {error, Reason} ->
                    ?dbg_log(Reason),
                    ?daemon_log(
                       "Could not refresh peers. Retrying in five seconds...",
                       []),
                    timelib:start_timer(?FIVE_SECONDS_TIMEOUT, refresh_peers),
                    loop(S)
            end;
	{From, stop} ->
	    ok = node_route:delete_node_db(NodeDb),
	    ok = node_route:delete_route_db(RouteDb),
	    From ! {self(), ok};
        {handshake, {node_send_serv, PeerNa, NodeSendServ}} ->
            ok = node_route:add_node_send_serv(NodeDb, PeerNa, NodeSendServ),
            loop(S);
        {From, {handshake, node_recv_serv}} ->
	    From ! {self(), {ok, NodeDb, RouteDb}},
            loop(S);
        {From, {handshake, {node_path_cost_serv, NewNodePathCostServ}}} ->
	    From ! {self(), {ok, NodeDb, RouteDb}},
            loop(S#state{node_path_cost_serv = NewNodePathCostServ});
	{From, get_route_entries} ->
	    {ok, Res} = node_route:get_route_entries(RouteDb),
	    From ! {self(), {ok, Res}},
	    loop(S);
	#route_entry{oa = Oa} ->
	    loop(S);
        #route_entry{na = ViaNa} = Re ->
            case handle_route_entry(
                   NodeDb, RouteDb, PeerNas, NodeInstanceSup, NodePathCostServ,
                   DsIpAddressPort, Na, Oa, Re) of
                {ok, UpdatedPeerNas} ->
                    loop(S#state{peer_nas = UpdatedPeerNas});
                {error, unknown_peer} ->
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
                        [net_tools:string_address(Na)]),
            ok = node_route:recalc(Na, NodeDb, RouteDb),
            if
                AutoRecalc ->
                    RandomRecalcTimeout = random:uniform(RecalcTimeout),
                    timelib:start_timer(RandomRecalcTimeout, recalc),
                    loop(S);
                true ->
                    loop(S)
            end;
	{path_cost, PeerNa, Pc}  ->
	    ok = node_route:update_path_cost(NodeDb, PeerNa, Pc),
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
    NodeInstance = ?config([nodes, {'node-address', S#state.na}]),
    read_config(S, NodeInstance).

read_config(S, []) ->
    S;
read_config(S, [{'directory-server', Value}|Rest]) ->
    read_config(S#state{directory_server = Value}, Rest);
read_config(S, [{'node-address', Value}|Rest]) ->
    read_config(S#state{na = Value}, Rest);
read_config(S, [{'overlay-addresses', [Oa]}|Rest]) ->
    read_config(S#state{oa = Oa}, Rest);
read_config(S, [{'public-key', Value}|Rest]) ->
    read_config(S#state{public_key = Value}, Rest);
read_config(S, [{'private-key', Value}|Rest]) ->
    read_config(S#state{private_key = Value}, Rest);
read_config(S, [{'number-of-peers', Value}|Rest]) ->
    read_config(S#state{number_of_peers = Value}, Rest);
read_config(S, [{'refresh-peers-timeout', Value}|Rest]) ->
    read_config(S#state{refresh_peers_timeout = Value}, Rest);
read_config(S, [{'recalc-timeout', Value}|Rest]) ->
    read_config(S#state{recalc_timeout = Value}, Rest);
read_config(S, [{'auto-recalc', Value}|Rest]) ->
    read_config(S#state{auto_recalc = Value}, Rest);
read_config(S, [{'path-cost', _}|Rest]) ->
    read_config(S, Rest).

%%%
%%% refresh_peers
%%%

refresh_peers(NodeDb, RouteDb, PeerNas, NodeInstanceSup, DsIpAddressPort,
              {NaIpAddress, _NaPort} = Na, NumberOfPeers, AutoRecalc) ->
    ?daemon_log("Known peers: ~s", [net_tools:string_addresses(PeerNas)]),
    case ds_jsonrpc:published_peers(NaIpAddress, DsIpAddressPort, PeerNas) of
        {ok, PublishedPeerNas} ->
            ?daemon_log("Still published peers: ~s",
                        [net_tools:string_addresses(PublishedPeerNas)]),
            UnreachableNodes = node_route:unreachable_nodes(NodeDb),
            UnreachablePeerNas = [Node#node.na || Node <- UnreachableNodes],
            ?daemon_log("Unreachable peers: ~s",
                        [net_tools:string_addresses(UnreachablePeerNas)]),
            RemainingPeerNas = PublishedPeerNas--UnreachablePeerNas,
            ?daemon_log("Remaining published and reachable peers: ~s",
                        [net_tools:string_addresses(RemainingPeerNas)]),
            case NumberOfPeers-length(RemainingPeerNas) of
                NumberOfMissingPeers when NumberOfMissingPeers > 0 ->
                    ?daemon_log("Need ~w additional peers...",
                                [NumberOfMissingPeers]),
                    get_more_peers(NodeDb, RouteDb, PeerNas, NodeInstanceSup,
                                   DsIpAddressPort, Na, AutoRecalc,
                                   RemainingPeerNas, NumberOfMissingPeers);
                _ ->
                    {ok, RemainingPeerNas}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_more_peers(NodeDb, RouteDb, PeerNas, NodeInstanceSup, DsIpAddressPort,
               {NaIpAddress, _NaPort} = Na, AutoRecalc, RemainingPeerNas,
               NumberOfMissingPeers) ->
    case ds_jsonrpc:get_random_peers(
           NaIpAddress, DsIpAddressPort, Na, NumberOfMissingPeers) of
        {ok, NewPeers} ->
            NewPeerNas = [NewPeer#peer.na || NewPeer <- NewPeers],
            ?daemon_log("Found ~w new peers: ~s",
                        [NumberOfMissingPeers,
                         net_tools:string_addresses(NewPeerNas)]),
            purge_peers(NodeDb, RouteDb, NodeInstanceSup, RemainingPeerNas,
                        PeerNas),
            lists:foreach(
              fun(#peer{na = PeerNa, public_key = PeerPublicKey}) ->
                      {ok, NodeSendServ} =
                          node_send_sup:start_node_send_serv(
                            Na, PeerNa, NodeInstanceSup),
                      Node = #node{
                        na = PeerNa,
                        public_key = PeerPublicKey,
                        flags = ?F_NODE_UPDATED bor ?F_NODE_IS_INCOMING_PEER,
                        node_send_serv = NodeSendServ},
                      ok = node_route:add_node(NodeDb, Node)
              end, NewPeers),
            UpdatedPeerNas = NewPeerNas++RemainingPeerNas,
            if
                AutoRecalc ->
                    ok = node_route:recalc(Na, NodeDb, RouteDb),
                    {ok, UpdatedPeerNas};
                true ->
                    {ok, UpdatedPeerNas}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

purge_peers(_NodeDb, _RouteDb, _NodeInstanceSup, _RemainingPeerNas, []) ->
    ok;
purge_peers(NodeDb, RouteDb, NodeInstanceSup, RemainingPeerNas,
            [PeerNa|Rest]) ->
    case lists:member(PeerNa, RemainingPeerNas) of
        false ->
            ok = node_route:delete_node(NodeDb, PeerNa),
            ok = node_send_sup:stop_node_send_serv(PeerNa, NodeInstanceSup),
            ok = node_route:update_path_costs(RouteDb, PeerNa, -1),
            purge_peers(NodeDb, RouteDb, NodeInstanceSup, RemainingPeerNas,
                        Rest);
        true ->
            purge_peers(NodeDb, RouteDb, NodeInstanceSup,
                        lists:delete(PeerNa, RemainingPeerNas), Rest)
    end.

%%%
%%% handle incoming #route_entry{}
%%%

handle_route_entry(NodeDb, RouteDb, PeerNas, NodeInstanceSup, NodePathCostServ,
                   DsIpAddressPort, {NaIpAddress, _NaPort} = Na, Oa,
                   #route_entry{na = ViaNa, path_cost = Pc} = Re) ->
    case node_route:is_member_node(NodeDb, ViaNa) of
        true ->
            update_route_entry(RouteDb, PeerNas, Na, Oa, Re);
        false ->
            case ds_jsonrpc:get_peer(NaIpAddress, DsIpAddressPort, ViaNa) of
                {ok, #peer{public_key = PublicKey}} ->
                    ?daemon_log("~s adding peer ~s",
                                [net_tools:string_address(Oa),
                                 net_tools:string_address(ViaNa)]),
                    UpdatedPeerNas = [ViaNa|PeerNas],
                    ok = node_path_cost_serv:updated_peer_nas(
                           NodePathCostServ, UpdatedPeerNas),
                    {ok, NodeSendServ} =
                        node_send_sup:start_node_send_serv(
                          Na, ViaNa, NodeInstanceSup),
                    Node = #node{na = ViaNa, public_key = PublicKey,
                                 path_cost = Pc, flags = ?F_NODE_UPDATED,
                                 node_send_serv = NodeSendServ},
                    ok = node_route:add_node(NodeDb, Node),
                    update_route_entry(RouteDb, UpdatedPeerNas, Na, Oa, Re);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

update_route_entry(RouteDb, UpdatedPeerNas, Na, Oa, 
                   #route_entry{oa = DestOa, na = ViaNa, path_cost = Pc,
                                hops = Hops} = Re) ->
    %% patrik: lists:member/2 should return the same result as you
    %% when you look into #route_entry.psp, see node_route.hrl
    case lists:member(Na, Hops) of
        true ->
            ?dbg_log({loop_rejected, Na, Hops}),
            ?daemon_log(
               "~s rejected looping route entry: ~s -> ~s (~w)",
               [net_tools:string_address(Oa), net_tools:string_address(DestOa),
                net_tools:string_address(ViaNa), Pc]),
            {ok, UpdatedPeerNas};
        false ->
            case node_route:update_route_entry(RouteDb, Re) of
                {updated, #route_entry{path_cost = CurrentPc}} ->
                    ?daemon_log(
                       "~s updated existing route: ~s -> ~s (~w, ~w) with new "
                       "path cost ~w",
                       [net_tools:string_address(Oa),
                        net_tools:string_address(DestOa),
                        net_tools:string_address(ViaNa), CurrentPc, Hops, Pc]),
                    {ok, UpdatedPeerNas};
                {kept, _CurrentRe} ->
                    ?daemon_log(
                       "~s kept existing route: ~s -> ~s (~w, ~w)",
                       [net_tools:string_address(Oa),
                        net_tools:string_address(DestOa),
                        net_tools:string_address(ViaNa), Pc, Hops]),
                    {ok, UpdatedPeerNas};
                got_new ->
                    ?daemon_log(
                       "~s got new route: ~s -> ~s (~w, ~w)",
                       [net_tools:string_address(Oa),
                        net_tools:string_address(DestOa),
                        net_tools:string_address(ViaNa), Pc, Hops]),
                    {ok, UpdatedPeerNas};
                {got_better,
                 #route_entry{
                   na = CurrentViaNa, path_cost = CurrentPc,
                   hops = CurrentHops}} ->
                    ?daemon_log(
                       "~s got better route: ~s -> ~s (~w, ~w) "
                       "replacing ~s -> ~s (~w, ~w)",
                       [net_tools:string_address(Oa),
                        net_tools:string_address(DestOa),
                        net_tools:string_address(ViaNa), Pc, Hops,
                        net_tools:string_address(DestOa),
                        net_tools:string_address(CurrentViaNa),
                        CurrentPc, CurrentHops]),
                    {ok, UpdatedPeerNas};
                {got_worse,
                 #route_entry{
                   na = CurrentViaNa, path_cost = CurrentPc,
                   hops = CurrentHops}} ->
                    ?daemon_log(
                       "~s got worse route: ~s -> ~s (~w, ~w) "
                       "not replacing ~s -> ~s (~w, ~w)",
                       [net_tools:string_address(Oa),
                        net_tools:string_address(DestOa),
                        net_tools:string_address(ViaNa), Pc, Hops,
                        net_tools:string_address(DestOa),
                        net_tools:string_address(CurrentViaNa), CurrentPc,
                        CurrentHops]),
                    {ok, UpdatedPeerNas}
            end
    end.
