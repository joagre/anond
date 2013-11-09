-module(node_serv).

%%% external exports
-export([start_link/4, stop/1, stop/2]).
-export([get_routing_entries/1, send_routing_entry/2]).
-export([get_nodes/1]).
-export([enable_recalc/1, disable_recalc/1, recalc/1]).
-export([update_path_cost/3]).

%%% internal exports
-export([init/5]).

%%% include files
-include_lib("util/include/log.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/shorthand.hrl").
-include_lib("node/include/node.hrl").
-include_lib("node/include/node_route.hrl").
-include_lib("ds/include/ds.hrl").

%%% constants
-define(BOOTSTRAP_TIMEOUT, 1000*2).
-define(FIVE_SECONDS_TIMEOUT, 1000*5).

%%% records
-record(state, {
          parent                    :: pid(),
          oa                        :: oa(),
          ip                        :: ip(),
          ttl                       :: integer(),
          peer_ips                  :: [ip()],
	  public_key                :: public_key:rsa_public_key(),
	  private_key               :: public_key:rsa_private_key(),
          node_db                   :: node_db(),
          routing_db                :: routing_db(),
          auto_recalc               :: boolean(),
          number_of_peers           :: integer(),
          measure_path_cost_timeout :: integer(),
          refresh_peers_timeout     :: integer(),
          recalc_timeout            :: integer()
	 }).

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link(oa(), public_key:rsa_public_key(),
                 public_key:rsa_private_key(), boolean()) ->
			{'ok', ip()}.

start_link(Oa, PublicKey, PrivateKey, AutoRecalc) ->
    Args = [self(), Oa, PublicKey, PrivateKey, AutoRecalc],
    Ip = proc_lib:spawn_link(?MODULE, init, Args),
    receive
	{Ip, started} ->
	    {ok, Ip};
	{Ip, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: stop
%%%

-spec stop(ip()) -> 'ok'.

stop(Ip) -> 
    stop(Ip, 15000).

-spec stop(ip(), timeout()) -> 'ok'.

stop(Ip, Timeout) ->
    serv:call(Ip, stop, Timeout).

%%%
%%% exported: get_routing_entries
%%%

-spec get_routing_entries(ip()) -> {'ok', [#routing_entry{}]}.

get_routing_entries(Ip) ->
    serv:call(Ip, get_routing_entries).

%%%
%%% exported: send_routing_entry
%%%

-spec send_routing_entry(ip(), #routing_entry{}) -> 'ok'.

send_routing_entry(Ip, Re) ->
    Ip ! Re,
    ok.

%%%
%%% exported: get_nodes
%%%

-spec get_nodes(ip()) -> {'ok', [#node{}]}.

get_nodes(Ip) ->
    serv:call(Ip, get_nodes).

%%%
%%% exported: enable_recalc
%%%

-spec enable_recalc(ip()) -> 'ok'.

enable_recalc(Ip) ->
    Ip ! enable_recalc,
    ok.

%%%
%%% exported: disable_recalc
%%%

-spec disable_recalc(ip()) -> 'ok'.

disable_recalc(Ip) ->
    Ip ! disable_recalc,
    ok.

%%%
%%% exported: recalc
%%%

-spec recalc(ip()) -> 'ok'.

recalc(Ip) ->
    Ip ! recalc,
    ok.

%%%
%% exported: update_path_cost
%%%

-spec update_path_cost(ip(), ip(), path_cost()) -> 'ok'.

update_path_cost(Ip, PeerIp, Pc) ->
    Ip ! {path_cost, PeerIp, Pc},
    ok.

%%%
%%% server loop
%%%

init(Parent, Oa, PublicKey, PrivateKey, AutoRecalc) ->
    process_flag(trap_exit, true),
    {A1, A2, A3} = erlang:now(),
    random:seed({A1, A2, A3}),
    S = read_config(#state{}),
    ok = config_serv:subscribe(),
    {ok, NodeDb} = node_route:create_node_db(),
    {ok, RoutingDb} = node_route:create_routing_db(),
    Ip = self(),
    {ok, TTL} = ds_serv:publish_peer(#peer{ip = Ip, public_key = PublicKey}),
    ?daemon_log("Published my ip (~w) on directory server.", [Ip]),
    timelib:start_timer(TTL, republish_self),
    ok = ds_serv:reserve_oa(Oa, Ip),
    ?daemon_log("Reserved my oa (~w) on directory server.", [Oa]),
    %% patrik: init psp?
    SelfRe = #routing_entry{oa = Oa, ip = Ip, path_cost = 0, psp = <<"foo">>},
    got_new = node_route:update_routing_entry(RoutingDb, SelfRe),
    timelib:start_timer(S#state.measure_path_cost_timeout, measure_path_cost),
    ?daemon_log("Peer refresh started...", []),
    timelib:start_timer(?BOOTSTRAP_TIMEOUT, bootstrap),
    if
        AutoRecalc ->
            RandomRecalcTimeout = random:uniform(S#state.recalc_timeout),
            timelib:start_timer(RandomRecalcTimeout, recalc);
        true ->
            ok
    end,
    Parent ! {self(), started},
    loop(S#state{parent = Parent,
                 oa = Oa,
                 ip = Ip,
                 peer_ips = [],
                 ttl = TTL,
                 public_key = PublicKey,
                 private_key = PrivateKey,
                 node_db = NodeDb,
                 routing_db = RoutingDb,
                 auto_recalc = AutoRecalc}).

loop(#state{parent = Parent,
	    oa = Oa,
            ip = Ip,
            peer_ips = PeerIps,
            ttl = _TTL,
	    public_key = PublicKey,
	    private_key = _PrivateKey,
            node_db = NodeDb,
	    routing_db = RoutingDb,
            auto_recalc = AutoRecalc,
            number_of_peers = NumberOfPeers,
            measure_path_cost_timeout = MeasurePcTimeout,
            refresh_peers_timeout = RefreshPeersTimeout,
            recalc_timeout = RecalcTimeout} = S) ->
    receive
        bootstrap ->
            UpdatedPeerIps =
                refresh_peers(
                  Ip, PeerIps, NodeDb, RoutingDb, AutoRecalc, NumberOfPeers,
                  RefreshPeersTimeout),
            loop(S#state{peer_ips = UpdatedPeerIps});
        config_updated ->
            ?daemon_log("Configuration changed...", []),
            loop(read_config(S));
        republish_self ->
            {ok, UpdatedTTL} =
                ds_serv:publish_peer(#peer{ip = Ip, public_key = PublicKey}),
            ?daemon_log("Republished itself on the directory server.", []),
            timelib:start_timer(UpdatedTTL, republish_self),
            loop(S#state{ttl = UpdatedTTL});
        refresh_peers ->
            ?daemon_log("Peer refresh started...", []),
            UpdatedPeerIps = 
                refresh_peers(Ip, PeerIps, NodeDb, RoutingDb, AutoRecalc,
                              NumberOfPeers, RefreshPeersTimeout),
            loop(S#state{peer_ips = UpdatedPeerIps});
        measure_path_cost ->
            PeerIp = hd(PeerIps),
            RotatedPeerIps = rotate_peer_ips(PeerIps),
            Self = self(),
            spawn(
              fun() ->
                      measure_path_cost(Ip, PeerIp),
                      timelib:start_timer(MeasurePcTimeout, Self,
                                          measure_path_cost)
              end),
            loop(S#state{peer_ips = RotatedPeerIps});
	{From, stop} ->
	    ok = node_route:delete_node_db(NodeDb),
	    ok = node_route:delete_routing_db(RoutingDb),
	    From ! {self(), ok};
	{From, get_routing_entries} ->
	    {ok, Res} = node_route:get_routing_entries(RoutingDb),
	    From ! {self(), {ok, Res}},
	    loop(S);
	#routing_entry{oa = Oa} ->
	    loop(S);
        #routing_entry{oa = DestOa, ip = ViaIp, path_cost = Pc,
                       hops = Hops} = Re ->
            case node_route:is_member_node(NodeDb, ViaIp) of
                true ->
                    UpdatedPeerIps = PeerIps;
                false ->
                    ?daemon_log("~w added incoming peer ~w.", [Oa, ViaIp]),
                    Node = #node{ip = ViaIp, path_cost = Pc,
                                 flags = ?F_NODE_UPDATED},
                    UpdatedPeerIps = [ViaIp|PeerIps],
                    ok = node_route:add_node(NodeDb, Node)
            end,
            %% patrik: lists:member/2 should return the same result as you
            %% when you look into #routing_entry.psp, see node_route.hrl
            case lists:member(Ip, Hops) of
                true ->
                    ?dbg_log({loop_rejected, Ip, Hops}),
                    ?daemon_log(
                       "~w rejected looping routing entry: ~w -> ~w (~w)",
                       [Oa, DestOa, ViaIp, Pc]),
                    loop(S#state{peer_ips = UpdatedPeerIps});
                false ->
                    case node_route:update_routing_entry(RoutingDb, Re) of
                        {updated, #routing_entry{path_cost = CurrentPc}} ->
                            ?daemon_log(
                               "~w updated existing route: ~w -> ~w (~w, ~w) "
                               "with new path cost ~w",
                               [Oa, DestOa, ViaIp, CurrentPc, Hops, Pc]),
                            loop(S#state{peer_ips = UpdatedPeerIps});
                        {kept, _CurrentRe} ->
                            ?daemon_log(
                               "~w kept existing route: ~w -> ~w (~w, ~w)",
                               [Oa, DestOa, ViaIp, Pc, Hops]),
                            loop(S#state{peer_ips = UpdatedPeerIps});
                        got_new ->
                            ?daemon_log(
                               "~w got new route: ~w -> ~w (~w, ~w)",
                               [Oa, DestOa, ViaIp, Pc, Hops]),
                            loop(S#state{peer_ips = UpdatedPeerIps});
                        {got_better,
                         #routing_entry{
                           ip = CurrentViaIp, path_cost = CurrentPc,
                           hops = CurrentHops}} ->
                            ?daemon_log(
                               "~w got better route: ~w -> ~w (~w, ~w) "
                               "replacing ~w -> ~w (~w, ~w)",
                               [Oa, DestOa, ViaIp, Pc, Hops,
                                DestOa, CurrentViaIp, CurrentPc, CurrentHops]),
                            loop(S#state{peer_ips = UpdatedPeerIps});
                        {got_worse,
                         #routing_entry{
                           ip = CurrentViaIp, path_cost = CurrentPc,
                           hops = CurrentHops}} ->
                            ?daemon_log(
                               "~w got worse route: ~w -> ~w (~w, ~w) "
                               "not replacing ~w -> ~w (~w, ~w)",
                               [Oa, DestOa, ViaIp, Pc, Hops,
                                DestOa, CurrentViaIp, CurrentPc, CurrentHops]),
                            loop(S#state{peer_ips = UpdatedPeerIps})
                    end
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
	    ?daemon_log("** ~w recalculates its routing table.", [Oa]),
            ok = node_route:recalc(Ip, NodeDb, RoutingDb),
            if
                AutoRecalc ->
                    RandomRecalcTimeout = random:uniform(RecalcTimeout),
                    timelib:start_timer(RandomRecalcTimeout, recalc),
                    loop(S);
                true ->
                    loop(S)
            end;
	{path_cost, PeerIp, Pc}  ->
	    ok = node_route:update_path_cost(NodeDb, PeerIp, Pc),
	    loop(S);
	{'EXIT', Parent, Reason} ->
	    ok = node_route:delete_node_db(NodeDb),
	    ok = node_route:delete_routing_db(RoutingDb),
            exit(Reason);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

%%%
%%% init
%%%

read_config(S) ->
    [NumberOfPeers] = ?cfg([node, 'number-of-peers']),
    [MeasurePcTimeout] = ?cfg([node, 'measure-path-cost-timeout']),
    [RefreshPeersTimeout] = ?cfg([node, 'refresh-peers-timeout']),
    [RecalcTimeout] = ?cfg([node, 'recalc-timeout']),
    S#state{number_of_peers = NumberOfPeers,
            measure_path_cost_timeout = MeasurePcTimeout,
            refresh_peers_timeout = RefreshPeersTimeout,
            recalc_timeout = RecalcTimeout}.

%%%
%%% path cost measurements
%%%

measure_path_costs(_Ip, []) ->
    ok;
measure_path_costs(Ip, [#peer{ip = PeerIp}|Rest]) ->
    {ok, Pc} = overseer_serv:get_path_cost(Ip, PeerIp),
    Ip ! {path_cost, PeerIp, Pc},
    measure_path_costs(Ip, Rest).

rotate_peer_ips([PeerIp|Rest]) ->
    lists:reverse([PeerIp|lists:reverse(Rest)]).

measure_path_cost(Ip, PeerIp) ->
    {ok, Pc} = overseer_serv:get_path_cost(Ip, PeerIp),
    Ip ! {path_cost, PeerIp, Pc}.

%%%
%%% refresh_peers
%%%

refresh_peers(Ip, PeerIps, NodeDb, RoutingDb, AutoRecalc, NumberOfPeers,
              RefreshPeersTimeout) ->
    ?daemon_log("Known peers: ~w", [PeerIps]),
    {ok, PublishedPeerIps} = ds_serv:published_peers(PeerIps),
    ?daemon_log("Still published peers: ~w", [PublishedPeerIps]),
    UnreachableNodes = node_route:unreachable_nodes(NodeDb),
    UnreachablePeerIps = [Node#node.ip || Node <- UnreachableNodes],
    ?daemon_log("Unreachable peers: ~w", [UnreachablePeerIps]),
    RemainingPeerIps = PublishedPeerIps--UnreachablePeerIps,
    ?daemon_log("Remaining published and reachable peers: ~w",
                [RemainingPeerIps]),
    case NumberOfPeers-length(RemainingPeerIps) of
        NumberOfMissingPeers when NumberOfMissingPeers > 0 ->
            ?daemon_log("Need ~w additional peers...", [NumberOfMissingPeers]),
            case ds_serv:get_random_peers(Ip, NumberOfMissingPeers) of
                {error, too_few_peers} ->
                    ?daemon_log("Can not find ~w additional peers. "
                                "Retrying in five seconds...",
                                [NumberOfMissingPeers]),
                    timelib:start_timer(?FIVE_SECONDS_TIMEOUT, refresh_peers);
                {ok, NewPeers} ->
                    NewPeerIps = [NewPeer#peer.ip || NewPeer <- NewPeers],
                    ?daemon_log("Found ~w new peers: ~w", [NumberOfMissingPeers,
                                                           NewPeerIps]),
                    purge_peers(NodeDb, RoutingDb, RemainingPeerIps, PeerIps),
                    ?daemon_log("Measures initial path costs to new nodes...",
                                []),
                    spawn(
                      fun() ->
                              measure_path_costs(Ip, NewPeers)
                      end),
                    lists:foreach(
                      fun(#peer{ip = PeerIp, public_key = PeerPublicKey}) ->
                              Node = #node{
                                ip = PeerIp,
                                public_key = PeerPublicKey,
                                flags = ?F_NODE_UPDATED bor
                                    ?F_NODE_IS_INCOMING_PEER},
                              ok = node_route:add_node(NodeDb, Node)
                      end, NewPeers),
                    if
                        AutoRecalc ->
                            ok = node_route:recalc(Ip, NodeDb, RoutingDb);
                        true ->
                            ok
                    end,
                    timelib:start_timer(RefreshPeersTimeout, refresh_peers),
                    RemainingPeerIps++NewPeerIps
            end;
        _ ->
            ok
    end.

purge_peers(_NodeDb, _RoutingDb, _RemainingPeerIps, []) ->
    ok;
purge_peers(NodeDb, RoutingDb, RemainingPeerIps, [PeerIp|Rest]) ->
    case lists:member(PeerIp, RemainingPeerIps) of
        false ->
            ok = node_route:delete_node(NodeDb, PeerIp),            
            ok = node_route:update_path_costs(RoutingDb, PeerIp, -1),
            purge_peers(NodeDb, RoutingDb, RemainingPeerIps, Rest);
        true ->
            purge_peers(NodeDb, RoutingDb,
                        lists:delete(PeerIp, RemainingPeerIps), Rest)
    end.
