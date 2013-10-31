-module(node_serv).

%%% external exports
-export([start_link/3, stop/1, stop/2]).
-export([get_routing_entries/1, update_routing_entry/2]).
-export([enable_recalc/1, disable_recalc/1, recalc/1]).
-export([update_link_quality/3]).

%%% internal exports
-export([init/4]).

%%% include files
-include_lib("util/include/log.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/shorthand.hrl").
-include_lib("node/include/node.hrl").
-include_lib("node/include/node_route.hrl").
-include_lib("ds/include/ds.hrl").

%%% constants
-define(STATIC_LINK_QUALITY, 10).
-define(BOOTSTRAP_TIMEOUT, 1000*2).
-define(FIVE_SECONDS_TIMEOUT, 1000*5).

%%% records
-record(state, {
	  parent                       :: pid(),
	  oa                           :: oa(),
          ip                           :: ip(),
          ttl                          :: integer(),
          peer_ips                     :: [ip()],
	  public_key                   :: public_key:rsa_public_key(),
          node_db                      :: node_db(),
          routing_db                   :: routing_db(),
          auto_recalc                  :: boolean(),
          simulation                   :: boolean(),
          number_of_peers              :: integer(),
          measure_link_quality_timeout :: integer(),
          refresh_peers_timeout        :: integer(),
          recalc_timeout               :: integer()
	 }).

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link(oa(), public_key:rsa_public_key(), boolean()) ->
			{'ok', ip()}.

start_link(Oa, PublicKey, AutoRecalc) ->
    Args = [self(), Oa, PublicKey, AutoRecalc],
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
%%% exported: update_routing_entry
%%%

-spec update_routing_entry(ip(), #routing_entry{}) -> 'ok'.

update_routing_entry(Ip, Re) ->
    Ip ! Re,
    ok.

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
%% exported: update_link_quality
%%%

-spec update_link_quality(ip(), ip(), link_quality()) -> 'ok'.

update_link_quality(Ip, PeerIp, LinkQuality) ->
    Ip ! {link_quality, PeerIp, LinkQuality},
    ok.

%%%
%%% server loop
%%%

init(Parent, Oa, PublicKey, AutoRecalc) ->
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
    SelfRe = #routing_entry{oa = Oa, ip = Ip, link_quality = 0},
    ok = node_route:update_routing_entry(RoutingDb, SelfRe),
    timelib:start_timer(
      S#state.measure_link_quality_timeout, measure_link_quality),
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
                 node_db = NodeDb,
                 routing_db = RoutingDb,
                 auto_recalc = AutoRecalc}).

loop(#state{parent = Parent,
	    oa = Oa,
            ip = Ip,
            peer_ips = PeerIps,
            ttl = _TTL,
	    public_key = PublicKey,
            node_db = NodeDb,
	    routing_db = RoutingDb,
            auto_recalc = AutoRecalc,
            simulation = Simulation,
            number_of_peers = NumberOfPeers,
            measure_link_quality_timeout = MeasureLinkQualityTimeout,
            refresh_peers_timeout = RefreshPeersTimeout,
            recalc_timeout = RecalcTimeout} = S) ->
    receive
        bootstrap ->
            UpdatedPeerIps =
                refresh_peers(
                  Ip, PeerIps, NodeDb, RoutingDb, Simulation, NumberOfPeers,
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
                refresh_peers(Ip, PeerIps, NodeDb, RoutingDb, Simulation,
                              NumberOfPeers, RefreshPeersTimeout),
            loop(S#state{peer_ips = UpdatedPeerIps});
        measure_link_quality ->
            RotatedPeerIps = rotate_peer_ips(PeerIps),
            PeerIp = hd(RotatedPeerIps),
            Self = self(),
            spawn(
              fun() ->
                      measure_link_quality(Ip, PeerIp, Simulation),
                      timelib:start_timer(MeasureLinkQualityTimeout, Self,
                                          measure_link_quality)
              end),
            loop(S#state{peer_ips = RotatedPeerIps});
	{From, stop} ->
	    ok = node_route:delete_node_db(NodeDb),
	    ok = node_route:delete_routing_db(RoutingDb),
	    From ! {self(), ok};
	{From, get_routing_entries} ->
	    {ok, RoutingTable} = node_route:get_routing_entries(RoutingDb),
	    From ! {self(), {ok, RoutingTable}},
	    loop(S);
	#routing_entry{oa = Oa} ->
	    loop(S);
	#routing_entry{oa = DestOa, ip = ViaIp, link_quality = LinkQuality,
                       hops = Hops} = Re ->
            case node_route:member_node(NodeDb, ViaIp) of
                true ->
                    ok;
                false->
                    ?daemon_log("~w added incoming peer ~w.", [Oa, ViaIp]),
                    Node = #node{ip = ViaIp, flags = ?F_NODE_UPDATED},
                    ok = node_route:add_node(NodeDb, Node)
            end,
            case lists:member(Ip, Hops) of
                true ->
                    ?daemon_log(
                       "~w rejected looping routing entry: ~w -> ~w (~w)",
                       [Oa, DestOa, ViaIp, LinkQuality]),
                    loop(S);
                false ->
                    ?daemon_log("~w accepted routing entry: ~w -> ~w (~w)",
                                [Oa, DestOa, ViaIp, LinkQuality]),
                    ok = node_route:update_routing_entry(RoutingDb, Re),
                    loop(S)
            end;
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
	{link_quality, PeerIp, LinkQuality}  ->
	    ok = node_route:update_link_quality(NodeDb, PeerIp, LinkQuality),
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
    [Simulation] = ?cfg([simulation]),
    [NumberOfPeers] = ?cfg([node, 'number-of-peers']),
    [MeasureLinkQualityTimeout] = ?cfg([node, 'measure-link-quality-timeout']),
    [RefreshPeersTimeout] = ?cfg([node, 'refresh-peers-timeout']),
    [RecalcTimeout] = ?cfg([node, 'recalc-timeout']),
    S#state{simulation = Simulation,
            number_of_peers = NumberOfPeers,
            measure_link_quality_timeout = MeasureLinkQualityTimeout,
            refresh_peers_timeout = RefreshPeersTimeout,
            recalc_timeout = RecalcTimeout}.

%%%
%%% link quality measurements
%%%

measure_link_qualities(_Ip, [], _Simulation) ->
    ok;
measure_link_qualities(Ip, [#peer{ip = PeerIp}|Rest], Simulation = true) ->
    {ok, LinkQuality} = overseer_serv:get_link_quality(Ip, PeerIp),
    Ip ! {link_quality, PeerIp, LinkQuality},
    measure_link_qualities(Ip, Rest, Simulation);
measure_link_qualities(Ip, [#peer{ip = PeerIp}|Rest], Simulation = false) ->
    %% to be implemented
    Ip ! {link_quality, PeerIp, ?STATIC_LINK_QUALITY},
    measure_link_qualities(Ip, Rest, Simulation).

rotate_peer_ips([]) ->
    [];
rotate_peer_ips([PeerIp|Rest]) ->
    lists:reverse([PeerIp|lists:reverse(Rest)]).

measure_link_quality(Ip, PeerIp, _Simulation = true) ->
    {ok, LinkQuality} = overseer_serv:get_link_quality(Ip, PeerIp),
    Ip ! {link_quality, PeerIp, LinkQuality};
measure_link_quality(_Ip, _PeerIp, _Simulation = false) ->
    %% to be implemented
    ok.

%%
%% refresh_peers
%%

refresh_peers(Ip, PeerIps, NodeDb, RoutingDb, Simulation, NumberOfPeers,
              RefreshPeersTimeout) ->
    ?daemon_log("Known peers: ~w", [PeerIps]),
    {ok, PublishedPeerIps} = ds_serv:published_peers(PeerIps),
    ?daemon_log("Still published peers: ~w", [PublishedPeerIps]),
    UnreachablePeerIps =
        ets:match(NodeDb, #node{ip = '$1', link_quality = -1, _ = '_'}),
    ?daemon_log("Unreachable peers: ~w", [UnreachablePeerIps]),
    RemainingPeerIps = PublishedPeerIps--UnreachablePeerIps,
    ?daemon_log("Remaining published and reachable peers: ~w",
                [RemainingPeerIps]),
    NumberOfMissingPeers = NumberOfPeers-length(RemainingPeerIps),
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
            ?daemon_log("Measures initial link qualities to new nodes...", []),
            spawn(
              fun() ->
                      measure_link_qualities(Ip, NewPeers, Simulation)
              end),
            lists:foreach(
              fun(#peer{ip = PeerIp, public_key = PeerPublicKey}) ->
                      Node = #node{
                        ip = PeerIp,
                        public_key = PeerPublicKey,
                        flags = ?F_NODE_UPDATED bor ?F_NODE_IS_INCOMING_PEER},
                      ok = node_route:add_node(NodeDb, Node)
              end, NewPeers),
            timelib:start_timer(RefreshPeersTimeout, refresh_peers),
            RemainingPeerIps++NewPeerIps
    end.

purge_peers(_NodeDb, _RoutingDb, _RemainingPeerIps, []) ->
    ok;
purge_peers(NodeDb, RoutingDb, RemainingPeerIps, [PeerIp|Rest]) ->
    case lists:member(PeerIp, RemainingPeerIps) of
        false ->
            ok = node_route:delete_node(NodeDb, PeerIp),
            ok = node_route:delete_routing_entry(RoutingDb, {ip, PeerIp}),
            purge_peers(NodeDb, RoutingDb, RemainingPeerIps, Rest);
        true ->
            purge_peers(NodeDb, RoutingDb,
                        lists:delete(PeerIp, RemainingPeerIps), Rest)
    end.
