-module(ds_serv).

%%% external exports
-export([start_link/0, stop/0, stop/1]).
-export([get_number_of_peers/0, get_all_peers/0, get_random_peers/2]).
-export([publish_peer/1, published_peers/1]).
-export([reserve_oa/2]).

%%% internal exports
-export([init/1]).

%%% include files
-include_lib("util/include/log.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/shorthand.hrl").
-include_lib("node/include/node.hrl").
-include_lib("ds/include/ds.hrl").
-include_lib("overseer/include/simulation.hrl").

%%% constants

%%% records
-record(state, {
	  parent           :: pid(),
	  peer_tid         :: ets:tid(),
          oa_tid           :: ets:tid(),
          simulation       :: boolean(),
          peer_ttl         :: integer(),
          max_oas_per_peer :: integer()
	 }).

%%% types

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
%%% exported: get_number_of_peers
%%%

-spec get_number_of_peers() -> {'ok', integer()}.

get_number_of_peers() ->
    serv:call(?MODULE, get_number_of_peers).

%%%
%%% exported: get_all_peers
%%%

-spec get_all_peers() -> {'ok', [#peer{}]}.

get_all_peers() ->
    serv:call(?MODULE, get_all_peers).

%%%
%%% exported: get_random_peers
%%%

-spec get_random_peers(ip(), N :: integer() | {percent, N :: integer()}) ->
                              {'ok', [#peer{}]} |
                              {'error', 'too_few_peers'}.

get_random_peers(Ip, N) ->
    serv:call(?MODULE, {get_random_peers, Ip, N}).

%%%
%%% exported: publish_peer
%%%

-spec publish_peer(#peer{}) -> {'ok', PeerTTL :: integer()}.

publish_peer(Peer) ->
    serv:call(?MODULE, {publish_peer, Peer}).

%%%
%%% exported: member_peers
%%%

-spec published_peers([ip()]) -> {'ok', [ip()]}.

published_peers(PeerIps) ->
    serv:call(?MODULE, {published_peers, PeerIps}).

%%%
%%% exported: reserve_oa
%%%

-spec reserve_oa(oa(), ip()) -> 'ok' | 'no_such_peer' | 'too_many_oas'.

reserve_oa(Oa, Ip) ->
    serv:call(?MODULE, {reserve_oa, Oa, Ip}).

%%%
%%% server loop
%%%

init(Parent) ->
    process_flag(trap_exit, true),
    case catch register(?MODULE, self()) of
        true ->
            {A1, A2, A3} = erlang:now(),
            random:seed({A1, A2, A3}),
            S = read_config(#state{}),
            ok = config_serv:subscribe(),
	    PeerTid = ets:new(peer_db, [{keypos, 2}]),
	    OaTid = ets:new(oa_db, [bag]),
            timelib:start_timer(S#state.peer_ttl, enforce_peer_ttl),
	    Parent ! {self(), started},
	    loop(S#state{parent = Parent,
                         peer_tid = PeerTid,
                         oa_tid = OaTid});
        _ ->
            Parent ! {self(), already_started}
    end.

loop(#state{parent = Parent,
            peer_tid = PeerTid,
            oa_tid = OaTid,
            simulation = Simulation,
            peer_ttl = PeerTTL,
            max_oas_per_peer = MaxOasPerPeer} = S) ->
    receive
        config_updated ->
            ?daemon_log("Configuration changed...", []),
            loop(read_config(S));
        enforce_peer_ttl ->
            ?daemon_log("Looking for stale peers...", []),
            StalePeerIps =
                ets:foldl(
                  fun(#peer{ip = Ip, last_updated = LastUpdated}, Acc) ->
                          Delta = timelib:ugnow_delta(LastUpdated),
                          if
                              Delta > PeerTTL ->
                                  [Ip|Acc];
                              true ->
                                  []
                          end
                  end, [], PeerTid),
            lists:foreach(fun(Ip) ->
                                  true = ets:delete(PeerTid, Ip),
                                  true = ets:match_delete(OaTid, {'_', Ip}),
                                  ?daemon_log("Removed stale peer ~w.", [Ip])
                          end, StalePeerIps),
            timelib:start_timer(PeerTTL, enforce_peer_ttl),
            loop(S);
	{From, stop} ->
	    ets:delete(PeerTid),
	    ets:delete(OaTid),
	    From ! {self(), ok};
        {From, get_all_peers} ->
            AllPeers = ets:foldl(fun(Peer, Acc) -> [Peer|Acc] end, [], PeerTid),
            ?daemon_log("All ~w peers returned.", [length(AllPeers)]),
            From ! {self(), {ok, AllPeers}},
            loop(S);
        %% return two non random peers as defined in simulation.hrl
        {From, {get_random_peers, Ip, 2}} when Simulation == true ->
            {SimulatedPeerOas, SimulatedPeers} =
                get_simulated_peers(PeerTid, OaTid, Ip),
            ?daemon_log("~w simulated peers returned: ~w",
                        [length(SimulatedPeerOas), SimulatedPeerOas]),
            RandomPeers = get_random_peers(PeerTid, Ip, 2),
            RandomPeerIps = [RandomPeer#peer.ip || RandomPeer <- RandomPeers],
            ?daemon_log("~w random peers generated but not returned: ~w",
                        [length(RandomPeerIps), RandomPeerIps]),
            From ! {self(), {ok, SimulatedPeers}},
	    loop(S);
        {From, {get_random_peers, Ip, {percent, N}}} ->
            Size = ets:info(PeerTid, size),
            RandomPeers = get_random_peers(PeerTid, Ip, trunc(N/100*Size)),
            RandomPeerIps = [RandomPeer#peer.ip || RandomPeer <- RandomPeers],
            ?daemon_log("~w random peers returned: ~w",
                        [length(RandomPeerIps), RandomPeerIps]),
            From ! {self(), {ok, RandomPeers}},
            loop(S);
        {From, {get_random_peers, Ip, N}} when is_integer(N) ->
            case ets:info(PeerTid, size) of
                Size when N >= Size ->
                    ?daemon_log("~w random peers could not be returned.",
                                [Size]),
                    From ! {self(), {error, too_few_peers}},
                    loop(S);
                _Size ->
                    RandomPeers = get_random_peers(PeerTid, Ip, N),
                    RandomPeerIps =
                        [RandomPeer#peer.ip || RandomPeer <- RandomPeers],
                    ?daemon_log("~w random peers returned: ~w",
                                [length(RandomPeerIps), RandomPeerIps]),
                    From ! {self(), {ok, RandomPeers}},
                    loop(S)
            end;
        {From, {publish_peer, #peer{ip = Ip} = Peer}} ->
            UpdatedPeer = Peer#peer{last_updated = timelib:ugnow()},
            true = ets:insert(PeerTid, UpdatedPeer),
            ?daemon_log("Peer ~w published.", [Ip]),
            From ! {self(), {ok, PeerTTL}},
            loop(S);
        {From, {reserve_oa, Oa, Ip}} ->
            case ets:lookup(PeerTid, Ip) of
                [] ->
                    ?daemon_log("Rejected reservation of overlay address ~w to "
                                "unknown peer ~w.", [Oa, Ip]),
                    From ! {self(), no_such_peer},
                    loop(S);
                _ ->
                    NumberOfOas = length(ets:lookup(OaTid, Oa)),
                    if
                        NumberOfOas =< MaxOasPerPeer ->
                            true = ets:insert(OaTid, {Oa, Ip}),
                            ?daemon_log(
                               "Reserved overlay address ~w to peer ~w.",
                               [Oa, Ip]),
                            From ! {self(), ok},
                            loop(S);
                        true ->
                            ?daemon_log("Peer ~w tried to reserve more than ~w "
                                        "overlay addresses.",
                                        [Ip, MaxOasPerPeer]),
                            From ! {self(), too_many_oas},
                            loop(S)
                    end
            end;
        {From, {published_peers, PeerIps}} ->
            PublishedPeerIps =
                [PeerIp || PeerIp <- PeerIps,
                           ets:member(PeerTid, PeerIp) == true],
            From ! {self(), {ok, PublishedPeerIps}},
            loop(S);
        {'EXIT', Parent, Reason} ->
	    true = ets:delete(PeerTid),
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
    [PeerTTL] = ?cfg(['directory-server', 'peer-ttl']),
    [MaxOasPerPeer] = ?cfg(['directory-server', 'max-oas-per-peer']),
    S#state{simulation = Simulation,
            peer_ttl = PeerTTL,
            max_oas_per_peer = MaxOasPerPeer}.

%%%
%%% get_random_peers
%%%

get_simulated_peers(PeerTid, OaTid, Ip) ->
    [[Oa]] = ets:match(OaTid, {'$1', Ip}),
    {value, {Oa, PeerOas}} = lists:keysearch(Oa, 1, ?NON_RANDOM_PEERS),
    PeerIps = oas_to_ips(OaTid, PeerOas),
    Peers =
        lists:map(fun(PeerIp) ->
                          [Peer] = ets:lookup(PeerTid, PeerIp),
                          Peer
                  end, PeerIps),
    {PeerOas, Peers}.

oas_to_ips(_OaTid, []) ->
    [];
oas_to_ips(OaTid, [Oa|Rest]) ->
    [[Ip]] = ets:match(OaTid, {Oa, '$1'}),
    [Ip|oas_to_ips(OaTid, Rest)].

%% http://en.wikipedia.org/wiki/Reservoir_sampling
get_random_peers(PeerTid, SelfIp, N) ->
    SampleTid = ets:new(peer_sample_db, []),
    NextKey = init_sample(PeerTid, SelfIp, ets:first(PeerTid), N, SampleTid),
    Sample = extract_peer_sample(PeerTid, SelfIp, NextKey, N, SampleTid, N+1),
    ets:delete(SampleTid),
    Sample.

init_sample(_PeerTid, _SelfIp, Ip, 0, _SampleTid) ->
    Ip;
init_sample(PeerTid, SelfIp, SelfIp, N, SampleTid) ->
    init_sample(PeerTid, SelfIp, ets:next(PeerTid, SelfIp), N, SampleTid);
init_sample(PeerTid, SelfIp, Ip, N, SampleTid) ->
    [Peer] = ets:lookup(PeerTid, Ip),
    true = ets:insert(SampleTid, {N, Peer}),
    init_sample(PeerTid, SelfIp, ets:next(PeerTid, Ip), N-1, SampleTid).

extract_peer_sample(_PeerTid, _SelfIp, '$end_of_table', _N, SampleTid, _M) ->
    ets:foldl(fun({_, Peer}, Acc) -> [Peer|Acc] end, [], SampleTid);
extract_peer_sample(PeerTid, SelfIp, SelfIp, N, SampleTid, M) ->
    extract_peer_sample(PeerTid, SelfIp, ets:next(PeerTid, SelfIp), N,
                        SampleTid, M);
extract_peer_sample(PeerTid, SelfIp, Ip, N, SampleTid, M) ->
    case random:uniform(M) of
        RandomN when RandomN =< N ->
            [Peer] = ets:lookup(PeerTid, Ip),
            true = ets:insert(SampleTid, {RandomN, Peer}),
            extract_peer_sample(PeerTid, SelfIp, ets:next(PeerTid, Ip), N,
                                SampleTid, M+1);
        _ ->
            extract_peer_sample(PeerTid, SelfIp, ets:next(PeerTid, Ip), N,
                                SampleTid, M+1)
    end.
