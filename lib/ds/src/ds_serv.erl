-module(ds_serv).

%%% external exports
-export([start_link/0, stop/0, stop/1]).
-export([enforce_peer_ttl/0]).
-export([get_number_of_peers/0, get_peer/1, get_all_peers/0,
         get_random_peers/2]).
-export([publish_peer/1, unpublish_peer/1, published_peers/1]).
-export([reserve_oa/2, reserved_oas/1]).
-export([lookup_peer/1]).

%%% internal exports
-export([init/1]).

%%% include files
-include_lib("ds/include/ds.hrl").
-include_lib("node/include/node.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants

%%% records
-record(state, {
	  parent           :: pid(),
	  peer_db          :: ets:tid(),
          oa_db            :: ets:tid(),
          %% anond.conf parameters
          mode             :: common_config_jsonrpc_serv:mode(),
          peer_ttl         :: non_neg_integer(),
          max_oas_per_peer :: non_neg_integer()
	 }).

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link() -> {'ok', pid()} | {'error', 'already_started'}.

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
%%% exported: enforce_peer_ttl
%%%

-spec enforce_peer_ttl() -> 'ok'.

enforce_peer_ttl() ->
    ?MODULE ! {enforce_peer_ttl, false},
    ok.

%%%
%%% exported: get_number_of_peers
%%%

-spec get_number_of_peers() -> {'ok', non_neg_integer()}.

get_number_of_peers() ->
    serv:call(?MODULE, get_number_of_peers).

%%%
%%% exported: get_peer
%%%

-spec get_peer(na()) -> {'ok', #peer{}} | {'error', 'no_such_peer'}.

get_peer(Na) ->
    serv:call(?MODULE, {get_peer, Na}).

%%%
%%% exported: get_all_peers
%%%

-spec get_all_peers() -> {'ok', [#peer{}]}.

get_all_peers() ->
    serv:call(?MODULE, get_all_peers).

%%%
%%% exported: get_random_peers
%%%

-spec get_random_peers(na(), non_neg_integer()) ->
                              {'ok', [#peer{}]} | {'error', 'too_few_peers'}.
get_random_peers(MyNa, N) ->
    serv:call(?MODULE, {get_random_peers, MyNa, N}).

%%%
%%% exported: publish_peer
%%%

-spec publish_peer(#peer{}) -> {'ok', PeerTTL :: non_neg_integer()}.

publish_peer(Peer) ->
    serv:call(?MODULE, {publish_peer, Peer}).

%%%
%%% exported: unpublish_peer
%%%

-spec unpublish_peer(na()) -> 'ok'.

unpublish_peer(Na) ->
    serv:call(?MODULE, {unpublish_peer, Na}).

%%%
%%% exported: published_peers
%%%

-spec published_peers([na()]) -> {'ok', [na()]}.

published_peers(Nas) ->
    serv:call(?MODULE, {published_peers, Nas}).

%%%
%%% exported: reserve_oa
%%%

-spec reserve_oa(oa(), na()) ->
                        'ok' | {'error', 'no_such_peer' | 'too_many_oas'}.

reserve_oa(Oa, Na) ->
    serv:call(?MODULE, {reserve_oa, Oa, Na}).

%%%
%%% exported: reserved_oas
%%%

-spec reserved_oas(na()) -> {'ok', [oa()]} | {'error', 'no_reserved_oas'}.

reserved_oas(Na) ->
    serv:call(?MODULE, {reserved_oas, Na}).

%%%
%%% exported: lookup_peer
%%%

-spec lookup_peer(na()) -> [#peer{}].

lookup_peer(Na) ->
    ets:lookup(peer_db, Na).

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
            ok = config_json_serv:subscribe(),
	    PeerDb = ets:new(peer_db, [{keypos, 2}, named_table]),
	    OaDb = ets:new(oa_db, [bag]),
            timelib:start_timer(S#state.peer_ttl, {enforce_peer_ttl, true}),
	    Parent ! {self(), started},
	    loop(S#state{parent = Parent, peer_db = PeerDb, oa_db = OaDb});
        _ ->
            Parent ! {self(), already_started}
    end.

loop(#state{parent = Parent,
            peer_db = PeerDb,
            oa_db = OaDb,
            mode = Mode,
            peer_ttl = PeerTTL,
            max_oas_per_peer = MaxOasPerPeer} = S) ->
    receive
        config_updated ->
            ?daemon_log("Configuration changed...", []),
            loop(read_config(S));
        {enforce_peer_ttl, Repeat} ->
            ?daemon_log("Looking for stale peers...", []),
            StaleNas =
                ets:foldl(
                  fun(#peer{na = Na, last_updated = LastUpdated}, Acc) ->
                          Delta = timelib:ugnow_delta({minus, LastUpdated}),
                          if
                              Delta > PeerTTL ->
                                  [Na|Acc];
                              true ->
                                  []
                          end
                  end, [], PeerDb),
            lists:foreach(fun(Na) ->
                                  true = ets:delete(PeerDb, Na),
                                  true = ets:match_delete(OaDb, {'_', Na}),
                                  ?daemon_log("Removed stale peer ~s",
                                              [net_tools:string_address(Na)])
                          end, StaleNas),
            if
                Repeat ->
                    timelib:start_timer(PeerTTL, {enforce_peer_ttl, true}),
                    loop(S);
                true ->
                    loop(S)
            end;
	{From, stop} ->
            ?daemon_log("Stopping directory server...", []),
	    ets:delete(PeerDb),
	    ets:delete(OaDb),
	    From ! {self(), ok};
        {From, get_number_of_peers} ->
            NumberOfPeers = ets:info(PeerDb, size),
            ?daemon_log("Calculated number of peers (~w)", [NumberOfPeers]),
            From ! {self(), {ok, NumberOfPeers}},
            loop(S);
        {From, {get_peer, Na}} ->
            case ets:lookup(PeerDb, Na) of
                [Peer] ->
                    From ! {self(), {ok, Peer}},
                    loop(S);
                [] ->
                    From ! {self(), {error, no_such_peer}},
                    loop(S)
            end;
        {From, get_all_peers} ->
            AllPeers = ets:foldl(fun(Peer, Acc) -> [Peer|Acc] end, [], PeerDb),
            ?daemon_log("Extracted all (~w) peers", [length(AllPeers)]),
            From ! {self(), {ok, AllPeers}},
            loop(S);
        %% see doc/small_simulation.jpg; return two non-random peers
        {From, {get_random_peers, MyNa, 2}} when Mode == simulation ->
            case get_simulated_peers(PeerDb, MyNa) of
                {ok, SimulatedNas, SimulatedPeers} ->
                    ?daemon_log(
                       "Extracted ~w simulated and non-random peers: ~s",
                       [length(SimulatedNas),
                        net_tools:string_addresses(SimulatedNas)]),
                    From ! {self(), {ok, SimulatedPeers}},
                    loop(S);
                {error, Reason} ->
                    ?daemon_log("2 random peers could not be extracted", []),
                    From ! {self(), {error, Reason}},
                    loop(S)
            end;
        {From, {get_random_peers, MyNa, N}} when is_integer(N) ->
            case ets:info(PeerDb, size) of
                Size when N >= Size ->
                    ?daemon_log("~w random peers could not be extracted",
                                [Size]),
                    From ! {self(), {error, too_few_peers}},
                    loop(S);
                _Size ->
                    RandomPeers = get_random_peers(PeerDb, MyNa, N),
                    RandomNas =
                        [RandomPeer#peer.na || RandomPeer <- RandomPeers],
                    ?daemon_log("Extracted ~w random peers: ~s",
                                [length(RandomNas),
                                 net_tools:string_addresses(RandomNas)]),
                    From ! {self(), {ok, RandomPeers}},
                    loop(S)
            end;
        {From, {publish_peer, #peer{na = Na} = Peer}} ->
            UpdatedPeer = Peer#peer{last_updated = timelib:ugnow()},
            true = ets:insert(PeerDb, UpdatedPeer),
            ?daemon_log("Peer ~s published", [net_tools:string_address(Na)]),
            From ! {self(), {ok, PeerTTL}},
            loop(S);
        {From, {unpublish_peer, Na}} ->
            true = ets:delete(PeerDb, Na),
            ?daemon_log("Peer ~s unpublished", [net_tools:string_address(Na)]),
            From ! {self(), ok},
            loop(S);
        {From, {published_peers, Nas}} ->
            StillPublishedNas =
                [Na || Na <- Nas, ets:member(PeerDb, Na) == true],
            ?daemon_log("Out of these peers ~s, these are still published ~s",
                        [net_tools:string_addresses(Nas),
                         net_tools:string_addresses(StillPublishedNas)]),
            From ! {self(), {ok, StillPublishedNas}},
            loop(S);
        {From, {reserve_oa, Oa, Na}} ->
            case ets:lookup(PeerDb, Na) of
                [] ->
                    ?daemon_log("Rejected reservation of overlay address ~s to "
                                "unknown peer ~s",
                                [net_tools:string_address(Oa),
                                 net_tools:string_address(Na)]),
                    From ! {self(), {error, no_such_peer}},
                    loop(S);
                _ ->
                    NumberOfOas = length(ets:lookup(OaDb, Oa)),
                    if
                        NumberOfOas =< MaxOasPerPeer ->
                            true = ets:insert(OaDb, {Oa, Na}),
                            ?daemon_log(
                               "Reserved overlay address ~s to peer ~s",
                               [net_tools:string_address(Oa),
                                net_tools:string_address(Na)]),
                            From ! {self(), ok},
                            loop(S);
                        true ->
                            ?daemon_log("Peer ~s tried to reserve more than ~w "
                                        "overlay addresses",
                                        [net_tools:string_address(Na),
                                         MaxOasPerPeer]),
                            From ! {self(), {error, too_many_oas}},
                            loop(S)
                    end
            end;
        {From, {reserved_oas, Na}} ->
            case ets:match(OaDb, {'$1', Na}) of
                [Oas] ->
                    ?daemon_log("~s has these overlay addresses reserved: ~s",
                                [net_tools:string_address(Na),
                                 net_tools:string_addresses(Oas)]),
                    From ! {self(), {ok, Oas}},
                    loop(S);
                [] ->
                    From ! {self(), {error, no_reserved_oas}},
                    loop(S)
            end;
        {'EXIT', Parent, Reason} ->
	    true = ets:delete(PeerDb),
            exit(Reason);
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            loop(S)
    end.

%%%
%%% init
%%%

read_config(S) ->
    Mode = ?config([mode]),
    PeerTTL = ?config(['directory-server', 'peer-ttl']),
    MaxOasPerPeer = ?config(['directory-server', 'max-oas-per-peer']),
    S#state{mode = Mode, peer_ttl = PeerTTL, max_oas_per_peer = MaxOasPerPeer}.

%%%
%%% get_random_peers
%%%

get_simulated_peers(PeerDb, {MyNaIpAddress, MyNaPort}) ->
    {value, {MyNaPort, PeerNaPorts}} =
        lists:keysearch(MyNaPort, 1, ?NON_RANDOM_PEERS),
    PeerNas = [{MyNaIpAddress, PeerNaPort} || PeerNaPort <- PeerNaPorts],
    case lookup_simulated_peers(PeerDb, PeerNas) of
        {ok, Peers} ->
            {ok, PeerNas, Peers};
        {error, Reason} ->
            {error, Reason}
    end.

lookup_simulated_peers(PeerDb, PeerNas) ->
    lookup_simulated_peers(PeerDb, PeerNas, []).

lookup_simulated_peers(_PeerDb, [], Acc) ->
    {ok, lists:reverse(Acc)};
lookup_simulated_peers(PeerDb, [PeerNa|Rest], Acc) ->
    case ets:lookup(PeerDb, PeerNa) of
        [Peer] ->
            lookup_simulated_peers(PeerDb, Rest, [Peer|Acc]);
        [] ->
            {error, too_few_peers}
    end.

%% http://en.wikipedia.org/wiki/Reservoir_sampling
get_random_peers(PeerDb, MyNa, N) ->
    Tid = ets:new(sample_db, []),
    NextKey = init_sample(PeerDb, MyNa, ets:first(PeerDb), N, Tid),
    Sample = extract_peer_sample(PeerDb, MyNa, NextKey, N, Tid, N+1),
    ets:delete(Tid),
    Sample.

init_sample(_PeerDb, _MyNa, Na, 0, _Tid) ->
    Na;
init_sample(PeerDb, MyNa, MyNa, N, Tid) ->
    init_sample(PeerDb, MyNa, ets:next(PeerDb, MyNa), N, Tid);
init_sample(PeerDb, MyNa, Na, N, Tid) ->
    [Peer] = ets:lookup(PeerDb, Na),
    true = ets:insert(Tid, {N, Peer}),
    init_sample(PeerDb, MyNa, ets:next(PeerDb, Na), N-1, Tid).

extract_peer_sample(_PeerDb, _MyNa, '$end_of_table', _N, Tid, _M) ->
    ets:foldl(fun({_, Peer}, Acc) -> [Peer|Acc] end, [], Tid);
extract_peer_sample(PeerDb, MyNa, MyNa, N, Tid, M) ->
    extract_peer_sample(PeerDb, MyNa, ets:next(PeerDb, MyNa), N, Tid, M);
extract_peer_sample(PeerDb, MyNa, Na, N, Tid, M) ->
    case random:uniform(M) of
        RandomN when RandomN =< N ->
            [Peer] = ets:lookup(PeerDb, Na),
            true = ets:insert(Tid, {RandomN, Peer}),
            extract_peer_sample(PeerDb, MyNa, ets:next(PeerDb, Na), N, Tid,
                                M+1);
        _ ->
            extract_peer_sample(PeerDb, MyNa, ets:next(PeerDb, Na), N, Tid,
                                M+1)
    end.
