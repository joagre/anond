-module(ds_jsonrpc).

%% example use:
%% ds_jsonrpc:enforce_peer_ttl(undefined, {{192,168,1,80}, <<>>, 6700}).
%% ds_jsonrpc:get_number_of_peers(undefined, {{192,168,1,80}, <<>>, 6700}).
%% ds_jsonrpc:get_all_peers(undefined, {{192,168,1,80}, <<>>, 6700}).
%% ds_jsonrpc:get_random_peers(undefined, {{192,168,1,80}, 6700}, <<>>, {{192,168,1,80}, 9001}, 1).
%% ds_jsonrpc:publish_peer(undefined, {{192,168,1,80}, 6700}, <<>>, #peer{na = {{192,168,1,80}, 9001}, public_key = <<"foo1">>}).
%% ds_jsonrpc:unpublish_peer(undefined, {{192,168,1,80}, 6700}, <<>>, {{192,168,1,80}, 9001}).
%% ds_jsonrpc:published_peers(undefined, {{192,168,1,80}, 6700}, <<>>, [{{192,168,1,80}, 9001}, {{192,168,1,80}, 9002}, {{192,168,1,80}, 9003}]).
%% ds_jsonrpc:reserve_oa(undefined, {{192,168,1,80}, 6700}, <<>>, {65152,0,0,0,50821,2303,65094,10}, {{192,168,1,80}, 9001}).
%% ds_jsonrpc:reserved_oas(undefined, {{192,168,1,80}, 6700}, <<>>, {{192,168,1,80}, 9001}).

%%% external exports
-export([format_error/1]).
-export([enforce_peer_ttl/3]).
-export([get_number_of_peers/3, get_peer/4, get_all_peers/3,
         get_random_peers/5]).
-export([publish_peer/4, unpublish_peer/4, published_peers/4]).
-export([reserve_oa/5, reserved_oas/4]).
-export([encode_peers/1, encode_peer/1, decode_peers/1, decode_peer/1]).

%%% internal exports

%%% include files
-include_lib("ds/include/ds.hrl").
-include_lib("node/include/node.hrl").
-include_lib("util/include/jsonrpc_serv.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants

%%% records

%%% types
-type error_reason() :: jsonrpc:error_reason() | 'einval'.

%%%
%%% exported: format_error
%%%

format_error(Reason) ->
    inet:format_error(Reason).

%%%
%%% exported: enforce_peer_ttl
%%%

-spec enforce_peer_ttl(httplib:ip_address_port() | 'undefined',
                       httplib:ip_address_port(), node_crypto:pki_key()) ->
                              'ok' | {'error', error_reason()}.

enforce_peer_ttl(LocalIpAddressPort, IpAddressPort, PrivateKey) ->
    case jsonrpc:call(LocalIpAddressPort, IpAddressPort, <<"enforce-peer-ttl">>,
                      PrivateKey) of
        {ok, true} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: get_number_of_peers
%%%

-spec get_number_of_peers(httplib:ip_address_port() | 'undefined',
                          httplib:ip_address_port(), node_crypto:pki_key()) ->
                                 {'ok', non_neg_integer()} |
                                 {'error', error_reason()}.

get_number_of_peers(LocalIpAddressPort, IpAddressPort, PrivateKey) ->
    jsonrpc:call(LocalIpAddressPort, IpAddressPort, <<"get-number-of-peers">>,
                 PrivateKey).

%%%
%%% exported: get_peer
%%%

-spec get_peer(httplib:ip_address_port() | 'undefined',
               httplib:ip_address_port(), node_crypto:pki_key(), na()) ->
                      {'ok', #peer{}} | {'error', error_reason()}.

get_peer(LocalIpAddressPort, IpAddressPort, PrivateKey, Na) ->
    case jsonrpc:call(LocalIpAddressPort, IpAddressPort, <<"get-peer">>,
                      PrivateKey, [{<<"na">>, node_jsonrpc:encode_na(Na)}]) of
        {ok, Peer}->
            decode_peer(Peer);
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: get_all_peers
%%%

-spec get_all_peers(httplib:ip_address_port() | 'undefined',
                    httplib:ip_address_port(), node_crypto:pki_key()) ->
                           {'ok', [#peer{}]} | {'error', error_reason()}.

get_all_peers(LocalIpAddressPort, IpAddressPort, PrivateKey) ->
    case jsonrpc:call(LocalIpAddressPort, IpAddressPort, <<"get-all-peers">>,
                      PrivateKey) of
        {ok, Peers}->
            decode_peers(Peers);
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: get_random_peers
%%%

-spec get_random_peers(httplib:ip_address_port() | 'undefined',
                       httplib:ip_address_port(), node_crypto:pki_key(), na(),
                       non_neg_integer()) ->
                              {'ok', [#peer{}]} | {'error', error_reason()}.

get_random_peers(LocalIpAddressPort, IpAddressPort, PrivateKey, MyNa, N) ->
    case jsonrpc:call(LocalIpAddressPort, IpAddressPort, <<"get-random-peers">>,
                      PrivateKey, [{<<"my-na">>, node_jsonrpc:encode_na(MyNa)},
                                   {<<"n">>, N}]) of
        {ok, Peers}->
            decode_peers(Peers);
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: publish_peer
%%%

-spec publish_peer(httplib:ip_address_port() | 'undefined',
                   httplib:ip_address_port(), node_crypto:pki_key(), #peer{}) ->
                          {'ok', PeerTTL :: non_neg_integer()} |
                          {'error', error_reason()}.

publish_peer(LocalIpAddressPort, IpAddressPort, PrivateKey, Peer) ->
    jsonrpc:call(LocalIpAddressPort, IpAddressPort, <<"publish-peer">>,
                 PrivateKey, encode_peer(Peer)).

%%%
%%% exported: unpublish_peer
%%%

-spec unpublish_peer(httplib:ip_address_port() | 'undefined',
                     httplib:ip_address_port(), node_crypto:pki_key(), na()) ->
                            'ok' | {'error', error_reason()}.

unpublish_peer(LocalIpAddressPort, IpAddressPort, PrivateKey, Na) ->
    case jsonrpc:call(LocalIpAddressPort, IpAddressPort, <<"unpublish-peer">>,
                      PrivateKey, [{<<"na">>, node_jsonrpc:encode_na(Na)}]) of
        {ok, true} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: published_peers
%%%

-spec published_peers(httplib:ip_address_port() | 'undefined',
                      httplib:ip_address_port(), node_crypto:pki_key(),
                      [na()]) ->
                             {'ok', [na()]} | {'error', error_reason()}.

published_peers(LocalIpAddressPort, IpAddressPort, PrivateKey, Nas) ->
    case jsonrpc:call(LocalIpAddressPort, IpAddressPort, <<"published-peers">>,
                      PrivateKey,
                      [{<<"nas">>, node_jsonrpc:encode_nas(Nas)}]) of
        {ok, PublishedNas} ->
            node_jsonrpc:decode_nas(PublishedNas);
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: reserve_oa
%%%

-spec reserve_oa(httplib:ip_address_port() | 'undefined',
                 httplib:ip_address_port(), node_crypto:pki_key(), oa(),
                 na()) ->
                        'ok' | {'error', error_reason()}.

reserve_oa(LocalIpAddressPort, IpAddressPort, PrivateKey, Oa, Na) ->
    case jsonrpc:call(LocalIpAddressPort, IpAddressPort, <<"reserve-oa">>,
                      PrivateKey, [{<<"oa">>, node_jsonrpc:encode_oa(Oa)},
                                   {<<"na">>, node_jsonrpc:encode_na(Na)}]) of
        {ok, true} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: reserved_oas
%%%

-spec reserved_oas(httplib:ip_address_port() | 'undefined',
                   httplib:ip_address_port(), node_crypto:pki_key(), na()) ->
                          {'ok', [oa()]} | {'error', error_reason()}.

reserved_oas(LocalIpAddressPort, IpAddressPort, PrivateKey, Na) ->
    case jsonrpc:call(LocalIpAddressPort, IpAddressPort, <<"reserved-oas">>,
                      PrivateKey, [{<<"na">>, node_jsonrpc:encode_na(Na)}]) of
        {ok, Oas} ->
            node_jsonrpc:decode_oas(Oas);
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: encode_peers (to be used by ds_jsonrpc_serv.erl) ok
%%%

-spec encode_peers([#peer{}]) -> jsx:json_term().

encode_peers(Peers) ->
    [encode_peer(Peer) || Peer <- Peers].

%%%
%%% exported: encode_peer (to be used by ds_jsonrpc_serv.erl)
%%%

-spec encode_peer(#peer{}) -> jsx:json_term().

encode_peer(Peer) ->
    [{<<"na">>, node_jsonrpc:encode_na(Peer#peer.na)},
     {<<"public-key">>, base64:encode(Peer#peer.public_key)},
     {<<"flags">>, Peer#peer.flags}].

%%%
%%% exported: decode_peers (to be used by ds_jsonrpc_serv.erl)
%%%

-spec decode_peers(jsx:json_term()) ->
                          {'ok', [#peer{}]} | {'error', 'einval'}.

decode_peers(Peers) ->
    node_jsonrpc:decode(Peers, fun decode_peer/1).

%%%
%%% exported: decode_peer (to be used by ds_jsonrpc_serv.erl)
%%%

-spec decode_peer(jsx:json_term()) ->
                          {'ok', #peer{}} | {'error', 'einval'}.

decode_peer([{<<"na">>, Na},
             {<<"public-key">>, PublicKey},
             {<<"flags">>, Flags}])
  when is_binary(PublicKey), is_integer(Flags) ->
    case node_jsonrpc:decode_na(Na) of
        {ok, DecodedNa} ->
            {ok, #peer{na = DecodedNa, public_key = base64:decode(PublicKey),
                       flags = Flags}};
        {error, Reason} ->
            {error, Reason}
    end;
decode_peer(_Peer) ->
    {error, einval}.
