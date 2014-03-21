-module(ds_jsonrpc).

%% example use:
%% ds_jsonrpc:housekeeping(undefined, {{192,168,1,80}, <<>>, 6700}).
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
-export([housekeeping/3]).
-export([get_number_of_nodes/3, get_node/4, get_all_nodes/3,
         get_random_nodes/4]).
-export([publish_node/4, unpublish_node/3, published_nodes/4]).
-export([reserve_oa/4, reserved_oas/4]).
-export([encode_node_descriptors/1, encode_node_descriptor/1,
         decode_node_descriptors/1, decode_node_descriptor/1]).

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
%%% exported: housekeeping
%%%

-spec housekeeping(httplib:ip_address_port(), httplib:ip_address_port(),
                   node_crypto:pki_key()) ->
                          'ok' | {'error', error_reason()}.

housekeeping(MyIpAddressPort, IpAddressPort, PrivateKey) ->
    case jsonrpc:call(MyIpAddressPort, IpAddressPort, <<"housekeeping">>,
                      PrivateKey) of
        {ok, true} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: get_number_of_nodes
%%%

-spec get_number_of_nodes(httplib:ip_address_port(), httplib:ip_address_port(),
                          node_crypto:pki_key()) ->
                                 {'ok', non_neg_integer()} |
                                 {'error', error_reason()}.

get_number_of_nodes(MyIpAddressPort, IpAddressPort, PrivateKey) ->
    jsonrpc:call(MyIpAddressPort, IpAddressPort, <<"get-number-of-nodes">>,
                 PrivateKey).

%%%
%%% exported: get_node
%%%

-spec get_node(httplib:ip_address_port(), httplib:ip_address_port(),
               node_crypto:pki_key(), na()) ->
                      {'ok', #node_descriptor{}} | {'error', error_reason()}.

get_node(MyIpAddressPort, IpAddressPort, PrivateKey, Na) ->
    case jsonrpc:call(MyIpAddressPort, IpAddressPort, <<"get-node">>,
                      PrivateKey, [{<<"na">>, node_jsonrpc:encode_na(Na)}]) of
        {ok, Nd}->
            decode_node_descriptor(Nd);
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: get_all_nodes
%%%

-spec get_all_nodes(httplib:ip_address_port(), httplib:ip_address_port(),
                    node_crypto:pki_key()) ->
                           {'ok', [#node_descriptor{}]} |
                           {'error', error_reason()}.

get_all_nodes(MyIpAddressPort, IpAddressPort, PrivateKey) ->
    case jsonrpc:call(MyIpAddressPort, IpAddressPort, <<"get-all-nodes">>,
                      PrivateKey) of
        {ok, Nds}->
            decode_node_descriptors(Nds);
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: get_random_nodes
%%%

-spec get_random_nodes(httplib:ip_address_port(), httplib:ip_address_port(),
                       node_crypto:pki_key(), non_neg_integer()) ->
                              {'ok', [#node_descriptor{}]} |
                              {'error', error_reason()}.

get_random_nodes(MyIpAddressPort, IpAddressPort, PrivateKey, N) ->
    case jsonrpc:call(MyIpAddressPort, IpAddressPort, <<"get-random-nodes">>,
                      PrivateKey, [{<<"n">>, N}]) of
        {ok, Nds} ->
            decode_node_descriptors(Nds);
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: publish_node
%%%

-spec publish_node(httplib:ip_address_port(), httplib:ip_address_port(),
                   node_crypto:pki_key(), #node_descriptor{}) ->
                          {'ok', NodeTTL :: non_neg_integer()} |
                          {'error', error_reason()}.

publish_node(MyIpAddressPort, IpAddressPort, PrivateKey, Nd) ->
    jsonrpc:call(MyIpAddressPort, IpAddressPort, <<"publish-node">>,
                 PrivateKey, encode_node_descriptor(Nd)).

%%%
%%% exported: unpublish_node
%%%

-spec unpublish_node(httplib:ip_address_port(), httplib:ip_address_port(),
                     node_crypto:pki_key()) ->
                            'ok' | {'error', error_reason()}.

unpublish_node(MyIpAddressPort, IpAddressPort, PrivateKey) ->
    case jsonrpc:call(MyIpAddressPort, IpAddressPort, <<"unpublish-node">>,
                      PrivateKey) of
        {ok, true} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: published_nodes
%%%

-spec published_nodes(httplib:ip_address_port(), httplib:ip_address_port(),
                      node_crypto:pki_key(), [na()]) ->
                             {'ok', [na()]} | {'error', error_reason()}.

published_nodes(MyIpAddressPort, IpAddressPort, PrivateKey, Nas) ->
    case jsonrpc:call(MyIpAddressPort, IpAddressPort, <<"published-nodes">>,
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

-spec reserve_oa(httplib:ip_address_port(), httplib:ip_address_port(),
                 node_crypto:pki_key(), oa()) ->
                        'ok' | {'error', error_reason()}.

reserve_oa(MyIpAddressPort, IpAddressPort, PrivateKey, Oa) ->
    case jsonrpc:call(MyIpAddressPort, IpAddressPort, <<"reserve-oa">>,
                      PrivateKey, [{<<"oa">>, node_jsonrpc:encode_oa(Oa)}]) of
        {ok, true} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: reserved_oas
%%%

-spec reserved_oas(httplib:ip_address_port(), httplib:ip_address_port(),
                   node_crypto:pki_key(), na()) ->
                          {'ok', [oa()]} | {'error', error_reason()}.

reserved_oas(MyIpAddressPort, IpAddressPort, PrivateKey, Na) ->
    case jsonrpc:call(MyIpAddressPort, IpAddressPort, <<"reserved-oas">>,
                      PrivateKey, [{<<"na">>, node_jsonrpc:encode_na(Na)}]) of
        {ok, Oas} ->
            node_jsonrpc:decode_oas(Oas);
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: encode_node_descriptors (to be used by ds_jsonrpc_serv.erl)
%%%

-spec encode_node_descriptors([#node_descriptor{}]) -> jsx:json_term().

encode_node_descriptors(Nds) ->
    [encode_node_descriptor(Nd) || Nd <- Nds].

%%%
%%% exported: encode_node_descriptor (to be used by ds_jsonrpc_serv.erl)
%%%

-spec encode_node_descriptor(#node_descriptor{}) -> jsx:json_term().

encode_node_descriptor(#node_descriptor{na = undefined, public_key = PublicKey,
                                        flags = Flags}) ->
    [{<<"public-key">>, base64:encode(PublicKey)},
     {<<"flags">>, Flags}];
encode_node_descriptor(#node_descriptor{na = Na, public_key = PublicKey,
                                        flags = Flags}) ->
    [{<<"na">>, node_jsonrpc:encode_na(Na)},
     {<<"public-key">>, base64:encode(PublicKey)},
     {<<"flags">>, Flags}].

%%%
%%% exported: decode_node_descriptors (to be used by ds_jsonrpc_serv.erl)
%%%

-spec decode_node_descriptors(jsx:json_term()) -> {'ok', [#node_descriptor{}]} |
                                                  {'error', 'einval'}.

decode_node_descriptors(Nds) ->
    jsonrpc:decode(Nds, fun decode_node_descriptor/1).

%%%
%%% exported: decode_node_descriptor (to be used by ds_jsonrpc_serv.erl)
%%%

-spec decode_node_descriptor(jsx:json_term()) -> {'ok', #node_descriptor{}} |
                                                 {'error', 'einval'}.

decode_node_descriptor([{<<"public-key">>, PublicKey},
                        {<<"flags">>, Flags}])
  when is_binary(PublicKey) andalso is_integer(Flags) ->
    {ok, #node_descriptor{public_key = base64:decode(PublicKey),
                          flags = Flags}};
decode_node_descriptor([{<<"na">>, Na},
                        {<<"public-key">>, PublicKey},
                        {<<"flags">>, Flags}])
  when is_binary(PublicKey) andalso is_integer(Flags) ->
    case node_jsonrpc:decode_na(Na) of
        {ok, DecodedNa} ->
            {ok, #node_descriptor{na = DecodedNa,
                                  public_key = base64:decode(PublicKey),
                                  flags = Flags}};
        {error, Reason} ->
            {error, Reason}
    end;
decode_node_descriptor(_Nd) ->
    {error, einval}.
