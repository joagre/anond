-module(ds_jsonrpc_serv).

%%% external exports
-export([start_link/0]).

%%% internal exports
-export([ds_handler/2]).

%%% include files
-include_lib("util/include/jsonrpc.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").
-include_lib("ds/include/ds.hrl").

%%% constants
-define(DS_PERMISSION_DENIED, 1).
-define(DS_UNKNOWN_PEER, 2).
-define(DS_TOO_FEW_PEERS, 3).

%%% records

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link() -> {ok, pid()}.

start_link() ->
    IpAddress = {0,0,0,0},
    Port = 6700,
    jsonrpc_serv:start_link(IpAddress, Port, [], {?MODULE, ds_handler, []}).

ds_handler(<<"enforce-peer-ttl">>, undefined) ->
    ok = ds_serv:enforce_peer_ttl(),
    {ok, true};
ds_handler(<<"get-number-of-peers">>, undefined) ->
    ds_serv:get_number_of_peers();
ds_handler(<<"get-all-peers">>, undefined) ->
    {ok, Peers} = ds_serv:get_all_peers(),
    {ok, [json_peer(Peer) || Peer <- Peers]};
ds_handler(<<"get-random-peers">>, [{<<"my-na">>, BinaryMyNa}, {<<"n">>, N}]) ->
    case decode_na(BinaryMyNa) of
        {ok, MyNa} ->
            if
                is_integer(N) ->
%% Swicth over to na()
                    case ds_serv:get_random_peers(self(), N) of
%                    case ds_serv:get_random_peers(MyNa, N) of
                        {ok, Peers} ->
                            {ok, [json_peer(Peer) || Peer <- Peers]};
                        too_few_peers ->
                            JsonError = #json_error{
                              code = ?DS_TOO_FEW_PEERS},
                            {error, JsonError}
                    end;
                true ->
                    JsonError = #json_error{
                      code = ?JSONRPC_INVALID_PARAMS,
                      data = <<"n">>},
                    {error, JsonError}
            end;
        {error, Reason} ->
            JsonError = #json_error{
              code = ?JSONRPC_INVALID_PARAMS,
              message = ?l2b(inet:format_error(Reason)),
              data = <<"my-na">>},
            {error, JsonError}
    end;
ds_handler(<<"publish-peer">>, [{<<"na">>, BinaryNa},
                                {<<"public-key">>, PublicKey},
                                {<<"flags">>, Flags}]) ->
    case decode_na(BinaryNa) of
        {ok, Na} ->
            Peer = #peer{na = Na, public_key = PublicKey, flags = Flags},
            {ok, PeerTTL} = ds_serv:publish_peer(Peer),
            {ok, PeerTTL};
        {error, Reason} ->
            JsonError = #json_error{
              code = ?JSONRPC_INVALID_PARAMS,
              message = ?l2b(inet:format_error(Reason)),
              data = <<"na">>},
            {error, JsonError}
    end;
ds_handler(<<"unpublish-peer">>, [{<<"na">>, BinaryNa}]) ->
    case decode_na(BinaryNa) of
        {ok, Na} ->
%% Swicth over to na()
            ok = ds_serv:unpublish_peer(self()),
%            ok = ds_serv:unpublish_peer(Na),
            {ok, true};
        {error, Reason} ->
            JsonError = #json_error{
              code = ?JSONRPC_INVALID_PARAMS,
              message = ?l2b(inet:format_error(Reason)),
              data = <<"na">>},
            {error, JsonError}
    end;
ds_handler(<<"published-peers">>, [{<<"nas">>, BinaryNas}]) ->
    case decode_nas(BinaryNas) of
        {ok, Nas} ->
%% Swicth ovr to na()
%            {ok, PublishedNas} = ds_serv:published_peers(Nas),
            PublishedNas = [{{1,1,1,1}, 1}],
            {ok, [encode_na(Na) || Na <- PublishedNas]};
        {error, Reason} ->
            JsonError = #json_error{
              code = ?JSONRPC_INVALID_PARAMS,
              message = ?l2b(inet:format_error(Reason)),
              data = <<"published-peers">>},
            {error, JsonError}
    end;
ds_handler(<<"reserve-oa">>, [{<<"oa">>, BinaryOa}, {<<"na">>, BinaryNa}]) ->
    case decode_na(BinaryNa) of
        {ok, Na} ->
            case decode_oa(BinaryOa) of
                {ok, Oa} ->
%% Swicth ovr to na()
                    case ds_serv:reserve_oa(1, self()) of
%                    case ds_serv:reserve_oa(Oa, Na) of
                        ok ->
                            {ok, true};
                        no_such_peer ->
                            JsonError = #json_error{
                              code = ?DS_UNKNOWN_PEER,
                              message = <<"Unknown peer">>,
                              data = BinaryNa},
                            {error, JsonError};
                        too_many_oas ->
                            JsonError = #json_error{
                              code = ?DS_PERMISSION_DENIED,
                              message = <<"Too many reservations">>},
                            {error, JsonError}
                    end;
                {error, Reason} ->
                    JsonError = #json_error{
                      code = ?JSONRPC_INVALID_PARAMS,
                      message = ?l2b(inet:format_error(Reason)),
                      data = <<"oa">>},
                    {error, JsonError}
            end;
        {error, Reason} ->
            JsonError = #json_error{
              code = ?JSONRPC_INVALID_PARAMS,
              message = ?l2b(inet:format_error(Reason)),
              data = <<"na">>},
            {error, JsonError}
    end;
ds_handler(Method, Params) ->
    ?error_log({invalid_request, Method, Params}),
    JsonError = #json_error{code = ?JSONRPC_INVALID_REQUEST},
    {error, JsonError}.

json_peer(Peer) ->
    [%{<<"na">>, encode_na(Peer#peer.na)},
     {<<"public-key">>, Peer#peer.public_key},
     {<<"flags">>, Peer#peer.flags}].

decode_na(BinaryNa) ->
    case string:tokens(?b2l(BinaryNa), ":") of
        [IpAddressString, PortString] ->
            case inet:parse_address(IpAddressString) of
                {ok, IpAddress} ->
                    case catch ?l2i(PortString) of
                        Port when is_integer(Port) ->
                            {ok, {IpAddress, Port}};
                        _ ->
                            {error, einval}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, einval}
    end.

decode_nas(BinaryNas) ->
    decode_nas(BinaryNas, []).

decode_nas([], Nas) ->
    {ok, lists:reverse(Nas)};
decode_nas([BinaryNa|Rest], Nas) ->
    case decode_na(BinaryNa) of
        {ok, Na} ->
            decode_nas(Rest, [Na|Nas]);
        {error, Reason} ->
            {error, Reason}
    end.

encode_na({IpAddress, Port}) ->
    ?l2b([net_tools:string_address(IpAddress), ":", ?i2l(Port)]).

decode_oa(BinaryOa) ->
    inet:parse_address(?b2l(BinaryOa)).
