-module(ds_jsonrpc_serv).

%%% external exports
-export([start_link/0]).

%%% internal exports
-export([ds_handler/2]).

%%% include files
-include_lib("ds/include/ds.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/jsonrpc_serv.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants

%%% records

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link() -> {ok, pid()}.

start_link() ->
    {IpAddress, Port} = ?config(['directory-server', listen]),
    jsonrpc_serv:start_link(IpAddress, Port, [], {?MODULE, ds_handler, []}).

ds_handler(<<"enforce-peer-ttl">>, undefined) ->
    ok = ds_serv:enforce_peer_ttl(),
    {ok, true};
ds_handler(<<"get-number-of-peers">>, undefined) ->
    ds_serv:get_number_of_peers();
ds_handler(<<"get-peer">>, [{<<"na">>, Na}]) ->
    case node_jsonrpc:decode_na(Na) of
        {ok, DecodedNa} ->
            case ds_serv:get_peer(DecodedNa) of
                {ok, Peer} ->
                    {ok, ds_jsonrpc:encode_peer(Peer)};
                {error, no_such_peer} ->
                    JsonError = #json_error{
                      code = ?DS_JSONRPC_UNKNOWN_PEER,
                      message = <<"Unknown peer">>,
                      data = Na},
                    {error, JsonError}
            end;
        {error, Reason} ->
            JsonError = #json_error{
              code = ?JSONRPC_INVALID_PARAMS,
              message = ?l2b(ds_jsonrpc:format_error(Reason)),
              data = <<"na">>},
            {error, JsonError}
    end;
ds_handler(<<"get-all-peers">>, undefined) ->
    {ok, Peers} = ds_serv:get_all_peers(),
    {ok, ds_jsonrpc:encode_peers(Peers)};
ds_handler(<<"get-random-peers">>, [{<<"my-na">>, MyNa}, {<<"n">>, N}]) ->
    case node_jsonrpc:decode_na(MyNa) of
        {ok, DecodedMyNa} ->
            if
                is_integer(N) ->
                    case ds_serv:get_random_peers(DecodedMyNa, N) of
                        {ok, Peers} ->
                            {ok, ds_jsonrpc:encode_peers(Peers)};
                        {error, too_few_peers} ->
                            {error, #json_error{
                               code = ?DS_JSONRPC_TOO_FEW_PEERS}}
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
              message = ?l2b(ds_jsonrpc:format_error(Reason)),
              data = <<"my-na">>},
            {error, JsonError}
    end;
ds_handler(<<"publish-peer">>, [{<<"peer">>, [{<<"na">>, Na},
                                              {<<"public-key">>, PublicKey},
                                              {<<"flags">>, Flags}]}]) ->
    case node_jsonrpc:decode_na(Na) of
        {ok, DecodedNa} ->
            Peer = #peer{na = DecodedNa, public_key = PublicKey, flags = Flags},
            ds_serv:publish_peer(Peer);
        {error, Reason} ->
            JsonError = #json_error{
              code = ?JSONRPC_INVALID_PARAMS,
              message = ?l2b(ds_jsonrpc:format_error(Reason)),
              data = <<"na">>},
            {error, JsonError}
    end;
ds_handler(<<"unpublish-peer">>, [{<<"na">>, Na}]) ->
    case node_jsonrpc:decode_na(Na) of
        {ok, DecodedNa} ->
            ok = ds_serv:unpublish_peer(DecodedNa),
            {ok, true};
        {error, Reason} ->
            JsonError = #json_error{
              code = ?JSONRPC_INVALID_PARAMS,
              message = ?l2b(ds_jsonrpc:format_error(Reason)),
              data = <<"na">>},
            {error, JsonError}
    end;
ds_handler(<<"published-peers">>, [{<<"nas">>, Nas}]) ->
    case node_jsonrpc:decode_nas(Nas) of
        {ok, DecodedNas} ->
            {ok, PublishedNas} = ds_serv:published_peers(DecodedNas),
            {ok, node_jsonrpc:encode_nas(PublishedNas)};
        {error, Reason} ->
            JsonError = #json_error{
              code = ?JSONRPC_INVALID_PARAMS,
              message = ?l2b(ds_jsonrpc:format_error(Reason)),
              data = <<"na">>},
            {error, JsonError}
    end;
ds_handler(<<"reserve-oa">>, [{<<"oa">>, Oa}, {<<"na">>, Na}]) ->
    case node_jsonrpc:decode_na(Na) of
        {ok, DecodedNa} ->
            case node_jsonrpc:decode_oa(Oa) of
                {ok, DecodedOa} ->
                    case ds_serv:reserve_oa(DecodedOa, DecodedNa) of
                        ok ->
                            {ok, true};
                        {error, no_such_peer} ->
                            JsonError = #json_error{
                              code = ?DS_JSONRPC_UNKNOWN_PEER,
                              message = <<"Unknown peer">>,
                              data = Na},
                            {error, JsonError};
                        {error, too_many_oas} ->
                            JsonError = #json_error{
                              code = ?DS_JSONRPC_PERMISSION_DENIED,
                              message = <<"Too many reservations">>},
                            {error, JsonError}
                    end;
                {error, Reason} ->
                    JsonError = #json_error{
                      code = ?JSONRPC_INVALID_PARAMS,
                      message = ?l2b(ds_jsonrpc:format_error(Reason)),
                      data = <<"oa">>},
                    {error, JsonError}
            end;
        {error, Reason} ->
            JsonError = #json_error{
              code = ?JSONRPC_INVALID_PARAMS,
              message = ?l2b(ds_jsonrpc:format_error(Reason)),
              data = <<"na">>},
            {error, JsonError}
    end;
ds_handler(<<"reserved-oas">>, [{<<"na">>, Na}]) ->
    case node_jsonrpc:decode_na(Na) of
        {ok, DecodedNa} ->
            case ds_serv:reserved_oas(DecodedNa) of
                {ok, ReservedOas} ->
                    {ok, node_jsonrpc:encode_oas(ReservedOas)};
                {error, no_reserved_oas} ->
                    JsonError = #json_error{
                      code = ?DS_JSONRPC_NO_RESERVED_OAS,
                      message = <<"No reserved overlay addresses">>,
                      data = Na},
                    {error, JsonError}
            end;
        {error, Reason} ->
            JsonError = #json_error{
              code = ?JSONRPC_INVALID_PARAMS,
              message = ?l2b(ds_jsonrpc:format_error(Reason)),
              data = <<"na">>},
            {error, JsonError}
    end;
ds_handler(Method, Params) ->
    ?error_log({invalid_request, Method, Params}),
    JsonError = #json_error{code = ?JSONRPC_INVALID_REQUEST},
    {error, JsonError}.
