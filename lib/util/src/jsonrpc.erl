-module(jsonrpc).

%%% external exports
-export([call/3, call/4, call/5, call/7]).

%%% internal exports

%%% include files
-include_lib("util/include/jsonrpc_serv.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants
-define(CALL_TIMEOUT, 60*1000).

%%% records

%%% types
-type error_reason() :: {'invalid_response', jsx:json_text()} |
                        #json_error{} |
                        'invalid_params' |
                        httplib:post_error_reason().

%%%
%%% exported: call
%%%

call(LocalIpAddressPort, IpAddressPort, Method) ->
    call(LocalIpAddressPort, IpAddressPort, ?CALL_TIMEOUT, <<"/jsonrpc">>,
         Method, undefined, undefined).

call(LocalIpAddressPort, IpAddressPort, Method, PrivateKey) ->
    call(LocalIpAddressPort, IpAddressPort, ?CALL_TIMEOUT, <<"/jsonrpc">>,
         Method, PrivateKey, undefined).

call(LocalIpAddressPort, IpAddressPort, Method, PrivateKey, Params) ->
    call(LocalIpAddressPort, IpAddressPort, ?CALL_TIMEOUT, <<"/jsonrpc">>,
         Method, PrivateKey, Params).

-spec call(httplib:ip_address_port() | 'undefined', httplib:ip_address_port(),
           timeout(), binary(), binary(), binary() | 'undefined',
           jsx:json_term() | 'undefined') ->
                  {'ok', jsx:json_term()} | {'error', error_reason()}.

call(LocalIpAddressPort, IpAddressPort, Timeout, Uri, Method, PrivateKey,
     Params) ->
    Id = new_id(),
    Request =
        [{<<"jsonrpc">>, <<"2.0">>},
         {<<"method">>, Method}]++
        params_if_any(Params)++
        [{<<"id">>, Id}],
    case catch jsx:encode(Request) of
        EncodedRequest when is_binary(EncodedRequest) ->
            case LocalIpAddressPort of
                undefined ->
                    LocalIpAddress = undefined,
                    LocalPort = -1;
                {LocalIpAddress, LocalPort} ->
                    ok
            end,
            ExtraHeaders =
                if
                    PrivateKey == undefined ->
                        [];
                    true ->
                        [{<<"Content-HMAC">>, hmac(EncodedRequest, PrivateKey)},
                         {<<"Local-Port">>, ?i2b(LocalPort)}]
                end,
            case httplib:post(ssl, LocalIpAddress, IpAddressPort, Timeout,
                              Uri, <<"application/json">>, EncodedRequest,
                              ExtraHeaders) of
                {ok, Response} ->
                    case catch jsx:decode(Response) of
                        {'EXIT', _} ->
                            {error, {invalid_response, Response}};
                        [{<<"jsonrpc">>, <<"2.0">>},
                         {<<"error">>, [{<<"code">>, Code},
                                        {<<"message">>, Message},
                                        {<<"data">>, Data}]},
                         {<<"id">>, Id}] ->
                            JsonError = #json_error{
                              code = Code,
                              message = Message,
                              data = Data},
                            {error, JsonError};
                        [{<<"jsonrpc">>, <<"2.0">>},
                         {<<"error">>, [{<<"code">>, Code},
                                        {<<"message">>, Message}]},
                         {<<"id">>, Id}] ->
                            JsonError = #json_error{
                              code = Code,
                              message = Message},
                            {error, JsonError};
                        [{<<"jsonrpc">>, <<"2.0">>},
                         {<<"error">>, [{<<"code">>, Code}]},
                         {<<"id">>, Id}] ->
                            JsonError = #json_error{code = Code},
                            {error, JsonError};
                        [{<<"jsonrpc">>, <<"2.0">>},
                         {<<"result">>, Result},
                         {<<"id">>, Id}] ->
                            {ok, Result};
                        _ ->
                            {error, {invalid_response, Response}}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        Error ->
            ?error_log({Request, Error}),
            {error, invalid_params}
    end.

new_id() ->
    case get(ds_jsonrpc_id) of
        undefined ->
            put(ds_jsonrpc_id, 1),
            1;
        Id ->
            put(ds_jsonrpc_id, Id+1),
            Id
    end.

params_if_any(undefined) ->
    [];
params_if_any(Params) ->
    [{<<"params">>, Params}].

hmac(Message, PrivateKey) ->
    base64:encode(
      salt:crypto_sign(salt:crypto_hash(Message), PrivateKey)).
