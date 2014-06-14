-module(jsonrpc_client).

%%% external exports
-export([call/4, call/5, call/6, call/8, call/9]).
-export([sort_properties/1]).

%%% internal exports

%%% include files
-include_lib("util/include/jsonrpc_serv.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants
-define(CALL_TIMEOUT, 60*1000).

%%% records

%%% types
-type error_reason() :: 'invalid_response' |
                        #json_error{} |
                        'invalid_params' |
                        httplib:post_error_reason().

%%%
%%% exported: call
%%%

call(NodeId, MyIpAddress, IpAddressPort, Method) ->
    call(NodeId, MyIpAddress, IpAddressPort, ?CALL_TIMEOUT, <<"/jsonrpc">>,
         Method, undefined, undefined, ssl).

call(NodeId, MyIpAddress, IpAddressPort, Method, SecretKey) ->
    call(NodeId, MyIpAddress, IpAddressPort, ?CALL_TIMEOUT, <<"/jsonrpc">>,
         Method, SecretKey, undefined, ssl).

call(NodeId, MyIpAddress, IpAddressPort, Method, SecretKey, Params) ->
    call(NodeId, MyIpAddress, IpAddressPort, ?CALL_TIMEOUT, <<"/jsonrpc">>,
         Method, SecretKey, Params, ssl).

call(NodeId, MyIpAddress, IpAddressPort, Timeout, Uri, Method, SecretKey,
     Params) ->
    call(NodeId, MyIpAddress, IpAddressPort, Timeout, Uri, Method, SecretKey,
         Params, ssl).

-spec call(integer(), inet:ip_address() | 'undefined',
           httplib:ip_address_port(), timeout(), binary(), binary(),
           binary() | 'undefined', jsx:json_term() | 'undefined',
           httplib:transport_module()) ->
                  {'ok', jsx:json_term()} | {'error', error_reason()}.

call(NodeId, MyIpAddress, IpAddressPort, Timeout, Uri, Method, SecretKey,
     Params, TransportModule) ->
    Id = new_id(),
    Request =
        [{<<"jsonrpc">>, <<"2.0">>},
         {<<"method">>, Method}]++
        params_if_any(Params)++
        [{<<"id">>, Id}],
    case catch jsx:encode(Request) of
        EncodedRequest when is_binary(EncodedRequest) ->
            ExtraHeaders =
                if
                    NodeId == 0 ->
                        [];
                    true ->
                        [{<<"Content-HMAC">>, hmac(EncodedRequest, SecretKey)},
                         {<<"Node-ID">>, ?i2b(NodeId)}]
                end,
            case httplib:post(
                   TransportModule, MyIpAddress, IpAddressPort, Timeout, Uri,
                   <<"application/json">>, EncodedRequest, ExtraHeaders) of
                {ok, Response} ->
                    case catch jsx:decode(Response) of
                        {'EXIT', _} ->
                            {error, invalid_response};
                        DecodedResponse ->
                            handle_response(
                              Id, Response, sort_properties(DecodedResponse))
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

hmac(Message, SecretKey) ->
    base64:encode(salt:crypto_sign(salt:crypto_hash(Message), SecretKey)).

handle_response(Id, _Response,
                [{<<"error">>, [{<<"code">>, Code},
                                {<<"data">>, Data},
                                {<<"message">>, Message}]},
                 {<<"id">>, Id},
                 {<<"jsonrpc">>, <<"2.0">>}]) ->
    JsonError = #json_error{
      code = Code,
      message = Message,
      data = Data},
    {error, JsonError};
handle_response(Id, _Response,
                [{<<"error">>, [{<<"code">>, Code},
                                {<<"message">>, Message}]},
                 {<<"id">>, Id},
                 {<<"jsonrpc">>, <<"2.0">>}]) ->
    JsonError = #json_error{
      code = Code,
      message = Message},
    {error, JsonError};
handle_response(Id, _Response,
                [{<<"error">>, [{<<"code">>, Code}]},
                 {<<"id">>, Id},
                 {<<"jsonrpc">>, <<"2.0">>}]) ->
    JsonError = #json_error{code = Code},
    {error, JsonError};
handle_response(Id, _Response,
                [{<<"id">>, Id},
                 {<<"jsonrpc">>, <<"2.0">>},
                 {<<"result">>, Result}]) ->
    {ok, Result};
handle_response(_Id, _Response, _DecodedResponse) ->
    {error, invalid_response}.

%%%
%%% exported: sort_properties
%%%

-spec sort_properties(jsx:json_term()) -> jsx:json_term().

sort_properties(Object) when is_list(Object) andalso is_tuple(hd(Object)) ->
    [{Property, sort_properties(Value)} ||
        {Property, Value} <- lists:keysort(1, Object)];
sort_properties(Value) ->
    Value.
