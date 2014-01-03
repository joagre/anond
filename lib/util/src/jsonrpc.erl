-module(jsonrpc).

%%% external exports
-export([call/4, call/5, call/7]).

%%% internal exports

%%% include files
-include_lib("util/include/jsonrpc_serv.hrl").
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

-spec call(inet:ip_address() | 'undefined', inet:ip_address(),
           inet:port_number(), binary()) ->
                  {'ok', jsx:json_term()}  | {'error', error_reason()}.

call(NicIpAddress, IpAddress, Port, Method) ->
    call(NicIpAddress, IpAddress, Port, ?CALL_TIMEOUT, <<"/jsonrpc">>, Method,
         undefined).

-spec call(inet:ip_address() | 'undefined', inet:ip_address(),
           inet:port_number(), binary(), jsx:json_term() | 'undefined') ->
                  {'ok',  jsx:json_term()} | {'error', error_reason()}.

call(NicIpAddress, IpAddress, Port, Method, Params) ->
    call(NicIpAddress, IpAddress, Port, ?CALL_TIMEOUT, <<"/jsonrpc">>, Method,
         Params).

-spec call(inet:ip_address() | 'undefined', inet:ip_address(),
           inet:port_number(), timeout(), binary(), binary(),
           jsx:json_term() | 'undefined') ->
                  {'ok', jsx:json_term()} | {'error', error_reason()}.

call(NicIpAddress, IpAddress, Port, Timeout, Uri, Method, Params) ->
    Id = new_id(),
    Request =
        [{<<"jsonrpc">>, <<"2.0">>},
         {<<"method">>, Method}]++
        params_if_any(Params)++
        [{<<"id">>, Id}],
    case catch jsx:encode(Request) of
        EncodedRequest when is_binary(EncodedRequest) ->
            PrettifiedRequest = jsx:prettify(EncodedRequest),
            case httplib:post(NicIpAddress, IpAddress, Port, Timeout,
                              Uri, <<"application/json">>, PrettifiedRequest) of
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
        _ ->
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
