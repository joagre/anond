-module(jsonrpc_serv).

%%% external exports
-export([start_link/4]).

%%% internal exports
-export([jsonrpc_handler/2]).

%%% include files
-include_lib("util/include/jsonrpc.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants
-define(MAX_SESSIONS, 1024).
-define(MAX_CONTENT_LENGTH, 65*1024).

%%% records

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link(inet:ip_address(), inet:port_number(), tcp_serv:options(),
                 tcp_serv:handler()) ->
                        {ok, pid()}.

start_link(IpAddress, Port, Options, Handler) ->
    SocketOptions =
        [{packet, http_bin}, {active, false}, {ip, IpAddress},
         {reuseaddr, true}],
    tcp_serv:start_link(Port, ?MAX_SESSIONS, Options, SocketOptions,
                        {?MODULE, jsonrpc_handler, [Handler]}).

jsonrpc_handler(Socket, Handler) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, {http_request, 'POST', {abs_path, <<"/jsonrpc">>}, {1, 1}}} ->
            {ok, HeaderValues} =
                httplib:get_headers(Socket, [{'content-length', -1}]),
            BinaryContentLength =
                httplib:lookup_header_value('content-length', HeaderValues),
            case catch ?b2i(BinaryContentLength) of
                ContentLength when is_integer(ContentLength) ->
                    jsonrpc_handler(Socket, Handler, ContentLength);
                _->
                    JsonError = #json_error{code = ?JSONRPC_INVALID_REQUEST},
                    send(Socket, null, JsonError)
            end;
        {ok, _} ->
            JsonError = #json_error{code = ?JSONRPC_INVALID_REQUEST},
            send(Socket, null, JsonError);
        {error, Reason} ->
            {error, Reason}
    end.

jsonrpc_handler(Socket, _Handler, -1) ->
    JsonError = #json_error{
      code = ?JSONRPC_INVALID_REQUEST,
      message = <<"Content length not specified">>},
    send(Socket, null, JsonError);    
jsonrpc_handler(Socket, {M, F, A}, ContentLength)
  when ContentLength < ?MAX_CONTENT_LENGTH ->
    case recv(Socket, ContentLength) of
        {ok, Method, Params, Id} ->
            case apply(M, F, [Method, Params|A]) of
                {ok, Result} ->
                    send(Socket, Id, Result);
                {error, JsonError} when is_record(JsonError, json_error) ->
                    send(Socket, Id, JsonError)
            end;
        invalid_json ->
            JsonError = #json_error{code = ?JSONRPC_PARSE_ERROR},
            send(Socket, null, JsonError);
        {error, Reason} ->
            JsonError = #json_error{
              code = ?JSONRPC_INTERNAL_ERROR,
              message = ?l2b(inet:format_error(Reason))},
            send(Socket, null, JsonError)
    end;
jsonrpc_handler(Socket, _Handler, ContentLength) ->
    JsonError = #json_error{
      code = ?JSONRPC_INVALID_REQUEST,
      message = <<"Content is too large">>,
      data = ContentLength},
    send(Socket, null, JsonError).

%%%
%%% recv
%%%

recv(Socket, ContentLength) ->
    ok = inet:setopts(Socket, [binary, {packet, 0}]),
    case gen_tcp:recv(Socket, ContentLength) of
        {ok, EncodedJson} ->
            case catch jsx:decode(EncodedJson) of
                [{<<"jsonrpc">>, <<"2.0">>},
                 {<<"method">>, Method},
                 {<<"params">>, Params},
                 {<<"id">>, Id}] ->
                    {ok, Method, Params, Id};
                [{<<"jsonrpc">>, <<"2.0">>},
                 {<<"method">>, Method},
                 {<<"id">>, Id}] ->
                    {ok, Method, undefined, Id};
                _ ->
                    invalid_json
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% send
%%%

send(Socket, Id, #json_error{code = Code, message = Message, data = Data}) ->
    case Message of
        undefined ->
            UpdatedMessage = message(Code);
        UpdatedMessage ->
            ok
    end,
    Json =
        [{<<"jsonrpc">>, <<"2.0">>},
         {<<"error">>, [{<<"code">>, Code}]++
              message_if_any(UpdatedMessage)++
              data_if_any(Data)},
         {<<"id">>, Id}],
    send_to_client(Socket, Id, Json);
send(Socket, Id, Result) ->
    Json =
        [{<<"jsonrpc">>, <<"2.0">>},
         {<<"result">>, Result},
         {<<"id">>, Id}],
    send_to_client(Socket, Id, Json).

message(?JSONRPC_PARSE_ERROR) ->
    <<"Invalid JSON was received by the server">>;
message(?JSONRPC_INVALID_REQUEST) ->
    <<"The JSON sent is not a valid Request object">>;
message(?JSONRPC_METHOD_NOT_FOUND) ->
    <<"The method does not exist / is not available">>;
message(?JSONRPC_INVALID_PARAMS) ->
    <<"Invalid method parameter(s)">>;
message(?JSONRPC_INTERNAL_ERROR) ->
    <<"Internal JSON-RPC error">>;
message(_) ->
    undefined.

message_if_any(undefined) ->
    [];
message_if_any(Message) ->
    [{<<"message">>, Message}].

data_if_any(undefined) ->
    [];
data_if_any(Data) ->
    [{<<"data">>, Data}].

send_to_client(Socket, Id, Json) ->
    case catch jsx:encode(Json) of
        EncodedJson when is_binary(EncodedJson) ->
            case jsx:prettify(EncodedJson) of
                PrettifiedJson when is_binary(PrettifiedJson) ->
                    ContentLength = ?i2l(size(PrettifiedJson)),
                    gen_tcp:send(
                      Socket,
                      [<<"HTTP/1.1 200\r\n">>,
                       <<"Content-Type: application/json\r\n">>,
                       <<"Content-Length: ">>, ContentLength, <<"\r\n">>,
                       <<"Connection: close\r\n\r\n">>,
                       PrettifiedJson])
            end;
        _ ->
            JsonError = #json_error{code = ?JSONRPC_INTERNAL_ERROR},
            send(Socket, Id, JsonError)
    end.
