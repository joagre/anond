-module(jsonrpc_serv).

%%% external exports
-export([start_link/6]).

%%% internal exports
-export([jsonrpc_handler/3]).

%%% include files
-include_lib("kernel/include/file.hrl").
-include_lib("util/include/jsonrpc_serv.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants
-define(MAX_SESSIONS, 1024).
-define(MAX_REQUEST_SIZE, 65*1024).

%%% records

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link(inet:ip_address(), inet:port_number(), binary(),
                 net_serv:options(), net_serv:handler(),
                 binary() | 'undefined') ->
                        {ok, pid()}.

start_link(IpAddress, Port, CertFile, Options, Handler, Docroot) ->
    TransportOptions =
        [{certfile, CertFile}, {packet, http_bin}, {active, false},
         {ip, IpAddress}, {reuseaddr, true}],
    net_serv:start_link(Port, Options, ssl, TransportOptions,
                        {?MODULE, jsonrpc_handler, [Handler, Docroot]}).

jsonrpc_handler(Socket, Handler, Docroot) ->
    case ssl:recv(Socket, 0) of
        %% a jsonrpc request
        {ok, {http_request, 'POST', {abs_path, <<"/jsonrpc">>}, {1, 1}}} ->
            ok = ssl:setopts(Socket, [{packet, httph_bin}]),
            {ok, HeaderValues} =
                httplib:get_headers(ssl, Socket, [{'content-length', -1}]),
            ok = ssl:setopts(Socket, [binary, {packet, 0}]),
            BinaryContentLength =
                httplib:lookup_header_value('content-length', HeaderValues),
            case catch ?b2i(BinaryContentLength) of
                ContentLength when is_integer(ContentLength) ->
                    handle_jsonrpc_request(Socket, Handler, ContentLength);
                _ ->
                    JsonError = #json_error{code = ?JSONRPC_INVALID_REQUEST},
                    send(Socket, null, JsonError)
            end;
        %% a request for a file
        {ok, {http_request, 'GET', {abs_path, Path}, {1, 1}}}
          when Docroot /= undefined ->
            ok = ssl:setopts(Socket, [{packet, httph_bin}]),
            %% just throw away header values for now
            {ok, _HeaderValues} = httplib:get_headers(ssl, Socket, []),
            ok = ssl:setopts(Socket, [binary, {packet, 0}]),
            send_file(Socket, Docroot, ?b2l(Path));
        {ok, _} ->
            JsonError = #json_error{code = ?JSONRPC_INVALID_REQUEST},
            send(Socket, null, JsonError);
        {error, Reason} ->
            {error, Reason}
    end.

handle_jsonrpc_request(Socket, _Handler, -1) ->
    JsonError = #json_error{
      code = ?JSONRPC_INVALID_REQUEST,
      message = <<"Content length not specified">>},
    send(Socket, null, JsonError);
handle_jsonrpc_request(Socket, {M, F, A}, ContentLength)
  when ContentLength < ?MAX_REQUEST_SIZE ->
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
              message = ?l2b(ssl:format_error(Reason))},
            send(Socket, null, JsonError)
    end;
handle_jsonrpc_request(Socket, _Handler, ContentLength) ->
    JsonError = #json_error{
      code = ?JSONRPC_INVALID_REQUEST,
      message = <<"Content is too large">>,
      data = ContentLength},
    send(Socket, null, JsonError).

%%%
%%% recv
%%%

recv(Socket, ContentLength) ->
    ok = ssl:setopts(Socket, [binary, {packet, 0}]),
    case ssl:recv(Socket, ContentLength) of
        {ok, Response} ->
            case catch jsx:decode(Response) of
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
    Request =
        [{<<"jsonrpc">>, <<"2.0">>},
         {<<"error">>, [{<<"code">>, Code}]++
              message_if_any(UpdatedMessage)++
              data_if_any(Data)},
         {<<"id">>, Id}],
    send_to_client(Socket, Id, Request);
send(Socket, Id, Result) ->
    Request =
        [{<<"jsonrpc">>, <<"2.0">>},
         {<<"result">>, Result},
         {<<"id">>, Id}],
    send_to_client(Socket, Id, Request).

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

send_to_client(Socket, Id, Request) ->
    case catch jsx:encode(Request) of
        EncodedRequest when is_binary(EncodedRequest) ->
            PrettifiedRequest = jsx:prettify(EncodedRequest),
            ContentLength = ?i2l(size(PrettifiedRequest)),
            ssl:send(
              Socket,
              [<<"HTTP/1.1 200 OK\r\n">>,
               <<"Content-Type: application/json\r\n">>,
               <<"Content-Length: ">>, ContentLength, <<"\r\n">>,
               <<"Connection: close\r\n\r\n">>,
               PrettifiedRequest]);
        _ ->
            JsonError = #json_error{code = ?JSONRPC_INTERNAL_ERROR},
            send(Socket, Id, JsonError)
    end.

%%%
%%% send_file
%%%

send_file(Socket, Docroot, "/") ->
    send_file(Socket, Docroot, "index.html");
send_file(Socket, Docroot, [$/|Rest]) ->
    send_file(Socket, Docroot, Rest);
send_file(Socket, Docroot, Path) ->
    AbsPath = lists:takewhile(fun(C) -> C /= $? end, Path),
    case string:str(Path, "..") of
        0 ->
            FilePath = filename:join([Docroot, AbsPath]),
            case file:read_file_info(FilePath) of
                {ok, #file_info{size = Size}} when Size /= undefined ->
                    ok = ssl:send(
                           Socket,
                           ["HTTP/1.1 200\r\n"
                            "Content-Type: ", get_mime_type(AbsPath), "\r\n",
                            "Content-Length: ", ?i2l(Size), "\r\n",
                            "Connection: close\r\n\r\n"]),
                    httplib:send_file(ssl, Socket, FilePath);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

get_mime_type(Path) ->
    mime_types:lookup(tl(string:to_lower(filename:extension(Path)))).
