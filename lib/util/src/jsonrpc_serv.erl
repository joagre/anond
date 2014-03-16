-module(jsonrpc_serv).

%%% external exports
-export([start_link/7]).

%%% internal exports
-export([jsonrpc_handler/4]).

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
-type options() :: [option()].
-type option() :: {'lookup_public_key',
                   fun((httplib:ip_address_port(), binary()) ->
                              'ignore' | 'not_found' | binary())}.

%%%
%%% exported: start_link
%%%

-spec start_link(options(), inet:ip_address(), inet:port_number(), binary(),
                 net_serv:options(), net_serv:handler(),
                 binary() | 'undefined') ->
                        {ok, pid()}.

start_link(Options, IpAddress, Port, CertFile, NetServOptions, Handler,
           Docroot) ->
    TransportOptions =
        [{certfile, CertFile}, {packet, http_bin}, {active, false},
         {ip, IpAddress}, {reuseaddr, true}],
    net_serv:start_link(Port, NetServOptions, ssl, TransportOptions,
                        {?MODULE, jsonrpc_handler,
                         [Options, Handler, Docroot]}).

jsonrpc_handler(Socket, Options, Handler, Docroot) ->
    case ssl:recv(Socket, 0) of
        %% a jsonrpc request
        {ok, {http_request, 'POST', {abs_path, <<"/jsonrpc">>}, {1, 1}}} ->
            ok = ssl:setopts(Socket, [{packet, httph_bin}]),
            {ok, HeaderValues} =
                httplib:get_headers(ssl, Socket,
                                    [{'content-length', <<"-1">>},
                                     {<<"content-hmac">>, not_set},
                                     {<<"local-port">>, <<"-1">>}]),
            ok = ssl:setopts(Socket, [binary, {packet, 0}]),
            ContentLength =
                httplib:lookup_header_value('content-length', HeaderValues),
            ContentHMAC =
                httplib:lookup_header_value(<<"content-hmac">>, HeaderValues),
            LocalPort =
                httplib:lookup_header_value(<<"local-port">>, HeaderValues),
            case catch {?b2i(ContentLength), ?b2i(LocalPort)} of
                {DecodedContentLength, DecodedLocalPort}
                  when is_integer(DecodedContentLength) andalso
                       is_integer(DecodedLocalPort) ->
                    handle_jsonrpc_request(
                      Socket, Options, Handler, DecodedContentLength,
                      ContentHMAC, DecodedLocalPort);
                Error ->
                    ?error_log({Error, ContentLength, LocalPort}),
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

handle_jsonrpc_request(Socket, _Options, _Handler, -1, _ContentHMAC,
                       _LocalPort) ->
    JsonError = #json_error{
      code = ?JSONRPC_INVALID_REQUEST,
      message = <<"Content length not specified">>},
    send(Socket, null, JsonError);
handle_jsonrpc_request(Socket, Options, {Module, Function, Args}, ContentLength,
                       ContentHMAC, LocalPort)
  when ContentLength < ?MAX_REQUEST_SIZE ->
    case recv(Socket, Options, ContentLength, ContentHMAC, LocalPort) of
        {ok, Method, Params, Id} ->
            case apply(Module, Function, [Method, Params|Args]) of
                {ok, Result} ->
                    send(Socket, Id, Result);
                {error, JsonError} when is_record(JsonError, json_error) ->
                    send(Socket, Id, JsonError)
            end;
        invalid_json ->
            JsonError = #json_error{code = ?JSONRPC_PARSE_ERROR},
            send(Socket, null, JsonError);
        invalid_signature ->
            JsonError = #json_error{
              code = ?JSONRPC_INVALID_REQUEST,
              message = <<"Invalid signature">>},
            send(Socket, null, JsonError);
        {error, Reason} ->
            JsonError = #json_error{
              code = ?JSONRPC_INTERNAL_ERROR,
              message = ?l2b(ssl:format_error(Reason))},
            send(Socket, null, JsonError)
    end;
handle_jsonrpc_request(Socket, _Options, _Handler, ContentLength, _ContentHMAC,
                       _LocalPort) ->
    JsonError = #json_error{
      code = ?JSONRPC_INVALID_REQUEST,
      message = <<"Content is too large">>,
      data = ContentLength},
    send(Socket, null, JsonError).

%%%
%%% recv
%%%

recv(Socket, Options, ContentLength, ContentHMAC, LocalPort) ->
    ok = ssl:setopts(Socket, [binary, {packet, 0}]),
    case ssl:recv(Socket, ContentLength) of
        {ok, Response} ->
            case catch jsx:decode(Response) of
                [{<<"jsonrpc">>, <<"2.0">>},
                 {<<"method">>, Method},
                 {<<"params">>, Params},
                 {<<"id">>, Id}] ->
                    case verify_hmac(
                           Socket, Options, ContentHMAC, LocalPort, Response,
                           Method) of
                        true ->
                            {ok, Method, Params, Id};
                        false ->
                            invalid_signature
                    end;
                [{<<"jsonrpc">>, <<"2.0">>},
                 {<<"method">>, Method},
                 {<<"id">>, Id}] ->
                    case verify_hmac(
                           Socket, Options, ContentHMAC, LocalPort, Response,
                           Method) of
                        true ->
                            {ok, Method, undefined, Id};
                        false ->
                            invalid_signature
                    end;
                Error ->
                    ?error_log({Error, Response}),
                    invalid_json
            end;
        {error, Reason} ->
            {error, Reason}
    end.

verify_hmac(Socket, Options, ContentHMAC, LocalPort, Response, Method) ->
    case lists:keysearch(lookup_public_key, 1, Options) of
        {value, {lookup_public_key, LookupPublicKey}} ->
            case ssl:peername(Socket) of
                {ok, {IpAddress, _EphemeralPort}} ->
                    case LookupPublicKey({IpAddress, LocalPort}, Method) of
                        ignore ->
                            true;
                        not_found ->
                            false;
                        _PublicKey when ContentHMAC == not_set ->
                            false;
                        PublicKey ->
                            {ok, salt:crypto_hash(Response)} ==
                                salt:crypto_sign_open(
                                  base64:decode(ContentHMAC), PublicKey)
                    end;
                {error, _Reason} ->
                    false
            end;
        false ->
            true
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
        Error ->
            ?error_log({Error, Request}),
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
