-module(jsonrpc_serv).

%%% external exports
-export([start_link/7]).

%%% internal exports
-export([jsonrpc_handler/5]).

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
                              'ignore' | 'not_found' | binary())} |
                  {'transport_module', httplib:transport_module()}.

%%%
%%% exported: start_link
%%%

-spec start_link(options(), inet:ip_address(), inet:port_number(), binary(),
                 tcp_serv:options(), tcp_serv:handler(),
                 binary() | 'undefined') ->
                        {ok, pid()}.

start_link(Options, IpAddress, Port, CertFile, TcpServOptions, Handler,
           Docroot) ->
    TransportOptions =
        [{certfile, CertFile}, {packet, http_bin}, {active, false},
         {ip, IpAddress}, {reuseaddr, true}],
    case lists:keysearch(transport_module, 1, Options) of
        {value, {_, TransportModule}} ->
            ok;
        false ->
            TransportModule = ssl
    end,
    tcp_serv:start_link(Port, TcpServOptions, TransportModule, TransportOptions,
                        {?MODULE, jsonrpc_handler,
                         [Options, Handler, Docroot, TransportModule]}).

jsonrpc_handler(Socket, Options, Handler, Docroot, TransportModule) ->
    case TransportModule:recv(Socket, 0) of
        %% a jsonrpc request
        {ok, {http_request, 'POST', {abs_path, <<"/jsonrpc">>}, {1, 1}}} ->
            ok = TransportModule:setopts(Socket, [{packet, httph_bin}]),
            {ok, HeaderValues} =
                httplib:get_headers(TransportModule, Socket,
                                    [{'content-length', <<"-1">>},
                                     {<<"content-hmac">>, not_set},
                                     {<<"my-port">>, <<"-1">>}]),
            ok = TransportModule:setopts(Socket, [binary, {packet, 0}]),
            ContentLength =
                httplib:lookup_header_value('content-length', HeaderValues),
            ContentHMAC =
                httplib:lookup_header_value(<<"content-hmac">>, HeaderValues),
            MyPort = httplib:lookup_header_value(<<"my-port">>, HeaderValues),
            case catch {?b2i(ContentLength), ?b2i(MyPort)} of
                {DecodedContentLength, DecodedMyPort}
                  when is_integer(DecodedContentLength) andalso
                       is_integer(DecodedMyPort) ->
                    handle_jsonrpc_request(
                      Socket, Options, Handler, TransportModule,
                      DecodedContentLength, ContentHMAC, DecodedMyPort);
                Error ->
                    ?error_log({Error, ContentLength, MyPort}),
                    JsonError = #json_error{code = ?JSONRPC_INVALID_REQUEST},
                    send(Socket, TransportModule, null, JsonError)
                end;
        %% a request for a file
        {ok, {http_request, 'GET', {abs_path, Path}, {1, 1}}}
          when Docroot /= undefined ->
            ok = TransportModule:setopts(Socket, [{packet, httph_bin}]),
            %% just throw away header values for now
            {ok, _HeaderValues} =
                httplib:get_headers(TransportModule, Socket, []),
            ok = TransportModule:setopts(Socket, [binary, {packet, 0}]),
            send_file(Socket, Docroot, TransportModule, ?b2l(Path));
        {ok, _} ->
            JsonError = #json_error{code = ?JSONRPC_INVALID_REQUEST},
            send(Socket, TransportModule, null, JsonError);
        {error, Reason} ->
            {error, Reason}
    end.

handle_jsonrpc_request(
  Socket, _Options, _Handler, TransportModule, -1, _ContentHMAC, _MyPort) ->
    JsonError = #json_error{
      code = ?JSONRPC_INVALID_REQUEST,
      message = <<"Content length not specified">>},
    send(Socket, TransportModule, null, JsonError);
handle_jsonrpc_request(
  Socket, Options, {Module, Function, Args}, TransportModule, ContentLength,
  ContentHMAC, MyPort) when ContentLength < ?MAX_REQUEST_SIZE ->
    case peername(TransportModule, Socket) of
        {ok, {MyIpAddress, _EphemeralPort}} ->
            ok;
        {error, _Reason} ->
            MyIpAddress = undefined
    end,
    case recv(Socket, Options, TransportModule, ContentLength, ContentHMAC,
              MyPort, MyIpAddress) of
        {ok, Method, Params, Id} ->
            case apply(Module, Function,
                       [{MyIpAddress, MyPort}, Method, Params|Args]) of
                {ok, Result} ->
                    send(Socket, TransportModule, Id, Result);
                {error, JsonError} when is_record(JsonError, json_error) ->
                    send(Socket, TransportModule, Id, JsonError)
            end;
        invalid_json ->
            JsonError = #json_error{code = ?JSONRPC_PARSE_ERROR},
            send(Socket, TransportModule, null, JsonError);
        invalid_signature ->
            JsonError = #json_error{
              code = ?JSONRPC_INVALID_REQUEST,
              message = <<"Invalid signature">>},
            send(Socket, TransportModule, null, JsonError);
        {error, Reason} ->
            JsonError = #json_error{
              code = ?JSONRPC_INTERNAL_ERROR,
              message = ?l2b(TransportModule:format_error(Reason))},
            send(Socket, TransportModule, null, JsonError)
    end;
handle_jsonrpc_request(
  Socket, _Options, _Handler, TransportModule, ContentLength, _ContentHMAC,
  _MyPort) ->
    JsonError = #json_error{
      code = ?JSONRPC_INVALID_REQUEST,
      message = <<"Content is too large">>,
      data = ContentLength},
    send(Socket, TransportModule, null, JsonError).

peername(gen_tcp, Socket) ->
    peername(inet, Socket);
peername(TransportModule, Socket) ->
    TransportModule:peername(Socket).

%%%
%%% recv
%%%

recv(Socket, Options, TransportModule, ContentLength, ContentHMAC, MyPort,
     MyIpAddress) ->
    ok = TransportModule:setopts(Socket, [binary, {packet, 0}]),
    case TransportModule:recv(Socket, ContentLength) of
        {ok, Response} ->
            case catch jsx:decode(Response) of
                [{<<"jsonrpc">>, <<"2.0">>},
                 {<<"method">>, Method},
                 {<<"params">>, Params},
                 {<<"id">>, Id}] ->
                    case verify_hmac(Options, ContentHMAC, MyPort, MyIpAddress,
                                     Response, Method) of
                        true ->
                            {ok, Method, Params, Id};
                        false ->
                            invalid_signature
                    end;
                [{<<"jsonrpc">>, <<"2.0">>},
                 {<<"method">>, Method},
                 {<<"id">>, Id}] ->
                    case verify_hmac(Options, ContentHMAC, MyPort, MyIpAddress,
                                     Response, Method) of
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

verify_hmac(Options, ContentHMAC, MyPort, MyIpAddress, Response, Method) ->
    case lists:keysearch(lookup_public_key, 1, Options) of
        {value, {lookup_public_key, _LookupPublicKey}}
          when MyIpAddress == undefined ->
            false;
        {value, {lookup_public_key, LookupPublicKey}} ->
            case LookupPublicKey({MyIpAddress, MyPort}, Method) of
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
        false ->
            true
    end.

%%%
%%% send
%%%

send(Socket, TransportModule, Id,
     #json_error{code = Code, message = Message, data = Data}) ->
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
    send_to_client(Socket, TransportModule, Id, Request);
send(Socket, TransportModule, Id, Result) ->
    Request =
        [{<<"jsonrpc">>, <<"2.0">>},
         {<<"result">>, Result},
         {<<"id">>, Id}],
    send_to_client(Socket, TransportModule, Id, Request).

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

send_to_client(Socket, TransportModule, Id, Request) ->
    case catch jsx:encode(Request) of
        EncodedRequest when is_binary(EncodedRequest) ->
            PrettifiedRequest = jsx:prettify(EncodedRequest),
            ContentLength = ?i2l(size(PrettifiedRequest)),
            TransportModule:send(
              Socket,
              [<<"HTTP/1.1 200 OK\r\n">>,
               <<"Content-Type: application/json\r\n">>,
               <<"Content-Length: ">>, ContentLength, <<"\r\n">>,
               <<"Connection: close\r\n\r\n">>,
               PrettifiedRequest]);
        Error ->
            ?error_log({Error, Request}),
            JsonError = #json_error{code = ?JSONRPC_INTERNAL_ERROR},
            send(Socket, TransportModule, Id, JsonError)
    end.

%%%
%%% send_file
%%%

send_file(Socket, Docroot, TransportModule, "/") ->
    send_file(Socket, Docroot, TransportModule, "index.html");
send_file(Socket, Docroot, TransportModule, [$/|Rest]) ->
    send_file(Socket, Docroot, TransportModule, Rest);
send_file(Socket, Docroot, TransportModule, Path) ->
    AbsPath = lists:takewhile(fun(C) -> C /= $? end, Path),
    case string:str(Path, "..") of
        0 ->
            FilePath = filename:join([Docroot, AbsPath]),
            case file:read_file_info(FilePath) of
                {ok, #file_info{size = Size}} when Size /= undefined ->
                    ok = TransportModule:send(
                           Socket,
                           ["HTTP/1.1 200\r\n"
                            "Content-Type: ", get_mime_type(AbsPath), "\r\n",
                            "Content-Length: ", ?i2l(Size), "\r\n",
                            "Connection: close\r\n\r\n"]),
                    httplib:send_file(TransportModule, Socket, FilePath);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

get_mime_type(Path) ->
    mime_types:lookup(tl(string:to_lower(filename:extension(Path)))).
