-module(httplib).

%%% external exports
-export([get_headers/1, get_headers/2]).
-export([fmt_headers/1, fmt_headers/2]).
-export([lookup_header_value/2, lookup_header_value/3]).
-export([download/3]).
-export([inflate_file/2]).
-export([send_file/2]).
-export([post/7]).

%%% internal exports

%%% include files
-include_lib("kernel/include/file.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants
-define(DOWNLOAD_FILE_CHUNK_SIZE, 4096).
-define(READ_FILE_CHUNK_SIZE, 8192).
-define(FILE_READ_BUFFER, 8192).

%%% records

%%% types
-type header_name() :: atom() | binary().
-type header_value() :: any().
-type header_values() :: [{header_name(), header_value()}].
-type post_error_reason() :: inet:posix() | 'invalid_content_length'.

%%%
%%% exported: get_headers
%%%

-spec get_headers(inet:socket(), header_values()) ->
                         {'ok', header_values()}.

get_headers(Socket) ->
    get_headers(Socket, ['_'], []).

get_headers(Socket, HeaderValues) ->
    get_headers(Socket, HeaderValues, []).

get_headers(Socket, HeaderValues, CollectedHeaderValues) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, {http_header, _, Name, _Reserved, Value}} ->
            LowerCaseName = lower_case(Name),
            case lists:keymember(LowerCaseName, 1, HeaderValues) of
                true ->
                    get_headers(
                      Socket,
                      lists:keydelete(LowerCaseName, 1, HeaderValues),
                      [{LowerCaseName, Value}|CollectedHeaderValues]);
                false ->
                    case lists:member('_', HeaderValues) of
                        true ->
                            get_headers(
                              Socket, HeaderValues,
                              [{LowerCaseName, Value}|CollectedHeaderValues]);
                        false ->
                            get_headers(Socket, HeaderValues,
                                        CollectedHeaderValues)
                    end
            end;
        {ok, http_eoh} ->
            {ok, lists:delete('_', CollectedHeaderValues++HeaderValues)};
        {ok, {http_error, Bytes}} ->
            {error, {http_error, Bytes}};
        {error, Reason} ->
            {error, Reason}
    end.

lower_case(Name) when is_atom(Name) ->
    ?l2a(string:to_lower(?a2l(Name)));
lower_case(Name) when is_binary(Name) ->
    ?l2b(string:to_lower(?b2l(Name)));
lower_case(Name) when is_list(Name) ->
    string:to_lower(Name).

%%%
%%% exported: fmt_headers
%%%

-spec fmt_headers(header_values(), SkipHeaderNames :: [header_name()]) ->
                         iolist().

fmt_headers(HeaderValues) ->
    fmt_headers(HeaderValues, []).

fmt_headers([], _SkipHeaderNames) ->
    [];
fmt_headers([{Name, Value}|Rest], SkipHeaderNames) ->
    case lists:member(Name, SkipHeaderNames) of
        true -> 
            fmt_headers(Rest, SkipHeaderNames);
        false when is_atom(Name) ->
            [?a2l(Name), ": ", Value, "\r\n"|
             fmt_headers(Rest, SkipHeaderNames)];
        false ->
            [Name, ": ", Value, "\r\n"|fmt_headers(Rest, SkipHeaderNames)]
    end.

%%%
%%% exported: lookup_header_value
%%%

-spec lookup_header_value(header_name(), header_values(), header_value()) ->
                                 header_value().

lookup_header_value(Name, Headers) ->
    lookup_header_value(Name, Headers, '$no_default_value').

lookup_header_value(Name, Headers, DefaultValue) ->
    case lists:keysearch(Name, 1, Headers) of
        false when DefaultValue /= '$no_default_value' -> DefaultValue;
        {value, {Name, Value}} -> Value
    end.

%%%
%%% exported: download
%%%

-spec download(inet:socket(), ContentLength :: integer(), file:filename()) ->
                      'ok' | {'error', file:posix() | 'terminated' |
                              'system_limit'}. 

download(Socket, ContentLength, Filename) ->
    case file:open(Filename, [write, binary]) of
        {ok, File} ->
            Result = download_content(Socket, ContentLength, File),
            file:close(File),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.

%% HTTP/1.0
download_content(Socket, -1, File) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            case file:write(File, Data) of
                ok ->
                    download_content(Socket, -1, File);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, closed} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end;
%% HTTP/1.1 with Content-Length
download_content(_Socket, 0, _File) ->
    ok;
download_content(Socket, ContentLength, File)
  when ContentLength < ?DOWNLOAD_FILE_CHUNK_SIZE ->
    case gen_tcp:recv(Socket, ContentLength) of
        {ok, Data} ->
            file:write(File, Data);
        {error, Reason} ->
            {error, Reason}
    end;
download_content(Socket, ContentLength, File) ->
    case gen_tcp:recv(Socket, ?DOWNLOAD_FILE_CHUNK_SIZE) of
        {ok, Data} ->
            case file:write(File, Data) of
                ok ->
                    download_content(Socket, ContentLength-
                                     ?DOWNLOAD_FILE_CHUNK_SIZE, File);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: inflate_file
%%%

-spec inflate_file(file:filename(), DecompressedFilename :: file:filename()) ->
                          'ok'.

inflate_file(Filename, DecompressedFilename) ->
    {ok, File} = file:open(Filename, [read, binary]),
    {ok, DecompressedFile} = file:open(DecompressedFilename, [write, binary]),
    Z = zlib:open(),
    ok = zlib:inflateInit(Z),
    ReadFile =
        fun() ->
                case file:read(File, ?READ_FILE_CHUNK_SIZE) of
                    {ok, Data} ->
                        Data;
                    eof ->
                        end_of_data
                end
        end,
    Decompress = 
        fun(end_of_data, _Cont) ->
                [];
           (Data, Cont) ->
                InflatedData = zlib:inflate(Z, Data),
                ok = file:write(DecompressedFile, InflatedData),
                Cont(ReadFile(), Cont)                        
        end,
    Decompress(ReadFile(), Decompress),
    ok = zlib:inflateEnd(Z),
    zlib:close(Z),
    file:close(File),
    file:close(DecompressedFile),
    ok.

%%%
%%% exported: send_file
%%%

-spec send_file(inet:socket(), file:filename()) ->
                       'ok' | {'error', inet:posix()}.

send_file(Socket, FilePath) ->
    case file:open(FilePath, [read, binary]) of
        {error, Reason} ->
            {error, Reason};
        {ok, IoDevice} ->
            case send_file(Socket, IoDevice, 0) of
                {error, Reason} ->
                    file:close(IoDevice),
                    {error, Reason};
                ok ->
                    file:close(IoDevice),
                    ok
            end
    end.

send_file(Socket, IoDevice, Position) ->
    case file:pread(IoDevice, Position, ?FILE_READ_BUFFER) of
        {ok, Data} ->
            gen_tcp:send(Socket, Data),
            send_file(Socket, IoDevice, Position+?FILE_READ_BUFFER);
        eof ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: post
%%%

-spec post(inet:ip_address(), inet:ip_address(), inet:port_number(), timeout(),
           binary(), binary(), binary()) ->
                  'ok' | {'error', post_error_reason()}.

post(NicIpAddress, IpAddress, Port, Timeout, Uri, ContentType, Payload) ->
    HttpRequest =
        [<<"POST ">>, Uri, <<" HTTP/1.1\r\n">>,
         <<"Content-Type: ">>, ContentType, <<"\r\n">>,
         <<"Content-Length: ">>, ?i2l(size(Payload)), <<"\r\n">>,
         <<"Connection: close\r\n\r\n">>,
         Payload],
    Options = [{packet, http_bin}, {active, false}, {ip, NicIpAddress}],
    case gen_tcp:connect(IpAddress, Port, Options, Timeout) of
        {ok, Socket} ->
            send_and_recv(Timeout, HttpRequest, Socket);
        {error, Reason} ->
            {error, Reason}
    end.

send_and_recv(Timeout, HttpRequest, Socket) ->
    case gen_tcp:send(Socket, HttpRequest) of
        ok ->
            recv(Timeout, Socket);
        {error, Reason} ->
            {error, Reason}
    end.

recv(Timeout, Socket) ->
    case gen_tcp:recv(Socket, 0, Timeout) of
        {ok, {http_response, {1, 1}, 200, <<"OK">>}} ->
            ok = inet:setopts(Socket, [{packet, httph_bin}]),
            {ok, HeaderValues} =
                httplib:get_headers(Socket, [{'content-length', -1}]),
            ok = inet:setopts(Socket, [binary, {packet, 0}]),
            BinaryContentLength =
                httplib:lookup_header_value('content-length', HeaderValues),
            case catch ?b2i(BinaryContentLength) of
                ContentLength when is_integer(ContentLength) ->
                    Result = gen_tcp:recv(Socket, ContentLength, Timeout),
                    gen_tcp:close(Socket),
                    Result;
                _ ->
                    gen_tcp:close(Socket),
                    {error, invalid_content_length}
            end;
        {error, Reason} ->
            gen_tcp:close(Socket),
            {error, Reason}
    end.
