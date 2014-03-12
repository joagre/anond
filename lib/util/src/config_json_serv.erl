-module(config_json_serv).

%%% external exports
-export([start_link/4]).
-export([lookup/1, lookup/2]).
-export([subscribe/0, subscribe/1]).
-export([tcp_send/3]).
-export([format_error/1]).

%%% internal exports
-export([init/5]).

%%% include files
-include_lib("kernel/include/file.hrl").
-include_lib("util/include/config_json.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants
-define(DEFAULT_CONTROL_ADDRESS, {127, 0, 0, 1}).
-define(DEFAULT_CONTROL_PORT, 23313).
-define(MAX_SESSIONS, 8).

%%% records
-record(state, {
	  parent           :: pid(),
          net_serv         :: pid(),
          config_filename  :: file:filename(),
          json_schema      :: json_schema(),
	  json_term        :: json_term(),
          subscribers = [] :: [pid()]
         }).

%%% types
-type error_reason() ::
        'already_started' |
        {'config', config_error_reason()} |
        {'net_serv', net_serv:error_reason()} |
        {'posix', inet:posix()}.
-type config_error_reason() ::
        'bad_json' |
        {'file_error', file:filename(), file:posix()} |
        {'trailing', json_path()} |
        {'expected', json_path(), json_path()} |
        {'not_bool', json_value(), json_path()} |
        {'int_out_of_range', json_value(), integer(), integer(), json_path()} |
        {'not_int', json_value(), json_path()} |
        {'not_ipv4_address_port', json_value(), json_path()} |
        {'not_ipv6_address', json_value(), json_path()} |
        {'not_base64', json_value(), json_path()} |
        {'not_valid_directory', string(), json_path()} |
        {'file_error', file:filename(), file:posix(), json_path()} |
        {'not_string', json_value(), json_path()} |
        {'invalid_value', json_value(), json_path()}.

%%%
%%% exported: start_link
%%%

-spec start_link(ConfigFilename :: file:filename(),
                 JsonSchema :: json_schema(),
                 ControlAddressPortPath :: json_path(),
                 net_serv:handler()) ->
                        {'ok', pid()} |
                        {'error',
                         'already_started'|
                         {'config', config_error_reason()} |
                         {'net_serv', net_serv:error_reason()}}.

start_link(ConfigFilename, JsonSchema, ControlAddressPortPath, Handler) ->
    Args =
        [self(), ConfigFilename, JsonSchema, ControlAddressPortPath, Handler],
    Pid = proc_lib:spawn_link(?MODULE, init, Args),
    receive
	{Pid, started} ->
	    {ok, Pid};
	{Pid, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: lookup
%%%

-spec lookup(json_path()) -> json_term().

lookup(JsonPath) ->
    case serv:call(?MODULE, {lookup, JsonPath}) of
        not_found ->
            throw({unknown_config_parameter, JsonPath});
        JsonTermOrValue ->
            JsonTermOrValue
    end.

-spec lookup(json_path(), json_value()) -> json_term().

lookup(JsonPath, DefaultJsonValue) ->
    case serv:call(?MODULE, {lookup, JsonPath, DefaultJsonValue}) of
        not_found ->
            throw({unknown_config_parameter, JsonPath});
        JsonTermOrValue ->
            JsonTermOrValue
    end.

%%%
%%% exported: subscribe
%%%

-spec subscribe() -> 'ok'.

subscribe() ->
    ?MODULE ! {subscribe, self()},
    ok.

-spec subscribe(pid()) -> 'ok'.

subscribe(Pid) ->
    ?MODULE ! {subscribe, Pid},
    ok.

%%%
%%% exported: tcp_send
%%%

-spec tcp_send(inet:ip_address(), inet:port_number(), Message :: binary()) ->
                      'ok' | {'error', {'posix', inet:posix()}}.

tcp_send(Address, Port, Message) ->
    case gen_tcp:connect(Address, Port,
                         [{packet, 2}, {nodelay, true}, binary]) of
        {ok, Socket} ->
            gen_tcp:send(Socket, Message),
            gen_tcp:close(Socket);
        {error, Reason} ->
            {error, {posix, Reason}}
    end.

%%%
%%% exported: format_error
%%%

-spec format_error(error_reason()) -> iolist().

format_error(already_started) ->
    "Already started";
format_error({net_serv, Reason}) ->
    net_serv:format_error(Reason);
format_error({posix, Reason}) ->
    inet:format_error(Reason);
format_error({config, bad_json}) ->
    "Bad JSON";
format_error({config, {file_error, Filename, Reason}}) ->
    io_lib:format("~s: ~s", [Filename, file:format_error(Reason)]);
format_error({config, {trailing, JsonPath}}) ->
    io_lib:format("No configuration expected after ~s",
                  [json_path_to_string(JsonPath)]);
format_error({config, {expected, ExpectedJsonPath, JsonPath}}) ->
    io_lib:format("Expected ~s, got ~s",
                  [json_path_to_string(ExpectedJsonPath),
                   json_path_to_string(JsonPath)]);
format_error({config, {not_bool, Value, JsonPath}}) ->
    io_lib:format("~s: ~s is not a valid boolean value",
                  [json_path_to_string(JsonPath),
                   json_value_to_string(Value)]);
format_error({config, {int_out_of_range, Value, From, To, JsonPath}}) ->
    io_lib:format("~s: ~s must be in the range between ~w and ~w",
                  [json_path_to_string(JsonPath),
                   json_value_to_string(Value), From, To]);
format_error({config, {not_int, Value, JsonPath}}) ->
    io_lib:format("~s: ~s is not a valid integer",
                  [json_path_to_string(JsonPath),
                   json_value_to_string(Value)]);
format_error({config, {not_ipv4_address_port, Value, JsonPath}}) ->
    io_lib:format("~s: ~s is not a valid ipv4-address and port",
                  [json_path_to_string(JsonPath),
                   json_value_to_string(Value)]);
format_error({config, {not_ipv6_address, Value, JsonPath}}) ->
    io_lib:format("~s: ~s is not a valid ipv6-address",
                  [json_path_to_string(JsonPath),
                   json_value_to_string(Value)]);
format_error({config, {not_base64, Value, JsonPath}}) ->
    io_lib:format("~s: ~s is not a valid base64 value",
                  [json_path_to_string(JsonPath),
                   json_value_to_string(Value)]);
format_error({config, {not_valid_directory, Dirname, JsonPath}}) ->
    io_lib:format("~s: ~s is not an appropriate directory",
                  [json_path_to_string(JsonPath), Dirname]);
format_error({config, {file_error, Filename, Reason, JsonPath}}) ->
    io_lib:format("~s: ~s is not an appropriate file (~s)",
                  [json_path_to_string(JsonPath), Filename,
                   file:format_error(Reason)]);
format_error({config, {not_string, Value, JsonPath}}) ->
    io_lib:format("~s: ~s is not a valid string",
                  [json_path_to_string(JsonPath),
                   json_value_to_string(Value)]);
format_error({config, {invalid_value, Value, JsonPath}}) ->
    io_lib:format("~s: ~s is not a valid value",
                  [json_path_to_string(JsonPath),
                   json_value_to_string(Value)]);
format_error(UnknownReason) ->
    ?error_log(UnknownReason),
    "internal error".

json_path_to_string([Name|Rest]) ->
    json_path_to_string(Rest, [?a2l(Name)]).

json_path_to_string([], Acc) ->
    Acc;
json_path_to_string([Name|Rest], Acc) ->
    json_path_to_string(Rest, [?a2l(Name), $/|Acc]).

json_value_to_string(JsonValue) when is_integer(JsonValue) ->
    ?i2l(JsonValue);
json_value_to_string(true) ->
    "true";
json_value_to_string(false) ->
    "false";
json_value_to_string(JsonValue) when is_binary(JsonValue) ->
    ?b2l(JsonValue);
json_value_to_string({IpAddress, Port}) ->
    [net_tools:string_address(IpAddress), ":", ?i2l(Port)].

%%%
%%% server loop
%%%

init(Parent, ConfigFilename, JsonSchema, ControlListenPath, Handler) ->
    process_flag(trap_exit, true),
    case catch register(?MODULE, self()) of
        true ->
            case parse(ConfigFilename, JsonSchema) of
                {ok, JsonTerm} ->
                    {IpAddress, Port} =
                        json_lookup(JsonTerm, ControlListenPath),
                    SocketOptions = [{packet, 2}, {ip, IpAddress}, binary],
                    case net_serv:start_link(Port, [], SocketOptions,
                                             Handler) of
                        {ok, NetServ} ->
                            S = #state{parent = Parent,
                                       net_serv = NetServ,
                                       config_filename = ConfigFilename,
                                       json_schema = JsonSchema,
                                       json_term = JsonTerm},
                            Parent ! {self(), started},
                            loop(S);
                        {error, {not_started, Reason}} ->
                            Parent ! {self(), {net_serv, Reason}}
                    end;
                {error, Reason} ->
                    Parent ! {self(), {config, Reason}}
            end;
        _ ->
            Parent ! {self(), already_started}
    end.

loop(#state{parent = Parent,
            net_serv = NetServ,
            config_filename = ConfigFilename,
            json_schema = JsonSchema,
            json_term = JsonTerm,
            subscribers = Subscribers} = S) ->
    receive
        {From, {lookup, JsonPath}} ->
            From ! {self(), json_lookup(JsonTerm, JsonPath)},
            loop(S);
        {From, {lookup, JsonPath, DefaultJsonValue}} ->
            case json_lookup(JsonTerm, JsonPath) of
                not_found ->
                    From ! {self(), DefaultJsonValue},
                    loop(S);
                JsonTermOrValue ->
                    From ! {self(), JsonTermOrValue},
                    loop(S)
            end;
        {subscribe, ClientPid} ->
            case lists:member(ClientPid, Subscribers) of
                true ->
                    loop(S);
                false ->
                    erlang:monitor(process, ClientPid),
                    loop(S#state{subscribers = [ClientPid|Subscribers]})
            end;
        {'DOWN', _MonitorRef, process, ClientPid, _Info} ->
            UpdatedSubscribers = lists:delete(ClientPid, Subscribers),
            loop(S#state{subscribers = UpdatedSubscribers});
        reload ->
            case parse(ConfigFilename, JsonSchema) of
                {ok, JsonTerm} ->
                    ?daemon_log("~s: load succeeded", [ConfigFilename]),
                    lists:foreach(fun(ClientPid) ->
                                          ClientPid ! config_updated
                                  end, Subscribers),
                    loop(S#state{json_term = JsonTerm});
                {error, Reason} ->
                    ?daemon_log("~s: syntax error: ~s",
                                [ConfigFilename, format_error(Reason)]),
                    loop(S)
            end;
	{'EXIT', Parent, shutdown} ->
	    exit(shutdown);
	{'EXIT', NetServ, Reason} ->
	    exit(Reason);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

%%%
%%% lookup
%%%

json_lookup(JsonTerm, [Name]) when is_atom(Name), is_list(JsonTerm) ->
    case lists:keysearch(Name, 1, JsonTerm) of
        {value, {Name, NestedJsonTermOrValue}} ->
            NestedJsonTermOrValue;
        false ->
            not_found
    end;
%% Note: For now a key can only be the last item in the path. This
%% restriction will be lifted when needed.
json_lookup(JsonTerm, [{KeyName, Value}]) when is_list(JsonTerm) ->
    json_lookup_instance(JsonTerm, {KeyName, Value});
json_lookup(JsonTerm, [Name|Rest]) when is_atom(Name), is_list(JsonTerm) ->
    case lists:keysearch(Name, 1, JsonTerm) of
        {value, {Name, NestedJsonTerm}} ->
            json_lookup(NestedJsonTerm, Rest);
        false ->
            not_found
    end.

json_lookup_instance([], {_KeyName, _Value}) ->
    not_found;
json_lookup_instance([JsonTermInstance|Rest], {KeyName, Value}) ->
    case lists:member({KeyName, Value}, JsonTermInstance)of
        true ->
            JsonTermInstance;
        false ->
            json_lookup_instance(Rest, {KeyName, Value})
    end.

%%%
%%% parse config file
%%%

parse(ConfigFilename, JsonSchema) ->
    case file:read_file(ConfigFilename) of
        {ok, EncodedJson} ->
            case catch jsx:decode(EncodedJson) of
                JsonTerm when is_list(JsonTerm) ->
                    try
                        {ok, validate(JsonSchema, atomify(JsonTerm), [])}
                    catch
                        throw:Reason ->
                            {error, Reason}
                    end;
                _ ->
                    {error, bad_json}
            end;
        {error, Reason} ->
            {error, {file_error, ConfigFilename, Reason}}
    end.

atomify([]) ->
    [];
atomify([{Binary, JsonTerm}|Rest]) when is_list(JsonTerm), is_list(Rest) ->
    [{?b2a(Binary), atomify(JsonTerm)}|atomify(Rest)];
atomify([{Binary, JsonValue}|Rest]) when is_list(Rest) ->
    [{?b2a(Binary), JsonValue}|atomify(Rest)];
atomify([JsonTerm|Rest]) when is_list(JsonTerm), is_list(Rest) ->
    [atomify(JsonTerm)|atomify(Rest)];
atomify([JsonValue|Rest]) when is_list(Rest) ->
    [JsonValue|atomify(Rest)].

validate([], [], _JsonPath) ->
    [];
validate([], _JsonTerm, JsonPath) ->
    throw({trailing, JsonPath});
validate(_JsonSchema, [], _JsonPath) ->
    [];
%% single value
validate([{Name, JsonType}|JsonSchemaRest],
         [{Name, JsonValue}|JsonTermRest], JsonPath)
  when is_record(JsonType, json_type) ->
    ValidatedValue = validate_value(JsonType, JsonValue, [Name|JsonPath]),
    [{Name, ValidatedValue}|validate(JsonSchemaRest, JsonTermRest, JsonPath)];
validate([{Name, JsonType}|_JsonSchemaRest],
         [{AnotherName, _JsonValue}|_JsonTermRest], JsonPath)
  when is_record(JsonType, json_type) ->
    throw({expected, [Name|JsonPath], [AnotherName|JsonPath]});
%% array of single values
validate([{Name, [JsonType]}|JsonSchemaRest],
         [{Name, JsonValues}|JsonTermRest], JsonPath)
  when is_record(JsonType, json_type) ->
    ValidatedValues = validate_values(JsonType, JsonValues, [Name|JsonPath]),
    [{Name, ValidatedValues}|validate(JsonSchemaRest, JsonTermRest, JsonPath)];
validate([{Name, [JsonType]}|_JsonSchemaRest],
         [{AnotherName, _JsonValue}|_JsonTermRest], JsonPath)
  when is_record(JsonType, json_type) ->
    throw({expected, [Name|JsonPath], [AnotherName|JsonPath]});
%% object
validate([{Name, NestedJsonSchema}|JsonSchemaRest],
         [{Name, NestedJsonTerm}|JsonTermRest], JsonPath) ->
    [{Name, validate(NestedJsonSchema, NestedJsonTerm, [Name|JsonPath])}|
     validate(JsonSchemaRest, JsonTermRest, JsonPath)];
validate([{Name, _NestedJsonSchema}|_JsonSchemaRest],
         [{AnotherName, _NestedJsonTerm}|_JsonTermRest], JsonPath) ->
    throw({expected, [Name|JsonPath],[AnotherName|JsonPath]});
%% array of objects
validate([JsonSchema|JsonSchemaRest],
         [JsonTerm|JsonTermRest], JsonPath)
  when is_list(JsonSchema), is_list(JsonTerm) ->
    [validate(JsonSchema, JsonTerm, JsonPath)|
     validate([JsonSchema|JsonSchemaRest], JsonTermRest, JsonPath)].

%% bool
validate_value(#json_type{name = bool, convert = Convert}, Value, JsonPath)
  when is_boolean(Value) ->
    convert_value(Convert, Value, JsonPath);
validate_value(#json_type{name = bool}, Value, JsonPath) ->
    throw({not_bool, Value, JsonPath});
%% int
validate_value(#json_type{name = {int, From, unbounded}, convert = Convert},
               Value, JsonPath)
  when is_integer(Value) andalso Value >= From andalso Value >= From ->
    convert_value(Convert, Value, JsonPath);
validate_value(#json_type{name = {int, From, To}, convert = Convert}, Value,
               JsonPath)
  when is_integer(Value) andalso Value >= From andalso Value =< To ->
    convert_value(Convert, Value, JsonPath);
validate_value(#json_type{name = {int, From, To}}, Value, JsonPath)
  when is_integer(Value) ->
    throw({int_out_of_range, Value, From, To, JsonPath});
validate_value(#json_type{name = {int, _From, _To}}, Value, JsonPath) ->
    throw({not_int, Value, JsonPath});
%% ipv4address:port
validate_value(#json_type{name = 'ipv4address:port', convert = Convert}, Value,
               JsonPath)
  when is_binary(Value) ->
    case string:tokens(?b2l(Value), ":") of
        [Ipv4AddressString, PortString] ->
            case inet:parse_ipv4_address(Ipv4AddressString) of
                {ok, Ipv4Address} ->
                    case catch ?l2i(PortString) of
                        Port when is_integer(Port) ->
                            convert_value(Convert, {Ipv4Address, Port},
                                          JsonPath);
                        _ ->
                            throw({not_ipv4_address_port, Value, JsonPath})
                    end;
                {error, einval} ->
                    throw({not_ipv4_address_port, Value, JsonPath})
            end;
        _ ->
            throw({not_ipv4_address_port, Value, JsonPath})
    end;
validate_value(#json_type{name = 'ipv4address:port'}, Value, JsonPath) ->
    throw({not_ipv4_address_port, Value, JsonPath});
%% ipv6address
validate_value(#json_type{name = ipv6address, convert = Convert}, Value,
               JsonPath)
  when is_binary(Value) ->
    case inet:parse_ipv6_address(?b2l(Value)) of
        {ok, Ipv6Address} ->
            convert_value(Convert, Ipv6Address, JsonPath);
        {error, einval} ->
            throw({not_ipv6_address, Value, JsonPath})
    end;
validate_value(#json_type{name = ipv6address}, Value, JsonPath) ->
    throw({not_ipv6_address, Value, JsonPath});
%% base64
validate_value(#json_type{name = base64, convert = Convert}, Value, JsonPath)
  when is_binary(Value) ->
    convert_value(Convert, Value, JsonPath);
validate_value(#json_type{name = base64}, Value, JsonPath) ->
    throw({not_base64, Value, JsonPath});
%% filename
validate_value(#json_type{name = filename, convert = Convert}, Value, JsonPath)
  when is_binary(Value) ->
    Dirname = filename:dirname(?b2l(Value)),
    case file:read_file_info(Dirname) of
        {ok, #file_info{type = directory, access = read}} ->
            convert_value(Convert, Value, JsonPath);
        {ok, #file_info{type = directory, access = read_write}} ->
            convert_value(Convert, Value, JsonPath);
        {ok, _FileInfo} ->
            throw({not_valid_directory, Dirname, JsonPath});
        {error, Reason} ->
            throw({file_error, Value, Reason, JsonPath})
    end;
validate_value(#json_type{name = filename}, Value, JsonPath) ->
    throw({file_error, Value, enotdir, JsonPath});
%% string
validate_value(#json_type{name = string, convert = Convert}, Value, JsonPath)
  when is_binary(Value) ->
    convert_value(Convert, Value, JsonPath);
validate_value(#json_type{name = string}, Value, JsonPath) ->
    throw({not_string, Value, JsonPath}).

convert_value(undefined, Value, _JsonPath) ->
    Value;
convert_value(Convert, Value, JsonPath) ->
    case catch Convert(Value) of
        {'EXIT', _Reason} ->
            throw({invalid_value, Value, JsonPath});
        ConvertedValue ->
            ConvertedValue
    end.

validate_values(_JsonType, [], _JsonPath) ->
    [];
validate_values(JsonType, [JsonValue|Rest], JsonPath) ->
    [validate_value(JsonType, JsonValue, JsonPath)|
     validate_values(JsonType, Rest, JsonPath)].
