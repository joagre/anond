-module(common_config_serv).

%%% external exports
-export([start_link/0]).
-export([stop/0, reload/0]).

%%% internal exports
-export([convert_callback/2]).
-export([session_handler/1]).

%%% include files
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants
-define(DEFAULT_CONFIG_FILENAME, "/etc/anond/anond.conf").
-define(DEFAULT_CONTROL_ADDRESS, {127, 0, 0, 1}).
-define(DEFAULT_CONTROL_PORT, 23765).

%%% records

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link() -> {'ok', pid()} |
                      {'error',
                       {'not_started', config_serv:error_reason()} |
                       'already_started'}.

start_link() ->
    {ok, Cwd} = file:get_cwd(),
    SchemaFilename =
        filename:join([Cwd, code:priv_dir(common), "anond.xsd"]),
    ConfigFilename = config_filename(),
    SessionHandler = {?MODULE, session_handler, []},
    case config_serv:start_link(SchemaFilename, ConfigFilename,
                                fun convert_callback/2,
                                [config],
                                ['anond-control', listen, address],
                                ['anond-control', listen, port],
                                SessionHandler) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {not_started, Reason}} ->
            stderr:print("~s: ~s", [ConfigFilename,
                                    config_serv:format_error(Reason)]),
            {error, {not_started, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

config_filename() ->
    case init:get_argument('-config') of
        {ok, [[ConfigFilename]]} ->
            ConfigFilename;
        error ->
            ?DEFAULT_CONFIG_FILENAME
    end.

session_handler(Socket) ->
    receive
        {tcp, Socket, <<"stop">>} ->
            application:stop(proxy),
            ok;
        {tcp, Socket, <<"reload">>} ->
            config_serv ! reload,
            ok;
        {tcp_closed, Socket} ->
            ok;
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage})
    end.

convert_callback(LeafPath, Value) ->
    LabelPath = lists:filter(fun(Name) -> is_atom(Name) end, LeafPath),
    convert_value(lists:reverse(LabelPath), Value).

convert_value(['simulation'|_], Value) ->
    {ok, ?l2a(Value)};
%% node
convert_value(['number-of-peers'|_], Value) ->
    {ok, ?l2i(Value)};
convert_value(['measure-link-quality-timeout'|_], Value) ->
    {ok, ?l2i(Value)*1000};
convert_value(['refresh-peers-timeout'|_], Value) ->
    {ok, ?l2i(Value)*1000*60};
convert_value(['recalc-timeout'|_], Value) ->
    {ok, ?l2i(Value)*1000};
%% directory-server
convert_value(['peer-ttl'|_], Value) ->
    {ok, ?l2i(Value)*1000*60*60};
convert_value(['max-oas-per-peer'|_], Value) ->
    {ok, ?l2i(Value)};
%% generic
convert_value([address, listen|_], Value) ->
    case inet_parse:address(Value) of
        {ok, Address} ->
            {ok, Address};
        {error, Reason} ->
            {error, {posix, Reason}}
    end;
convert_value([port, listen|_], Value) ->
    {ok, ?l2i(Value)};
convert_value([enabled|_], Value) ->
    {ok, ?l2a(Value)};
convert_value(_LeafPath, Value) ->
    {ok, ?l2b(Value)}.

%%%
%%% exported: stop
%%%

-spec stop() -> no_return().

stop() ->
    ControlAddress = control_address(),
    ControlPort = control_port(),
    case config_serv:tcp_send(ControlAddress, ControlPort, <<"stop">>) of
        ok ->
            erlang:halt(0);
        {error, Reason} ->
            stderr:print(1, true, config_serv:format_error(Reason), [])
    end.

%%%
%%% exported: reload
%%%

-spec reload() -> no_return().

reload() ->
    ControlAddress = control_address(),
    ControlPort = control_port(),
    case config_serv:tcp_send(ControlAddress, ControlPort, <<"reload">>) of
        ok ->
            erlang:halt(0);
        {error, Reason} ->
            stderr:print(1, true, config_serv:format_error(Reason), [])
    end.

control_address() ->
    case init:get_argument('-control-address') of
        error ->
            ?DEFAULT_CONTROL_ADDRESS;
        {ok, [[ControlAddressString]]}->
            case inet_parse:address(ControlAddressString) of
                {ok, ControlAddress} ->
                    ControlAddress;
                {error, _Reason} ->
                    stderr:print(1, true, "invalid control address: ~s",
                                 [ControlAddressString])
            end
    end.

control_port() ->
    case init:get_argument('-control-port') of
        error ->
            ?DEFAULT_CONTROL_PORT;
        {ok, [[ControlPortString]]} ->
            case catch ?l2i(ControlPortString) of
                {'EXIT', _} ->
                    stderr:print(1, true, "invalid control port number: ~s",
                                 [ControlPortString]);
                ControlPort ->
                    ControlPort
            end
    end.
