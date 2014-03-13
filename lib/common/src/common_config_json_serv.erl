-module(common_config_json_serv).

%%% external exports
-export([start_link/0]).
-export([stop/0, reload/0]).
-export_type([mode/0]).

%%% internal exports
-export([config_handler/1]).

%%% include files
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").
-include_lib("util/include/config_json.hrl").

%%% constants
-define(DEFAULT_CONFIG_FILENAME, "/etc/anond/anond.conf").
-define(DEFAULT_CONTROL_ADDRESS, {127, 0, 0, 1}).
-define(DEFAULT_CONTROL_PORT, 23313).
-define(JSON_SCHEMA,
        [{mode,
          #json_type{name = string, typical = normal,
                     convert = fun(<<"normal">>) -> normal;
                                  (<<"simulation">>) -> simulation
                               end}},
         {'directory-server',
          [{listen,
            #json_type{name = 'ipv4address:port',
                       typical = {{192,168,1,80}, 6700}}},
           {'json-rpc-cert-file',
            #json_type{name = string, typical = <<"ds-cert.pem">>}},
           {'peer-ttl',
            #json_type{name = {int, 1, 24}, info = <<"hours">>, typical = 3,
                       convert = fun(Value) -> Value*1000*60*60 end}},
           {'max-oas-per-peer',
            #json_type{name = {int, 1, 20}, typical = 10}}]},
         {nodes,
          [[{'directory-server',
             #json_type{name = 'ipv4address:port',
                        typical = {{192,168,1,80}, 6700}}},
            {'node-address',
             #json_type{name = 'ipv4address:port'}},
            {'json-rpc-cert-file',
             #json_type{name = string, typical = <<"node-cert.pem">>}},
            {'create-tun-device',
             #json_type{name = bool, typical = false}},
            {'overlay-addresses',
             [#json_type{name = ipv6address}]},
            {'public-key',
             #json_type{name = base64}},
            {'private-key',
             #json_type{name = base64}},
            {'number-of-peers',
             #json_type{name = {int, 2, 1000}, typical = 100}},
            {'refresh-peers-timeout',
             #json_type{name = {int, 5, 120}, info = <<"minutes">>,
                        typical = 60,
                        convert = fun(Value) -> Value*1000*60 end}},
            {'recalc-timeout',
             #json_type{name = {int, 5, 60*5}, info = <<"seconds">>,
                        typical = 15,
                        convert = fun(Value) -> Value*1000 end}},
            {'auto-recalc',
             #json_type{name = bool, typical = true}},
            {'max-cell-size',
             #json_type{name = {int, 512, 4096}, typical = 1024}},
            {'cell-sending-timeout',
             #json_type{name = {int, 50, 1000}, info = <<"milliseconds">>,
                        typical = 100}},
            {'path-cost',
             [{'number-of-echo-requests',
               #json_type{name = {int, 8, 128}, typical = 16}},
              {'acceptable-number-of-echo-replies',
               #json_type{name = {int, 8, 128}, typical = 8}},
              {'delay-between-echo-requests',
               #json_type{name = {int, 200, 2000}, info = <<"milliseconds">>,
                          typical = 1000}},
              {'delay-between-measurements',
               #json_type{name = {int, 1000, 10000}, info = <<"milliseconds">>,
                          typical = 5000}},
              {'echo-reply-timeout',
               #json_type{name = {int, 1, 60}, info = <<"seconds">>,
                          typical = 2,
                          convert = fun(Value) -> Value*1000 end}}]}]]},
         {'anond-control',
          [{listen,
            #json_type{name = 'ipv4address:port',
                       typical = {{127,0,0,1}, 23765}}}]},
         {logs,
          [{daemon,
            [{enabled,
              #json_type{name = bool, typical = true}},
             {filter,
              [{show,
                [#json_type{name = string, typical = <<"*">>}]},
               {hide,
                [#json_type{name = string, typical = <<"*">>}]}]},
             {tty,
              #json_type{name = bool, typical = false}},
             {file,
              [{enabled,
                #json_type{name = bool, typical = true}},
               {path,
                #json_type{name = filename,
                           typical = <<"/tmp/daemon.log">>}}]}]},
           {dbg,
            [{enabled,
              #json_type{name = bool, typical = true}},
             {filter,
              [{show,
                [#json_type{name = string, typical = <<"*">>}]},
               {hide,
                [#json_type{name = string, typical = <<"*">>}]}]},
             {tty,
              #json_type{name = bool, typical = false}},
             {file,
              [{enabled,
                #json_type{name = bool, typical = true}},
               {path,
                #json_type{name = filename, typical = <<"/tmp/dbg.log">>}}]}]},
           {error,
            [{enabled,
              #json_type{name = bool, typical = true}},
             {tty,
              #json_type{name = bool, typical = true}},
             {file,
              [{enabled,
                #json_type{name = bool, typical = true}},
               {path,
                #json_type{name = filename,
                           typical = <<"/tmp/error.log">>}}]}]}]}]).

%%% records

%%% types
-type mode() :: 'normal' | 'simulation'.

%%%
%%% exported: start_link
%%%

-spec start_link() -> {'ok', pid()} |
                      {'error', config_json_serv:error_reason()}.

start_link() ->
    ConfigFilename = config_filename(),
    case config_json_serv:start_link(ConfigFilename, ?JSON_SCHEMA,
                                     ['anond-control', listen],
                                     {?MODULE, config_handler, []}) of
        {ok, Pid} ->
            {ok, Pid};
        {error, Reason} ->
            stderr:print("~s: ~s", [ConfigFilename,
                                    config_json_serv:format_error(Reason)]),
            {error, Reason}
    end.

config_filename() ->
    case init:get_argument('-config') of
        {ok, [[ConfigFilename]]} ->
            ConfigFilename;
        error ->
            ?DEFAULT_CONFIG_FILENAME
    end.

config_handler(Socket) ->
    receive
        {tcp, Socket, <<"stop">>} ->
            init:stop(),
            ok;
        {tcp, Socket, <<"reload">>} ->
            config_json_serv ! reload,
            ok;
        {tcp_closed, Socket} ->
            ok;
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage})
    end.

%%%
%%% exported: stop
%%%

-spec stop() -> no_return().

stop() ->
    ControlAddress = control_address(),
    ControlPort = control_port(),
    case config_json_serv:tcp_send(ControlAddress, ControlPort, <<"stop">>) of
        ok ->
            erlang:halt(0);
        {error, Reason} ->
            stderr:print(1, true, config_json_serv:format_error(Reason), [])
    end.

%%%
%%% exported: reload
%%%

-spec reload() -> no_return().

reload() ->
    ControlAddress = control_address(),
    ControlPort = control_port(),
    case config_json_serv:tcp_send(ControlAddress, ControlPort, <<"reload">>) of
        ok ->
            erlang:halt(0);
        {error, Reason} ->
            stderr:print(1, true, config_json_serv:format_error(Reason), [])
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
