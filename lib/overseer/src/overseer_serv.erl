-module(overseer_serv).

%%% external exports
-export([start_link/0, stop/0, stop/1]).
-export([get_nodes/0, get_nodes/1]).
-export([get_route_entries/0, get_route_entries/1]).
-export([enable_recalc/0, enable_recalc/1, disable_recalc/0, disable_recalc/1]).
-export([recalc/0, recalc/1]).

%%% internal exports
-export([init/1]).

%%% include files
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").
-include_lib("util/include/config.hrl").
-include_lib("node/include/node.hrl").
-include_lib("node/include/node_route.hrl").
-include_lib("ds/include/ds.hrl").

%%% constants

%%% records
-record(state, {
	  parent           :: pid(),
          directory_server :: {inet:ip4_address(), inet:port_number()}
	 }).

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link() -> {'ok', pid()} | {'error', 'already_started'}.

start_link() ->
    Args = [self()],
    Pid = proc_lib:spawn_link(?MODULE, init, Args),
    receive
	{Pid, started} ->
	    {ok, Pid};
	{Pid, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: stop
%%%

-spec stop() -> 'ok'.

stop() ->
    stop(15000).

-spec stop(timeout()) -> 'ok'.

stop(Timeout) ->
    serv:call(?MODULE, stop, Timeout).

%%%
%%% exported: get_nodes
%%%

-spec get_nodes() -> {'ok',
                      [{na(),
                        [#node{}] |
                        {'not_available', jsonrpc:error_reason()}}]} |
                     {'error', jsonrpc:error_reason()}.

get_nodes() ->
    serv:call(?MODULE, get_nodes).

-spec get_nodes(na()) -> {'ok',
                      [{na(),
                        [#node{}] |
                        {'not_available', jsonrpc:error_reason()}}]} |
                     {'error', jsonrpc:error_reason()}.

get_nodes(Na) ->
    serv:call(?MODULE, {get_nodes, Na}).

%%%
%%% exported: get_route_entries
%%%

-spec get_route_entries() -> {'ok',
                              [{na(),
                                [#route_entry{} |
                                 {'not_available', jsonrpc:error_reason()}]}]} |
                             {'error', jsonrpc:error_reason()}.

get_route_entries() ->
    serv:call(?MODULE, get_route_entries).

-spec get_route_entries(na()) -> {'ok', [{na(),
                                          [#route_entry{} |
                                           {'not_available',
                                            jsonrpc:error_reason()}]}]} |
                                 {'error', jsonrpc:error_reason()}.

get_route_entries(Na) ->
    serv:call(?MODULE, {get_route_entries, Na}).

%%%
%%% exported: enable_recalc
%%%

-spec enable_recalc() -> 'ok'.

enable_recalc() ->
    serv:call(?MODULE, enable_recalc).

-spec enable_recalc(na()) -> 'ok' | {'error', jsonrpc:error_reason()}.

enable_recalc(Na) ->
    serv:call(?MODULE, {enable_recalc, Na}).

%%%
%%% exported: disable_recalc
%%%

-spec disable_recalc() -> 'ok'.

disable_recalc() ->
    serv:call(?MODULE, disable_recalc).

-spec disable_recalc(na()) -> 'ok' | {'error', jsonrpc:error_reason()}.

disable_recalc(Na) ->
    serv:call(?MODULE, {disable_recalc, Na}).

%%%
%%% exported: recalc
%%%

-spec recalc() -> 'ok'.

recalc() ->
    serv:call(?MODULE, recalc).

-spec recalc(na()) -> 'ok' | {'error', jsonrpc:error_reason()}.

recalc(Na) ->
    serv:call(?MODULE, {recalc, Na}).

%%%
%%% server loop
%%%

init(Parent) ->
    process_flag(trap_exit, true),
    case catch register(?MODULE, self()) of
        true ->
            S = read_config(#state{}),
            ok = config_json_serv:subscribe(),
	    Parent ! {self(), started},
	    loop(S#state{parent = Parent});
        _ ->
            Parent ! {self(), already_started}
    end.

loop(#state{parent = Parent, directory_server = DsIpAddressPort} = S) ->
    receive
        config_updated ->
            loop(read_config(S));
	{From, stop} ->
	    From ! {self(), ok};
    	{From, get_nodes} ->
            case ds_jsonrpc:get_all_peers(undefined, DsIpAddressPort) of
                {ok, Peers} ->
                    AllNodes =
                        lists:map(
                          fun(#peer{na = Na}) ->
                                  case node_route_jsonrpc:get_nodes(undefined,
                                                                    Na) of
                                      {ok, Nodes} ->
                                          {Na, Nodes};
                                      {error, Reason} ->
                                          {Na, {not_available, Reason}}
                                  end
                          end, Peers),
                    From ! {self(), {ok, AllNodes}},
                    loop(S);
                {error, Reason} ->
                    From ! {self(), {error, Reason}},
                    loop(S)
            end;
	{From, {get_nodes, Na}} ->
            case node_route_jsonrpc:get_nodes(undefined, Na) of
                {ok, Nodes} ->
                    From ! {self(), {ok, Nodes}},
                    loop(S);
                {error, Reason} ->
                    From ! {self(), {error, Reason}},
                    loop(S)
            end;
	{From, get_route_entries} ->
            case ds_jsonrpc:get_all_peers(undefined, DsIpAddressPort) of
                {ok, Peers} ->
                    AllRouteTables =
                        lists:map(
                          fun(#peer{na = Na}) ->
                                  case node_route_jsonrpc:get_route_entries(
                                         undefined, Na) of
                                      {ok, Res} ->
                                          {Na, Res};
                                      {error, Reason} ->
                                          {Na, {not_available, Reason}}
                                  end
                          end, Peers),
                    From ! {self(), {ok, AllRouteTables}},
                    loop(S);
                {error, Reason} ->
                    From ! {self(), {error, Reason}},
                    loop(S)
            end;
	{From, {get_route_entries, Na}} ->
            case node_route_jsonrpc:get_route_entries(undefined, Na) of
                {ok, Res} ->
                    From ! {self(), {ok, Res}},
                    loop(S);
                {error, Reason} ->
                    From ! {self(), {error, Reason}},
                    loop(S)
            end;
	{From, enable_recalc} ->
            case ds_jsonrpc:get_all_peers(undefined, DsIpAddressPort) of
                {ok, Peers} ->
                    lists:foreach(
                      fun(#peer{na = Na}) ->
                              node_route_jsonrpc:enable_recalc(undefined, Na)
                      end, Peers),
                    From ! {self(), ok},
                    loop(S);
                {error, Reason} ->
                    From ! {self(), {error, Reason}},
                    loop(S)
            end;
        {From, {enable_recalc, Na}} ->
            From ! {self(), node_route_jsonrpc:enable_recalc(undefined, Na)},
            loop(S);
	{From, disable_recalc} ->
            case ds_jsonrpc:get_all_peers(undefined, DsIpAddressPort) of
                {ok, Peers} ->
                    lists:foreach(
                      fun(#peer{na = Na}) ->
                              node_route_jsonrpc:disable_recalc(undefined, Na)
                      end, Peers),
                    From ! {self(), ok},
                    loop(S);
                {error, Reason} ->
                    From ! {self(), {error, Reason}},
                    loop(S)
            end;
        {From, {disable_recalc, Na}} ->
            From ! {self(), node_route_jsonrpc:disable_recalc(undefined, Na)},
            loop(S);
	{From, recalc} ->
            case ds_jsonrpc:get_all_peers(undefined, DsIpAddressPort) of
                {ok, Peers} ->
                    lists:foreach(
                      fun(#peer{na = Na}) ->
                              node_route_jsonrpc:recalc(undefined, Na)
                      end, Peers),
                    From ! {self(), ok},
                    loop(S);
                {error, Reason} ->
                    From ! {self(), {error, Reason}},
                    loop(S)
            end;
        {From, {recalc, Na}} ->
            From ! {self(), node_route_jsonrpc:recalc(undefined, Na)},
            loop(S);
        {'EXIT', Parent, Reason} ->
            exit(Reason);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

%%%
%%% init
%%%

read_config(S) ->
    DsIpAddressPort = ?config(['directory-server', listen]),
    S#state{directory_server = DsIpAddressPort}.
