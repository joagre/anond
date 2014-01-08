-module(overseer_serv).

%%% external exports
-export([start_link/0, stop/0, stop/1]).
-export([refresh/0]).
-export([get_global_route_table/0, get_route_table/1]).
-export([get_neighbours/0, get_neighbours/1]).
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
-define(REFRESH_TIMEOUT, 3*1000).

%%% records
-record(state, {
	  parent           :: pid(),
	  nodes = []       :: [{oa(), na()}],
          directory_server :: {inet:ip4_address(), inet:port_number()}
	 }).

%%% types
-type global_route_table() ::
        [{oa(), {oa(), path_cost(),
                 [na()] | {'not_available', jsonrpc:error_reason()}}}].
-type route_table() :: [{oa(), path_cost(), [na()]}].
-type neighbours() :: [{na(), path_cost()}].

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
%%% exported: refresh
%%%

-spec refresh() -> 'ok'.

refresh() ->
    ?MODULE ! refresh,
    ok.

%%%
%%% exported: get_global_route_table
%%%

-spec get_global_route_table() -> global_route_table().

get_global_route_table() ->
    serv:call(?MODULE, get_global_route_table).

%%%
%%% exported: get_route_table
%%%

-spec get_route_table(na()) -> {'ok', route_table()} |
                               {'error', jsonrpc:error_reason()}.

get_route_table(Na) ->
    serv:call(?MODULE, {get_route_table, Na}).

%%%
%%% exported: get_neighbours
%%%

-spec get_neighbours() -> {'ok', [{na(), neighbours() |
                                   {'not_available', jsonrpc:error_reason()}}]}.

get_neighbours() ->
    serv:call(?MODULE, get_neighbours).

-spec get_neighbours(na()) -> {'ok', neighbours()} |
                              {'error', jsonrpc:error_reason()}.

get_neighbours(Na) ->
    serv:call(?MODULE, {get_neighbours, Na}).

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
            timelib:start_timer(?REFRESH_TIMEOUT, refresh),
	    Parent ! {self(), started},
	    loop(S#state{parent = Parent});
        _ ->
            Parent ! {self(), already_started}
    end.

loop(#state{parent = Parent,
            nodes = Nodes,
            directory_server = DsIpAddressPort} = S) ->
    receive
        config_updated ->
            loop(read_config(S));
	{From, stop} ->
	    From ! {self(), ok};
        refresh ->
            case get_all_published_nodes(DsIpAddressPort) of
                {ok, UpdatedNodes} ->
                    loop(S#state{nodes = UpdatedNodes});
                {error, Reason} ->
                    ?error_log(Reason),
                    loop(S)
            end;
	{From, get_global_route_table} ->
	    From ! {self(), get_global_route_table(Nodes)},
	    loop(S);
	{From, {get_route_table, Na}} ->
            case node_route_jsonrpc:get_route_entries(undefined, Na) of
                {ok, Res} ->
                    RouteTable =
                        [{ReOa, Pc, Hops} ||
                            #route_entry{oa = ReOa,
                                         path_cost = Pc,
                                         hops = Hops} <- Res],
                    From ! {self(), {ok, RouteTable}},
                    loop(S);
                {error, Reason} ->
                    From ! {self(), {error, Reason}},
                    loop(S)
            end;
    	{From, get_neighbours} ->
            AllNeighbours =
                lists:map(
                  fun({_Oa, Na}) ->
                          case node_route_jsonrpc:get_nodes(undefined, Na) of
                              {ok, ActualNodes} ->
                                  {Na, [{ActualNode#node.na,
                                         ActualNode#node.path_cost} ||
                                           ActualNode <- ActualNodes]};
                              {error, Reason} ->
                                  {Na, {not_available, Reason}}
                          end
                  end, Nodes),
            From ! {self(), {ok, AllNeighbours}},
            loop(S);
	{From, {get_neighbours, Na}} ->
            case node_route_jsonrpc:get_nodes(undefined, Na) of
                {ok, ActualNodes} ->
                    Neighbours =
                        [{ActualNode#node.na, ActualNode#node.path_cost} ||
                            ActualNode <- ActualNodes],
                    From ! {self(), {ok, Neighbours}},
                    loop(S);
                {error, Reason} ->
                    From ! {self(), {error, Reason}},
                    loop(S)
            end;
	{From, enable_recalc} ->
            lists:foreach(
              fun({_Oa, Na}) ->
                      _ =  node_route_jsonrpc:enable_recalc(undefined, Na)
              end, Nodes),
            From ! {self(), ok},
            loop(S);
        {From, {enable_recalc, Na}} ->
            From ! {self(), node_route_jsonrpc:enable_recalc(undefined, Na)},
            loop(S);
	{From, disable_recalc} ->
            lists:foreach(
              fun({_Oa, Na}) ->
                      _ = node_route_jsonrpc:disable_recalc(undefined, Na)
              end, Nodes),
            From ! {self(), ok},
            loop(S);
        {From, {disable_recalc, Na}} ->
            From ! {self(), node_route_jsonrpc:disable_recalc(undefined, Na)},
            loop(S);
	{From, recalc} ->
            lists:foreach(fun({_Oa, Na}) ->
                                  _ = node_route_jsonrpc:recalc(undefined, Na)
                          end, Nodes),
            From ! {self(), ok},
            loop(S);
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

get_all_published_nodes(DsIpAddressPort) ->
    case ds_jsonrpc:get_all_peers(undefined, DsIpAddressPort) of
        {ok, Peers} ->
            get_all_published_nodes(DsIpAddressPort, Peers);
        {error, Reason} ->
            {error, Reason}
    end.

get_all_published_nodes(_DsIpAddressPort, Peers) ->
    get_all_published_nodes(_DsIpAddressPort, Peers, []).

get_all_published_nodes(_DsIpAddressPort, [], Acc) ->
    {ok, lists:reverse(Acc)};
get_all_published_nodes(DsIpAddressPort, [#peer{na = Na}|Rest], Acc) ->
    case ds_jsonrp:reserved_oas(DsIpAddressPort, Na) of
        {ok, [Oa]} ->
            get_all_published_nodes(DsIpAddressPort, Rest, [{Oa, Na}|Acc]);
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% get_global_route_table
%%%

get_global_route_table(Nodes) ->
    AllRouteEntries = get_all_route_entries(Nodes),
    merge_route_entries(AllRouteEntries, AllRouteEntries).

get_all_route_entries([]) ->
    [];
get_all_route_entries([{Oa, Na}|Rest]) ->
    case node_route_jsonrpc:get_route_entries(undefined, Na) of
        {ok, RouteEntries} ->
            [{Oa, Na, RouteEntries}|get_all_route_entries(Rest)];
        {error, Reason} ->
            [{Oa, Na, {not_available, Reason}}|get_all_route_entries(Rest)]
    end.

merge_route_entries([], _AllRouteEntries) ->
    [];
merge_route_entries([{Oa, _Na, RouteEntries}|Rest], AllRouteEntries) ->
    [traverse_each_destination(Oa, RouteEntries, AllRouteEntries)|
     merge_route_entries(Rest, AllRouteEntries)].

traverse_each_destination(_FromOa, [], _AllRouteEntries) ->
    [];
traverse_each_destination(Oa, [#route_entry{oa = Oa}|Rest],
                          AllRouteEntries) ->
    traverse_each_destination(Oa, Rest, AllRouteEntries);
traverse_each_destination(FromOa, [#route_entry{oa = ToOa, na = Na,
                                                path_cost = Pc}|Rest],
                          AllRouteEntries) ->
    OaTrail = walk_to_destination(ToOa, Na, AllRouteEntries, []),
    [{FromOa, ToOa, Pc, OaTrail}|
     traverse_each_destination(FromOa, Rest, AllRouteEntries)].

walk_to_destination(ToOa, Na, AllRouteEntries, Acc) ->
    case lists:keysearch(Na, 2, AllRouteEntries) of
        false ->
            [];
        {value, {NextOa, Na, RouteEntries}} ->
            case lists:keysearch(ToOa, 2, RouteEntries) of
                {value, #route_entry{oa = ToOa, path_cost = 0}} ->
                    lists:reverse([NextOa|Acc]);
                {value, #route_entry{oa = ToOa, na = NextNa}} ->
                    walk_to_destination(ToOa, NextNa, AllRouteEntries,
                                        [NextOa|Acc]);
                false ->
                    []
            end
    end.
