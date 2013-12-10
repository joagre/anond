-module(node_starter_serv).

%%% external exports
-export([start_link/0, stop/1, stop/2]).

%%% internal exports
-export([init/1]).

%%% include files
-include_lib("util/include/log.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/shorthand.hrl").
-include_lib("node/include/node.hrl").

%%% constants

%%% records
-record(state, {
          parent      :: pid(),
          nas_db = [] :: [{na(), supervisor:sup_ref()}]
	 }).

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link() -> {'ok', pid()}.

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

-spec stop(pid()) -> 'ok'.

stop(Pid) -> 
    stop(Pid, 15000).

-spec stop(pid(), timeout()) -> 'ok'.

stop(Pid, Timeout) ->
    serv:call(Pid, stop, Timeout).

%%%
%%% server loop
%%%

init(Parent) ->
    process_flag(trap_exit, true),
    ok = config_json_serv:subscribe(),
    Parent ! {self(), started},
    S = read_config(#state{}),
    loop(S#state{parent = Parent}).

loop(#state{parent = Parent} = S) ->
    receive
        config_updated ->
            loop(read_config(S));
	{From, stop} ->
	    From ! {self(), ok};
	{'EXIT', Parent, Reason} ->
            exit(Reason);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

%%%
%%% init
%%%

read_config(#state{nas_db = NasDb} = S) ->
    Nas = [Na || [{'node-address', Na}|_] <- ?config([nodes])],
    ?iof("BAJS1: ~p~n", [Nas]),
    StillRunningNasDb = stop_nodes(NasDb, Nas),
    ?iof("BAJS2: ~p~n", [StillRunningNasDb]),
    NowRunningNasDb = start_nodes(StillRunningNasDb, Nas),
    ?iof("BAJS3: ~p~n", [NowRunningNasDb]),
    S#state{nas_db = NowRunningNasDb}.

stop_nodes([], _Nas) ->
    [];
stop_nodes([{Na, NodeInstanceSup}|Rest], Nas) ->
    case lists:member(Na, Nas) of
        true ->
            [{Na, NodeInstanceSup}|stop_nodes(Rest, Nas)];
        false ->
            case node_sup:stop_node(NodeInstanceSup) of
                ok ->
                    stop_nodes(Rest, Nas);
                {error, Reason} ->
                    ?error_log({could_not_stop_node, Reason}),
                    stop_nodes(Rest, Nas)
            end
    end.

start_nodes(NasDb, Nas) ->
    start_nodes(NasDb, Nas, []).

start_nodes(_NasDb, [], Acc) ->
    Acc;
start_nodes(NasDb, [Na|Rest], Acc) ->
    case lists:keysearch(Na, 1, NasDb) of
        {value, {Na, NodeInstanceSup}} ->
            start_nodes(NasDb, Rest, [{Na, NodeInstanceSup}|Acc]);
        false ->
            case node_sup:start_node(Na) of
                {ok, NodeInstanceSup} ->
                    start_nodes(NasDb, Rest, [{Na, NodeInstanceSup}|Acc]);
                {error, Reason} ->
                    ?daemon_log("Could not start node (~p)", [Reason]),
                    start_nodes(NasDb, Rest, Acc)
            end
    end.
