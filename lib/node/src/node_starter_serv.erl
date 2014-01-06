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
    Nas = [Na || [_, {'node-address', Na}|_] <- ?config([nodes])],
    StillRunningNasDb = stop_node_instances(NasDb, Nas),
    NowRunningNasDb = start_node_instances(StillRunningNasDb, Nas),
    S#state{nas_db = NowRunningNasDb}.

stop_node_instances([], _Nas) ->
    [];
stop_node_instances([{Na, NodeInstanceSup}|Rest], Nas) ->
    case lists:member(Na, Nas) of
        true ->
            [{Na, NodeInstanceSup}|stop_node_instances(Rest, Nas)];
        false ->
            case node_sup:stop_node_instance(NodeInstanceSup) of
                ok ->
                    stop_node_instances(Rest, Nas);
                {error, Reason} ->
                    ?error_log({could_not_stop_node, Reason}),
                    stop_node_instances(Rest, Nas)
            end
    end.

start_node_instances(NasDb, Nas) ->
    start_node_instances(NasDb, Nas, []).

start_node_instances(_NasDb, [], Acc) ->
    Acc;
start_node_instances(NasDb, [Na|Rest], Acc) ->
    case lists:keysearch(Na, 1, NasDb) of
        {value, {Na, NodeInstanceSup}} ->
            start_node_instances(NasDb, Rest, [{Na, NodeInstanceSup}|Acc]);
        false ->
            case node_sup:start_node_instance(Na) of
                {ok, NodeInstanceSup} ->
                    start_node_instances(NasDb, Rest,
                                         [{Na, NodeInstanceSup}|Acc]);
                {error, Reason} ->
                    ?daemon_log("Could not start node (~p)", [Reason]),
                    start_node_instances(NasDb, Rest, Acc)
            end
    end.
