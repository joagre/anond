-module(node_starter_serv).

%%% external exports
-export([start_link/0, stop/1, stop/2]).

%%% system exports
-export([system_continue/3, system_terminate/4, system_code_change/4,
         system_get_state/1, system_replace_state/2]).

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
          parent   :: pid(),
          nas = [] :: [na()]
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
    case catch register(?MODULE, self()) of
        true ->
            ok = config_json_serv:subscribe(),
            Parent ! {self(), started},
            S = read_config(#state{}),
            loop(S#state{parent = Parent});
        _ ->
            Parent ! {self(), already_started}
    end.

loop(#state{parent = Parent} = S) ->
    receive
        config_updated ->
            loop(read_config(S));
	{From, stop} ->
	    From ! {self(), ok};
	{'EXIT', Parent, Reason} ->
            exit(Reason);
        {system, From, Msg} ->
            sys:handle_system_msg(Msg, From, Parent, ?MODULE, [], S);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

system_continue(_Parent, _Debug, S) ->
    loop(S).

system_terminate(Reason, _Parent, _Debug, _S) ->
    exit(Reason).

system_code_change(S, _Module, _OldVsn, _Extra) ->
    {ok, S}.

system_get_state(S) ->
    {ok, S}.

system_replace_state(StateFun, S) ->
    NewS = StateFun(S),
    {ok, NewS, NewS}.

%%%
%%% init
%%%

read_config(#state{nas = Nas} = S) ->
    NewNas = [Na || [{'node-address', Na}|_] <- ?config([nodes])],
    StillRunningNas = stop_node_instances(Nas, NewNas),
    NowRunningNas = start_node_instances(StillRunningNas, NewNas),
    S#state{nas = NowRunningNas}.

stop_node_instances([], _NewNas) ->
    [];
stop_node_instances([Na|Rest], NewNas) ->
    case lists:member(Na, NewNas) of
        true ->
            [Na|stop_node_instances(Rest, NewNas)];
        false ->
            case node_sup:stop_node_instance(Na) of
                ok ->
                    stop_node_instances(Rest, NewNas);
                {error, Reason} ->
                    ?error_log({could_not_stop_node, Reason}),
                    stop_node_instances(Rest, NewNas)
            end
    end.

start_node_instances(StillRunningNas, NewNas) ->
    start_node_instances(StillRunningNas, NewNas, []).

start_node_instances(_StillRunningNas, [], Acc) ->
    Acc;
start_node_instances(StillRunningNas, [Na|Rest], Acc) ->
    case lists:member(Na, StillRunningNas) of
        true ->
            start_node_instances(StillRunningNas, Rest, [Na|Acc]);
        false ->
            case node_sup:start_node_instance(Na) of
                {ok, _NodeInstanceSup} ->
                    start_node_instances(StillRunningNas, Rest, [Na|Acc]);
                {error, Reason} ->
                    ?daemon_log("Could not start node ~s (~p)",
                                [net_tools:string_address(Na), Reason]),
                    start_node_instances(StillRunningNas, Rest, Acc)
            end
    end.
