-module(template_serv).

%%% external exports
-export([start_link/0, start_link/1, stop/1, stop/2]).

%%% system exports
-export([system_continue/3, system_terminate/4, system_code_change/4,
         system_get_state/1, system_replace_state/2]).

%%% internal exports
-export([init/1]).

%%% include files
-include_lib("util/include/log.hrl").

%%% constants
-define(BAZ, #state{bar = 42}).

%%% records
-record(state, {
	  parent :: pid(),
	  baz    :: zap()
	 }).

%%% types
-type zap() :: #state{}.

%%%
%%% exported: start_link
%%%

-spec start_link(timeout()) -> {'ok', pid()} | {'error', any()}.

start_link() ->
    start_link(60000).

start_link(Timeout) ->
    Args = [self()],
    Pid = proc_lib:spawn_link(?MODULE, init, Args),
    receive
	{Pid, started} ->
	    {ok, Pid};
	{Pid, Reason} ->
            {error, Reason}
    after
        Timeout ->
            {error, timeout}
    end.

%%%
%%% exported: stop
%%%

-spec stop(pid(), timeout()) -> 'ok'.

stop(Pid) ->
    stop(Pid, 15000).

stop(Pid, Timeout) ->
    serv:call(Pid, stop, Timeout).

%%%
%%% server loop
%%%

init(Parent) ->
    process_flag(trap_exit, true),
    case catch register(?MODULE, self()) of
        true ->
            case setup(Parent) of
                {ok, S} ->
                    Parent ! {self(), started},
                    loop(S);
                {error, Reason} ->
                    Parent ! {self(), {not_started, Reason}}
            end;
        _ ->
            Parent ! {self(), already_started}
    end.

setup(Parent) ->
    {ok, #state{parent = Parent}}.

loop(#state{parent = Parent} = S) ->
    receive
	{From, stop} ->
	    From ! {self(), ok};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        {system, From, Msg} ->
            sys:handle_system_msg(Msg, From, Parent, undefined, ?MODULE, S);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

system_continue(_Parent, undefined, S) ->
    loop(S).

system_terminate(Reason, _Parent, undefined, _S) ->
    exit(Reason).

system_code_change(S, _Module, _OldVsn, _Extra) ->
    {ok, S}.

system_get_state(S) ->
    {ok, S}.

system_replace_state(StateFun, S) ->
    NewS = StateFun(S),
    {ok, NewS, NewS}.

%%%
%%% major partition (may consist of minor partitions)
%%%

%%% minor partition

%%% minor partition

%%%
%%% major partition (may consist of minor partitions)
%%%
