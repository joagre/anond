-module(template_serv).

%%% external exports
-export([start_link/0, start_link/1, stop/1, stop/2]).

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
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

%%%
%%% major partition (may consist of minor partitions)
%%%

%%% minor partition

%%% minor partition

%%%
%%% major partition (may consist of minor partitions)
%%%
