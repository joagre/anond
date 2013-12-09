-module(node_starter_serv).

%%% external exports
-export([start_link/0, stop/1, stop/2]).

%%% internal exports
-export([init/1]).

%%% include files
-include_lib("util/include/log.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants

%%% records
-record(state, {
          parent :: pid()
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
    S = read_config(#state{}),    
    Parent ! {self(), started},
    loop(S#state{parent = Parent}).

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
%%% init
%%%

read_config(S) ->
    %%WHAT = ?cfg([node, 'node-address']),
    %%?iof("WHAT: ~p~n", [WHAT]),
    S.
