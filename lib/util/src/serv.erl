-module(serv).

%%% external exports
-export([call/2, call/3]).

%%% internal exports

%%% include files

%%% constants

%%% records

%%% types

%%%
%%% exported: call
%%%

-spec call(pid() | atom(), Request :: any(), timeout()) ->
                  {'error', any()} | any().

call(Pid, Request) ->
    call(Pid, Request, infinity).

call(undefined, _Request, _Timeout) ->
    {error, not_started};
call(Name, Request, Timeout) when is_atom(Name) ->
    call(whereis(Name), Request, Timeout);
call(Pid, Request, Timeout) when is_pid(Pid) ->
    Pid ! {self(), Request},
    receive
	{Pid, Reply} ->
            Reply
    after
	Timeout ->
            {error, timeout}
    end.
