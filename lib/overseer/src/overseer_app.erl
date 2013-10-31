-module(overseer_app).
-behaviour(application).

%%% external exports

%%% internal exports

%%% application exports
-export([start/2, stop/1]).

%%% include files

%%% constants

%%% records

%%% types

%%%
%%% exported: start
%%%

start(_Type, StartArgs) ->
    case overseer_sup:start_link(StartArgs) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%%
%%% exported: stop
%%%

stop(_State) ->
    ok.
