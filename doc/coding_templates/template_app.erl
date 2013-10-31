-module(template_app).
-behaviour(application).

%%% external exports

%%% internal exports

%%% application exports
-export([start/2, stop/1]).

%%% include files

%%% constants
-define(BAZ, #data{bar = 42}).

%%% records
-record(data, {
	  bar = 0 :: integer(),
	  baz     :: zap()
	 }).

%%% types
-type zap() :: #data{}.

%%%
%%% exported: start
%%%

start(_Type, StartArgs) ->
    case 'TopSupervisor':start_link(StartArgs) of
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

%%%
%%% major partition (may consist of minor partitions)
%%%

%%% minor partition

%%% minor partition

%%%
%%% major partition (may consist of minor partitions)
%%%
