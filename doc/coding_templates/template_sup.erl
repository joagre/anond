-module(template_sup).
-behaviour(supervisor).

%%% external exports
-export([start_link/0]).

%%% internal exports

%%% supervisor exports
-export([init/1]).

%%% include files

%%% constants
-define(SERVER, ?MODULE).
-define(BAZ, #data{bar = 42}).

%%% records
-record(data, {
	  bar = 0 :: integer(),
	  baz     :: zap()
	 }).

%%% types
-type zap() :: #data{}.

%%%
%%% exported: start_link
%%%

-spec start_link() -> supervisor:startlink_ret().

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%
%%% exported: init
%%%

init([]) ->
    AChildSpec = {'AName', {'AModule', start_link, []},
                  permanent, 2000, worker, ['AModule']},
    {ok, {{one_for_all, 0, 1}, [AChildSpec]}}.

%%%
%%% major partition (may consist of minor partitions)
%%%

%%% minor partition

%%% minor partition

%%%
%%% major partition (may consist of minor partitions)
%%%
