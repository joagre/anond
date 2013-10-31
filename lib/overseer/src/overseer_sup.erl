-module(overseer_sup).
-behaviour(supervisor).

%%% external exports
-export([start_link/1]).

%%% internal exports

%%% supervisor exports
-export([init/1]).

%%% include files

%%% constants
-define(SERVER, ?MODULE).

%%% records

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link([]) -> supervisor:startlink_ret().

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%%
%%% exported: init
%%%

init([]) ->
    OverseerServChild =
        {overseer_serv, {overseer_serv, start_link, []},
         permanent, 10000, worker, [overseer_serv]},
    {ok, {{one_for_one, 3, 10}, [OverseerServChild]}}.
