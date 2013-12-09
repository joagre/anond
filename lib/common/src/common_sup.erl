-module(common_sup).
-behaviour(supervisor).

%%% external exports
-export([start_link/1]).

%%% internal exports

%%% supervisor exports
-export([init/1]).

%%% include files

%%% constants

%%% records

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link([]) -> supervisor:startlink_ret().

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%%%
%%% exported: init
%%%

init([]) ->
    CommonConfigJsonServChildSpec =
        {common_config_json_serv, {common_config_json_serv, start_link, []},
         permanent, brutal_kill, worker, [common_config_json_serv]},
    CommonLogServChildSpec =
        {common_log_serv, {common_log_serv, start_link, []},
         permanent, brutal_kill, worker, [common_log_serv]},
    {ok, {{one_for_one, 3, 10},
          [CommonConfigJsonServChildSpec, CommonLogServChildSpec]}}.
