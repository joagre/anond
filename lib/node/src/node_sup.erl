-module(node_sup).
-behaviour(supervisor).

%%% external exports
-export([start_link/1]).
-export([start_node_instance/1, stop_node_instance/1, lookup_node_instance/1]).
-export([get_state/2]).

%%% internal exports

%%% supervisor exports
-export([init/1]).

%%% include files
-include_lib("node/include/node.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants

%%% records

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link([]) -> supervisor:startlink_ret().

start_link([]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%
%%% exported: start_node_instance
%%%

-spec start_node_instance(na()) -> supervisor:startchild_ret().

start_node_instance(Na) ->
    Id = {node_instance_sup, Na},
    NodeInstanceSupChildSpec =
        {Id, {node_instance_sup, start_link, [Na]},
         permanent, infinity, supervisor, [node_instance_sup]},
    supervisor:start_child(?MODULE, NodeInstanceSupChildSpec).

%%%
%%% exported: stop_node_instance
%%%

-spec stop_node_instance(na()) ->
                                'ok' |
                                {'error',
                                 'running' | 'restarting' | 'not_found' |
                                 'simple_one_for_one'}.

stop_node_instance(Na) ->
    Id = {node_instance_sup, Na},
    case supervisor:terminate_child(?MODULE, Id) of
        ok ->
            supervisor:delete_child(?MODULE, Id);
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: lookup_node_instance
%%%

-spec lookup_node_instance(na()) ->
				  {'ok', supervisor:child()} |
				  'not_found'.

lookup_node_instance(Na) ->
    Children = supervisor:which_children(?MODULE),
    case lists:keysearch({node_instance_sup, Na}, 1, Children) of
        {value, {_Id, NodeInstanceSup, _Type, _Modules}} ->
            {ok, NodeInstanceSup};
        false ->
            not_found
    end.

%%%
%%% exported: get_state
%%%

-spec get_state(na(), supervisor:child_id()) ->
		       'not_found' | term().

get_state(Na, Id) ->
    case lookup_node_instance(Na) of
	{ok, NodeInstanceSup} ->
	    node_instance_sup:get_state(NodeInstanceSup, Id);
	Error ->
	    Error
    end.

%%%
%%% exported: init
%%%

init([]) ->
    NodeStarterServChildSpec =
        {node_starter_serv,
         {node_starter_serv, start_link, []},
         permanent, 10000, worker, [node_starter_serv]},
    {ok, {{one_for_one, 3, 10}, [NodeStarterServChildSpec]}}.
