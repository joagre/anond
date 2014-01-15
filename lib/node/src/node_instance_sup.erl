-module(node_instance_sup).
-behaviour(supervisor).

%%% external exports
-export([start_link/1]).
-export([lookup_child/2]).

%%% internal exports

%%% supervisor exports
-export([init/1]).

%%% include files
-include_lib("node/include/node.hrl").

%%% constants

%%% records

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link(na()) -> supervisor:startlink_ret().

start_link(Na) ->
    supervisor:start_link(?MODULE, [Na]).

%%%
%%% exported: lookup_child
%%%

-spec lookup_child(supervisor:sup_ref(), supervisor:child_id()) ->
                          {'ok', supervisor:child()} |
                          {'error', 'not_found'}.

lookup_child(NodeInstanceSup, Id) ->
    Children = supervisor:which_children(NodeInstanceSup),
    case lists:keysearch(Id, 1, Children) of
        {value, {Id, Child, _Type, _Modules}} ->
            {ok, Child};
        false ->
            not_found
    end.

%%%
%%% exported: init
%%%

init([Na]) ->
    NodeRouteServChildSpec =
        {node_route_serv,
         {node_route_serv, start_link, [Na, self()]},
         permanent, 10000, worker, [node_route_serv]},
    NodeRouteJsonrpcServChildSpec =
        {node_route_jsonrpc_serv,
         {node_route_jsonrpc_serv, start_link, [Na, self()]},
         permanent, 10000, worker, [node_route_jsonrpc_serv]},
    NodeRecvServChildSpec =
        {node_recv_serv,
         {node_recv_serv, start_link, [Na, self()]},
         permanent, 10000, worker, [node_recv_serv]},
    NodeSendSupChildSpec =
        {node_send_sup,
         {node_send_sup, start_link, []},
         permanent, infinity, supervisor, [node_send_sup]},
    NodeTunServChildSpec =
        {node_tun_serv,
         {node_tun_serv, start_link, [Na, self()]},
         permanent, 10000, worker, [node_tun_serv]},
    NodePathCostServChildSpec =
        {node_path_cost_serv,
         {node_path_cost_serv, start_link, [Na, self()]},
         permanent, brutal_kill, worker, [node_path_cost_serv]},
    {ok, {{rest_for_one, 3, 10},
          [NodeRouteServChildSpec, NodeRouteJsonrpcServChildSpec,
           NodeRecvServChildSpec, NodeSendSupChildSpec, NodeTunServChildSpec,
           NodePathCostServChildSpec]}}.
