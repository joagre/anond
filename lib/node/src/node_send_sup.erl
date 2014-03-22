-module(node_send_sup).
-behaviour(supervisor).

%%% external exports
-export([start_link/0]).
-export([start_node_send_serv/3, stop_node_send_serv/2]).

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

-spec start_link() -> supervisor:startlink_ret().

start_link() ->
    supervisor:start_link(?MODULE, []).

%%%
%%% exported: start_node_send_serv
%%%

-spec start_node_send_serv(na(), na(), supervisor:sup_ref()) ->
                                  supervisor:startchild_ret().

start_node_send_serv(Na, NeighbourNa, NodeInstanceSup) ->
    Id = {node_send_serv, NeighbourNa},
    {ok, NodeSendSup} =
        node_instance_sup:lookup_child(NodeInstanceSup, node_send_sup),
    {ok, NodeRouteServ} =
        node_instance_sup:lookup_child(NodeInstanceSup, node_route_serv),
    {ok, NodeRecvServ} =
        node_instance_sup:lookup_child(NodeInstanceSup, node_recv_serv),
    NodeSendServChildSpec =
        {Id, {node_send_serv, start_link,
              [Na, NeighbourNa, NodeRouteServ, NodeRecvServ]},
         permanent, 10000, worker, [node_send_serv]},
    supervisor:start_child(NodeSendSup, NodeSendServChildSpec).

%%%
%%% exported: stop_node_send_serv
%%%

-spec stop_node_send_serv(na(), supervisor:sup_ref()) ->
                                 'ok' |
                                 {'error',
                                  'running' | 'restarting' | 'not_found' |
                                  'simple_one_for_one'}.

stop_node_send_serv(NeighbourNa, NodeInstanceSup) ->
    {ok, NodeSendSup} =
        node_instance_sup:lookup_child(NodeInstanceSup, node_send_sup),
    Id = {node_send_serv, NeighbourNa},
    case supervisor:terminate_child(NodeSendSup, Id) of
        ok ->
            supervisor:delete_child(NodeSendSup, Id);
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: init
%%%

init([]) ->
    {ok, {{one_for_one, 3, 10}, []}}.
