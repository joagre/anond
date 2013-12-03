-module(node_tunnel_sup).
-behaviour(supervisor).

%%% external exports
-export([start_link/4]).
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

-spec start_link(noa(), na(), na(), supervisor:sup_ref()) ->
                        supervisor:startlink_ret().

start_link(Oa, Na, PeerNa, NodeInstanceSup) ->
    supervisor:start_link(?MODULE, [Oa, Na, PeerNa, NodeInstanceSup]).

%%%
%%% exported: lookup_child
%%%

-spec lookup_child(supervisor:sup_ref(), supervisor:child_id()) ->
                          {'ok', supervisor:child()} |
                          {'error', 'not_found'}.

lookup_child(Sup, Id) ->
    Children = supervisor:which_children(Sup),
    case lists:keysearch(Id, 1, Children) of
        {value, {Id, Child, _Type, _Modules}} ->
            {ok, Child};
        false ->
            not_found
    end.

%%%
%%% exported: init
%%%

init([Oa, Na, PeerNa, NodeInstanceSup]) ->
    NodeTunnelRecvServChildSpec =
        {node_tunnel_recv_serv,
         {node_tunnel_recv_serv, start_link,
          [Oa, Na, PeerNa, NodeInstanceSup]},
         permanent, 10000, worker, [node_tunnel_recv_serv]},
    NodeTunnelSendServChildSpec =
        {node_tunnel_send_serv,
         {node_tunnel_send_serv, start_link,
          [Na, PeerNa, NodeInstanceSup, self()]},
         permanent, 10000, worker, [node_tunnel_send_serv]},
    {ok, {{rest_for_one, 3, 10},
          [NodeTunnelRecvServChildSpec, NodeTunnelSendServChildSpec]}}.
