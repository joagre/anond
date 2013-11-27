-module(node_tunnels_sup).
-behaviour(supervisor).

%%% external exports
-export([start_link/0]).
-export([start_tunnel/4]).

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
%%% exported: start_node
%%%

-spec start_tunnel(oa(), na(), na(), supervisor:sup_ref()) ->
                          supervisor:startchild_ret().

start_tunnel(Oa, Na, PeerNa, NodeSup) ->
    Id = {node_tunnel_sup, erlang:now()},
    NodeTunnelSupChildSpec =
        {Id, {node_tunnel_sup, start_link, [Oa, Na, PeerNa, NodeSup]},
         permanent, infinity, supervisor, [node_tunnel_sup]},
    supervisor:start_child(?MODULE, NodeTunnelSupChildSpec).

%%%
%%% exported: init
%%%

init([]) ->
    {ok, {{one_for_one, 3, 10}, []}}.
