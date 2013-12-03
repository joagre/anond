-module(node_root_sup).
-behaviour(supervisor).

%%% external exports
-export([start_link/1]).
-export([start_node/5]).

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

-spec start_link([]) -> supervisor:startlink_ret().

start_link([]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%
%%% exported: start_node
%%%

-spec start_node(noa(), na(), public_key:rsa_public_key(),
                 public_key:rsa_private_key(), boolean()) ->
                        supervisor:startchild_ret().

start_node(Oa, Na, PublicKey, PrivateKey, AutoRecalc) ->
    Id = {node_instance_sup, erlang:now()},
    NodeInstanceSupChildSpec =
        {Id, {node_instance_sup, start_link,
              [Oa, Na, PublicKey, PrivateKey, AutoRecalc]},
         permanent, infinity, supervisor, [node_instance_sup]},
    supervisor:start_child(?MODULE, NodeInstanceSupChildSpec).

%%%
%%% exported: init
%%%

init([]) ->
    {ok, {{one_for_one, 3, 10}, []}}.
