-module(node_sup).
-behaviour(supervisor).

%%% external exports
-export([start_link/4]).
-export([start_as_child/4]).

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

%% The Erlang type system does only handle monomorphic list types
%-spec start_link([oa(), public_key:rsa_public_key(),
%%                  public_key:rsa_private_key(), boolean()]) ->
%                        supervisor:startlink_ret().

start_link(Oa, PublicKey, PrivateKey, AutoRecalc) ->
    supervisor:start_link(?MODULE, [Oa, PublicKey, PrivateKey, AutoRecalc]).

%%%
%%% exported: start_as_child
%%%

-spec start_as_child(oa(), public_key:rsa_public_key(),
                     public_key:rsa_private_key(), boolean()) ->
                            {'ok', ip()}.

start_as_child(Oa, PublicKey, PrivateKey, AutoRecalc) ->
    NodeSupChildSpec =
        {{?MODULE, erlang:now()},
         {?MODULE, start_link, [Oa, PublicKey, PrivateKey, AutoRecalc]},
         permanent, infinity, supervisor, [node_sup]},
    {ok, NodeSupPid} = supervisor:start_child(node_multi_sup, NodeSupChildSpec),
    [{_Id, Ip, _Type, _Modules}] = supervisor:which_children(NodeSupPid),
    {ok, Ip}.

%%%
%%% exported: init
%%%

init([Oa, PublicKey, PrivateKey, AutoRecalc]) ->
    NodeServChildSpec =
        {node_serv, {node_serv, start_link,
                     [Oa, PublicKey, PrivateKey, AutoRecalc]},
         permanent, 10000, worker, [node_serv]},
    {ok, {{rest_for_one, 3, 10}, [NodeServChildSpec]}}.
