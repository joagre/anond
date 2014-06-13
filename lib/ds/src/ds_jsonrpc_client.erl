-module(ds_jsonrpc_client).

%%% external exports
-export([get_random_nodes/5, publish_node/6, unpublish_node/4,
         still_published_nodes/5, reserve_oa/5]).

%%% internal exports

%%% include files
-include_lib("ds/include/ds.hrl").
-include_lib("node/include/node.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/jsonrpc_serv.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants

%%% records

%%% types
-type error_reason() :: jsonrpc_client:error_reason() |
                        'invalid_result' | 'einval'.

%%% exported: get_random_nodes
%%%

-spec get_random_nodes(node_id(), inet:ip_address(), httplib:ip_address_port(),
                       binary(), non_neg_integer()) ->
                              {'ok', [node_id()]} |
                              {'error', error_reason()}.

get_random_nodes(NodeId, MyIpAddress, IpAddressPort, PrivateKey, N) ->
    case jsonrpc_client:call(
           NodeId, MyIpAddress, IpAddressPort, <<"get-random-nodes">>,
           PrivateKey, N) of
        {ok, RandomNodeIds} when is_list(RandomNodeIds) ->
            case lists:all(fun(RandomNodeId) ->
                                   is_integer(RandomNodeId)
                           end, RandomNodeIds) of
                true ->
                    {ok, RandomNodeIds};
                false ->
                    {error, invalid_result}
            end;
        {ok, InvalidResult} ->
            ?error_log(InvalidResult),
            {error, invalid_result};
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: publish_node
%%%

-spec publish_node(node_id(), inet:ip_address(), httplib:ip_address_port(),
                   binary(), na(), binary()) ->
                          {'ok', ds_id(), node_id(), non_neg_integer(),
                           binary()} |
                          {'error', error_reason()}.

publish_node(NodeId, MyIpAddress, IpAddressPort, PrivateKey, MyNa, PublicKey) ->
    Params = [{<<"my-na">>, ds_jsonrpc_conversion:encode_na(MyNa)},
              {<<"public-key">>, base64:encode(PublicKey)}],
    case jsonrpc_client:call(
           NodeId, MyIpAddress, IpAddressPort, <<"publish-node">>, PrivateKey,
           Params) of
        {ok, [{<<"ds-id">>, DsId},
              {<<"node-id">>, NewNodeId},
              {<<"node-ttl">>, NodeTTL},
              {<<"shared-key">>, SharedKey}]}
          when is_integer(DsId) andalso
               is_integer(NewNodeId) andalso
               is_integer(NodeTTL) andalso
               is_binary(SharedKey) ->
            {ok, DsId, NewNodeId, NodeTTL, base64:decode(SharedKey)};
        {ok, InvalidResult} ->
            ?error_log(InvalidResult),
            {error, invalid_result};
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: unpublish_node
%%%

-spec unpublish_node(node_id(), inet:ip_address(), httplib:ip_address_port(),
                     binary()) ->
                            'ok' | {'error', error_reason()}.

unpublish_node(NodeId, MyIpAddress, IpAddressPort, PrivateKey) ->
    case jsonrpc_client:call(
           NodeId, MyIpAddress, IpAddressPort, <<"unpublish-node">>,
           PrivateKey) of
        {ok, true} ->
            ok;
        {ok, InvalidResult} ->
            ?error_log(InvalidResult),
            {error, invalid_result};
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: still_published_nodes
%%%

-spec still_published_nodes(node_id(), inet:ip_address(),
                            httplib:ip_address_port(), binary(), [node_id()]) ->
                                   {'ok', [node_id()]} |
                                   {'error', error_reason()}.

still_published_nodes(
  NodeId, MyIpAddress, IpAddressPort, PrivateKey, NodeIds) ->
    case jsonrpc_client:call(
           NodeId, MyIpAddress, IpAddressPort, <<"still-published-nodes">>,
           PrivateKey, NodeIds) of
        {ok, StillPublishedNodeIds} when is_list(StillPublishedNodeIds) ->
            {ok, StillPublishedNodeIds};
        {ok, InvalidResult} ->
            ?error_log(InvalidResult),
            {error, invalid_result};
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: reserve_oa
%%%

-spec reserve_oa(node_id(), inet:ip_address(), httplib:ip_address_port(),
                 binary(), oa()) ->
                        'ok' | {'error', error_reason()}.

reserve_oa(NodeId, MyIpAddress, IpAddressPort, PrivateKey, Oa) ->
    Params = ds_jsonrpc_conversion:encode_oa(Oa),
    case jsonrpc_client:call(
           NodeId, MyIpAddress, IpAddressPort, <<"reserve-oa">>, PrivateKey,
           Params) of
        {ok, true} ->
            ok;
        {ok, InvalidResult} ->
            ?error_log(InvalidResult),
            {error, invalid_result};
        {error, Reason} ->
            {error, Reason}
    end.
