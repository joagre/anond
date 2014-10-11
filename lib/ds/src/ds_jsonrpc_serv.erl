-module(ds_jsonrpc_serv).

%%% external exports
-export([start_link/0]).

%%% internal exports
-export([ds_handler/4]).

%%% include files

-include_lib("ds/include/ds.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/jsonrpc_serv.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants
-define(GET_NETWORK_TOPOLOGY_TIMEOUT, 30000).

%%% records
-record(state, {
	  experimental_api :: boolean()
         }).

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link() -> {ok, pid()}.

start_link() ->
    {IpAddress, Port} = ?config(['directory-server', listen]),
    JsonRpcCertificate = ?config(['directory-server', 'json-rpc-certificate']),
    ExperimentalApi = ?config(['directory-server', 'experimental-api']),
    Docroot = filename:join(code:priv_dir(ds), "docroot"),
    Options = [{lookup_public_key,
                fun(_NodeId, <<"get-network-topology">>) ->
                        ignore;
                   (NodeId, Method) ->
                        case ds_serv:lookup_node(NodeId) of
                            [] when Method == <<"publish-node">> ->
                                ignore;
                            [] ->
                                not_found;
                            [Nd] ->
                                Nd#node_descriptor.public_key
                        end
                end}],
    S = #state{experimental_api = ExperimentalApi},
    jsonrpc_serv:start_link(Options, IpAddress, Port, JsonRpcCertificate, [],
                            {?MODULE, ds_handler, [S]}, Docroot).

%%% publish-node
ds_handler(NodeId, <<"publish-node">>, Params, _S) ->
    try
        ok = jsonrpc_serv:valid_params(
               [<<"public-key">>, <<"my-na">>], Params),
        PublicKey = get_base64(<<"public-key">>, Params),
        MyNa = get_na(<<"my-na">>, Params, {{0, 0, 0, 0}, 0}),
        Nd = #node_descriptor{node_id = NodeId, na = MyNa,
                              public_key = PublicKey},
	{ok, DsId, NewNodeId, SharedKey, NodeTTL} =
	    ds_serv:publish_node(Nd),
	Result =
	    [{<<"ds-id">>, DsId},
	     {<<"node-id">>, NewNodeId},
	     {<<"shared-key">>, base64:encode(SharedKey)},
	     {<<"node-ttl">>, NodeTTL}],
	{ok, Result}
    catch
        throw:JsonError when is_record(JsonError, json_error) ->
            {error, JsonError};
        Class:Reason ->
            ?error_log({Class, Reason}),
            {error, #json_error{code = ?JSONRPC_INTERNAL_ERROR}}
    end;
%%% unpublish-node
ds_handler(NodeId, <<"unpublish-node">>, undefined, _S) ->
    ok = ds_serv:unpublish_node(NodeId),
    {ok, true};
ds_handler(_NodeId, <<"unpublish-node">>, _Params, _S) ->
    JsonError = #json_error{code = ?JSONRPC_INVALID_PARAMS},
    {error, JsonError};
%%% still-published-nodes
ds_handler(_NodeId, <<"still-published-nodes">>, NodeIds, _S)
  when is_list(NodeIds) ->
    case lists:all(fun(NodeId) when is_integer(NodeId) ->
                           true;
                      (_) ->
                           false
                   end, NodeIds) of
        true ->
            ds_serv:still_published_nodes(NodeIds);
        false ->
            JsonError = #json_error{code = ?JSONRPC_INVALID_PARAMS},
            {error, JsonError}
    end;
ds_handler(_NodeId, <<"still-published-nodes">>, _Params, _S) ->
    JsonError = #json_error{code = ?JSONRPC_INVALID_PARAMS},
    {error, JsonError};
%%% get-random-nodes
ds_handler(NodeId, <<"get-random-nodes">>, N, _S) when is_integer(N) ->
    case ds_serv:get_random_nodes(NodeId, N) of
        {ok, RandomNodeIds} ->
            {ok, RandomNodeIds};
        {error, too_few_nodes} ->
            {error, #json_error{code = ?DS_JSONRPC_TOO_FEW_NODES}};
        {error, {too_many_nodes, MaxRandomNodes}} ->
            {error, #json_error{code = ?DS_JSONRPC_TOO_MANY_NODES,
                                data = MaxRandomNodes}}
    end;
ds_handler(_NodeId, <<"get-random-nodes">>, _Params, _S) ->
    JsonError = #json_error{code = ?JSONRPC_INVALID_PARAMS},
    {error, JsonError};
%%% reserve-oa
ds_handler(NodeId, <<"reserve-oa">>, Oa, _S) ->
    case ds_jsonrpc_conversion:decode_oa(Oa) of
        {ok, DecodedOa} ->
            case ds_serv:reserve_oa(NodeId, DecodedOa) of
                ok ->
                    {ok, true};
                {error, no_such_node} ->
                    JsonError = #json_error{
                      code = ?DS_JSONRPC_UNKNOWN_NODE},
                    {error, JsonError};
                {error, too_many_oas} ->
                    JsonError = #json_error{
                      code = ?DS_JSONRPC_PERMISSION_DENIED,
                      message = <<"Too many reservations">>},
                    {error, JsonError};
                {error, already_taken} ->
                    JsonError = #json_error{
                      code = ?DS_JSONRPC_PERMISSION_DENIED,
                      message = <<"Already taken">>},
                    {error, JsonError}
            end;
        {error, Reason} ->
            ?error_log(Reason),
            JsonError = #json_error{code = ?JSONRPC_INVALID_PARAMS},
            {error, JsonError}
    end;
%%% get-network-topology (experimental api)
ds_handler(_NodeId, <<"get-network-topology">>, undefined,
           #state{experimental_api = true}) ->
    {ok, Nds} = ds_serv:get_all_nodes(),
    {ok, get_network_topology(Nds)};
ds_handler(_NodeId, Method, Params, _S) ->
    ?error_log({invalid_request, Method, Params}),
    JsonError = #json_error{code = ?JSONRPC_INVALID_REQUEST},
    {error, JsonError}.

get_base64(Param, Params) ->
    Convert =
        fun(Value) when is_binary(Value) ->
                case catch base64:decode(Value) of
                    DecodedPublicKey when is_binary(DecodedPublicKey) ->
                        DecodedPublicKey;
                    Error ->
                        ?error_log(Error),
                        throw(#json_error{
                                 code = ?JSONRPC_INVALID_PARAMS,
                                 data = Param})
                end;
           (Value) ->
                ?error_log(Value),
                throw(#json_error{
                         code = ?JSONRPC_INVALID_PARAMS,
                         data = Param})
        end,
    jsonrpc_serv:get_param(Param, Params, Convert).

get_na(Param, Params, DefaultValue) ->
    Convert =
        fun(Value) when is_binary(Value) ->
                case ds_jsonrpc_conversion:decode_na(Value) of
                    {ok, DecodedNa} ->
                        DecodedNa;
                    {error, Reason} ->
                        ?error_log(Reason),
                        throw(#json_error{
                                 code = ?JSONRPC_INVALID_PARAMS,
                                 data = Param})
                end;
           (Value) ->
                ?error_log(Value),
                throw(#json_error{
                         code = ?JSONRPC_INVALID_PARAMS,
                         data = Param})
        end,
    jsonrpc_serv:get_param(Param, Params, Convert, DefaultValue).

%%%
%%% get-network-topology
%%%

get_network_topology(Nds) ->
    timelib:start_timer(?GET_NETWORK_TOPOLOGY_TIMEOUT, timeout),
    lists:foreach(fun(#node_descriptor{node_id = NodeId}) ->
                          ds_udp_serv:get_network_topology(
                            self(), NodeId, ?GET_NETWORK_TOPOLOGY_TIMEOUT)
                  end, Nds),
    NetworkTopology = wait_for_network_topology(length(Nds)),
    ds_jsonrpc_conversion:encode_network_topology(NetworkTopology).

wait_for_network_topology(NumberOfNds) ->
    wait_for_network_topology(NumberOfNds, []).

wait_for_network_topology(NumberOfNds, Acc) ->
    receive
        {network_topology, NodeId, Na, Neighbours, Res} ->
            NewAcc = [{NodeId, Na, Neighbours, Res}|Acc],
            if
                length(NewAcc) == NumberOfNds ->
                    NewAcc;
                true ->
                    wait_for_network_topology(NumberOfNds, NewAcc)
            end;
        timeout ->
            Acc;
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            wait_for_network_topology(NumberOfNds, Acc)
    end.
