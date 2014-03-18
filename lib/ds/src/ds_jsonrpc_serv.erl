-module(ds_jsonrpc_serv).

%% example use:
%% $ curl -k -X POST -d '{"jsonrpc": "2.0", "method": "enforce-peer-ttl", "id": 1}' https://127.0.0.1:6700/jsonrpc
%% $ curl -k -X POST -d '{"jsonrpc": "2.0", "method": "get-number-of-peers", "id": 1}' https://127.0.0.1:6700/jsonrpc
%% $ curl -k -X POST -d '{"jsonrpc": "2.0", "method": "get-all-peers", "id": 1}' https://127.0.0.1:6700/jsonrpc
%% $ curl -k -X POST -d '{"jsonrpc": "2.0", "method": "get-random-peers", "params": {"my-na": "127.0.0.1:50010", "n": 4}, "id": 1}' https://127.0.0.1:6700/jsonrpc
%% curl -k -X POST -d '{"jsonrpc": "2.0", "method": "publish-peer", "params": {"na": "127.0.0.1:50011", "public-key": "aa", "flags": 4}, "id": 1}' https://127.0.0.1:6700/jsonrpc
%% $ curl -k -X POST -d '{"jsonrpc": "2.0", "method": "unpublish-peer", "params": {"na": "127.0.0.1:50011"}, "id": 1}' https://127.0.0.1:6700/jsonrpc
%% $ curl -k -X POST -d '{"jsonrpc": "2.0", "method": "published-peers", "params": {"nas": ["127.0.0.1:50010"]}, "id": 1}' https://127.0.0.1:6700/jsonrpc
%% $ curl -k -X POST -d '{"jsonrpc": "2.0", "method": "reserve-oa", "params": {"oa": "fe80::c685:8ff:fe46:d502", "na": "127.0.0.1:50011"}, "id": 1}' https://127.0.0.1:6700/jsonrpc
%% $ curl -k -X POST -d '{"jsonrpc": "2.0", "method": "get-network-topology", "id": 1}' https://127.0.0.1:6700/jsonrpc

%%% external exports
-export([start_link/0]).

%%% internal exports
-export([ds_handler/3]).

%%% include files
-include_lib("ds/include/ds.hrl").
-include_lib("node/include/node_route.hrl").
-include_lib("util/include/bits.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/jsonrpc_serv.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants

%%% records

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link() -> {ok, pid()}.

start_link() ->
    {IpAddress, Port} = ?config(['directory-server', listen]),
    CertFile = ?config(['directory-server', 'json-rpc-certificate']),
    Docroot = filename:join(code:priv_dir(ds), "docroot"),
    Options = [{lookup_public_key,
                fun(_Na, <<"reserved-oas">>) ->
                        ignore;
                   (_Na, <<"get-network-topology">>) ->
                        ignore;
                   (Na, Method) ->
                        case ds_serv:lookup_node(Na) of
                            [] when Method == <<"publish-node">> ->
                                ignore;
                            [] ->
                                not_found;
                            [NodeDescriptor] ->
                                NodeDescriptor#node_descriptor.public_key
                        end
                end}],
    jsonrpc_serv:start_link(Options, IpAddress, Port, CertFile, [],
                            {?MODULE, ds_handler, []}, Docroot).

ds_handler(_MyNa, <<"enforce-node-ttl">>, undefined) ->
    ok = ds_serv:enforce_node_ttl(),
    {ok, true};
ds_handler(_MyNa, <<"get-number-of-nodes">>, undefined) ->
    ds_serv:get_number_of_nodes();
ds_handler(_MyNa, <<"get-node">>, [{<<"na">>, Na}]) ->
    case node_jsonrpc:decode_na(Na) of
        {ok, DecodedNa} ->
            case ds_serv:get_node(DecodedNa) of
                {ok, NodeDescriptor} ->
                    {ok, ds_jsonrpc:encode_node_descriptor(NodeDescriptor)};
                {error, no_such_node} ->
                    JsonError = #json_error{
                      code = ?DS_JSONRPC_UNKNOWN_NODE,
                      message = <<"Unknown node">>,
                      data = Na},
                    {error, JsonError}
            end;
        {error, Reason} ->
            JsonError = #json_error{
              code = ?JSONRPC_INVALID_PARAMS,
              message = ?l2b(ds_jsonrpc:format_error(Reason)),
              data = <<"na">>},
            {error, JsonError}
    end;
ds_handler(_MyNa, <<"get-all-nodes">>, undefined) ->
    {ok, NodeDescriptors} = ds_serv:get_all_nodes(),
    {ok, ds_jsonrpc:encode_node_descriptors(NodeDescriptors)};
ds_handler(MyNa, <<"get-random-nodes">>, [{<<"n">>, N}])
  when is_integer(N) ->
    case ds_serv:get_random_nodes(MyNa, N) of
        {ok, NodeDescriptors} ->
            {ok, ds_jsonrpc:encode_node_descriptors(NodeDescriptors)};
        {error, too_few_nodes} ->
            {error, #json_error{
               code = ?DS_JSONRPC_TOO_FEW_NODES}}
    end;
ds_handler(MyNa, <<"publish-node">>, PublicKey) when is_binary(PublicKey) ->
    NodeDescriptor= #node_descriptor{na = MyNa,
                                     public_key = base64:decode(PublicKey)},
    ds_serv:publish_node(NodeDescriptor);
ds_handler(_MyNa, <<"unpublish-node">>, [{<<"na">>, Na}]) ->
    case node_jsonrpc:decode_na(Na) of
        {ok, DecodedNa} ->
            ok = ds_serv:unpublish_node(DecodedNa),
            {ok, true};
        {error, Reason} ->
            JsonError = #json_error{
              code = ?JSONRPC_INVALID_PARAMS,
              message = ?l2b(ds_jsonrpc:format_error(Reason)),
              data = <<"na">>},
            {error, JsonError}
    end;
ds_handler(_MyNa, <<"published-nodes">>, [{<<"nas">>, Nas}]) ->
    case node_jsonrpc:decode_nas(Nas) of
        {ok, DecodedNas} ->
            {ok, PublishedNas} = ds_serv:published_nodes(DecodedNas),
            {ok, node_jsonrpc:encode_nas(PublishedNas)};
        {error, Reason} ->
            JsonError = #json_error{
              code = ?JSONRPC_INVALID_PARAMS,
              message = ?l2b(ds_jsonrpc:format_error(Reason)),
              data = <<"na">>},
            {error, JsonError}
    end;
ds_handler(MyNa, <<"reserve-oa">>, [{<<"oa">>, Oa}]) ->
    case node_jsonrpc:decode_oa(Oa) of
        {ok, DecodedOa} ->
            case ds_serv:reserve_oa(DecodedOa, MyNa) of
                ok ->
                    {ok, true};
                {error, no_such_node} ->
                    JsonError = #json_error{
                      code = ?DS_JSONRPC_UNKNOWN_NODE,
                      message = <<"Unknown node">>,
                      data = node_jsonrpc:encode_na(MyNa)},
                    {error, JsonError};
                {error, too_many_oas} ->
                    JsonError = #json_error{
                      code = ?DS_JSONRPC_PERMISSION_DENIED,
                      message = <<"Too many reservations">>},
                    {error, JsonError}
            end;
        {error, Reason} ->
            JsonError = #json_error{
              code = ?JSONRPC_INVALID_PARAMS,
              message = ?l2b(ds_jsonrpc:format_error(Reason)),
              data = <<"oa">>},
            {error, JsonError}
    end;
%% experimental api (must be restricted)
ds_handler(MyNa, <<"reserved-oas">>, undefined) ->
    case ds_serv:reserved_oas(MyNa) of
        {ok, ReservedOas} ->
            {ok, node_jsonrpc:encode_oas(ReservedOas)};
        {error, no_reserved_oas} ->
            JsonError = #json_error{
              code = ?DS_JSONRPC_NO_RESERVED_OAS,
              message = <<"No reserved overlay addresses">>,
              data = node_jsonrpc:encode_na(MyNa)},
            {error, JsonError}
    end;
%% experimental api (must be restricted)
ds_handler(_MyNa, <<"get-network-topology">>, undefined) ->
    {ok, NodeDescriptors} = ds_serv:get_all_nodes(),
    {ok, get_network_topology(NodeDescriptors)};
ds_handler(_MyNa, Method, Params) ->
    ?error_log({invalid_request, Method, Params}),
    JsonError = #json_error{code = ?JSONRPC_INVALID_REQUEST},
    {error, JsonError}.

%%%
%%% get-network-topology
%%%

get_network_topology([]) ->
    [];
get_network_topology([#node_descriptor{na = Na}|Rest]) ->
    case node_route_jsonrpc:get_nodes(undefined, Na) of
        {ok, Nodes} ->
            case node_route_jsonrpc:get_route_entries(undefined, Na) of
                {ok, Res} ->
                    [[{<<"na">>, node_jsonrpc:encode_na(Na)},
                      {<<"neighbour-nodes">>, encode_topology_nodes(Nodes)},
                      {<<"route-entries">>,
                       encode_topology_route_entries(Res)}]|
                     get_network_topology(Rest)];
                {error, _Reason} ->
                    [[{<<"na">>, node_jsonrpc:encode_na(Na)},
                      {<<"neighbour-nodes">>, encode_topology_nodes(Nodes)},
                      {<<"route-entries">>, []}]|
                     get_network_topology(Rest)]
            end;
        {error, _Reason} ->
            [[{<<"na">>, node_jsonrpc:encode_na(Na)},
              {<<"neighbour-nodes">>, []},
              {<<"route-entries">>, []}]|
             get_network_topology(Rest)]
    end.

encode_topology_nodes(Nodes) ->
    [encode_topology_node(Node) || Node <- Nodes].

encode_topology_node(Node) ->
    [{<<"na">>, node_jsonrpc:encode_na(Node#node.na)},
     {<<"path-cost">>, Node#node.path_cost},
     {<<"incoming-neighbour-node">>,
      ?bit_is_set(Node#node.flags, ?F_NODE_IS_INCOMING_PEER)}].

encode_topology_route_entries(Res) ->
    [[{<<"path-cost">>, PathCost},
      {<<"route">>, node_jsonrpc:encode_nas(Hops)}] ||
        #route_entry{path_cost = PathCost,
                     hops = Hops} <- Res,
        Hops /= []].
