-module(node_route_jsonrpc_serv).

%% example use:
%% $ curl -k -X POST -d '{"jsonrpc": "2.0", "method": "get-route-entries", "id": 1}' https://127.0.0.1:50010/jsonrpc
%% $ curl -k -X POST -d '{"jsonrpc": "2.0", "method": "get-nodes", "id": 1}' https://127.0.0.1:50010/jsonrpc
%% $ curl -k -X POST -d '{"jsonrpc": "2.0", "method": "enable-recalc", "id": 1}' https://127.0.0.1:50010/jsonrpc
%% $ curl -k -X POST -d '{"jsonrpc": "2.0", "method": "disable-recalc", "id": 1}' https://127.0.0.1:50010/jsonrpc
%% $ curl -k -X POST -d '{"jsonrpc": "2.0", "method": "recalc", "id": 1}' https://127.0.0.1:50010/jsonrpc

%%% external exports
-export([start_link/2]).

%%% internal exports
-export([node_handler/4]).

%%% include files
-include_lib("node/include/node.hrl").
-include_lib("node/include/node_route.hrl").
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

-spec start_link(na(), supervisor:sup_ref()) -> {ok, pid()}.

start_link({IpAddress, Port} = Na, NodeInstanceSup) ->
    NodeInstance = ?config([nodes, {'node-address', Na}]),
    {value, {'json-rpc-certificate', CertFile}} =
        lists:keysearch('json-rpc-certificate', 1, NodeInstance),
    %% I would prefer to use NodeRouteServ instead of NodeInstanceSup
    %% as handler function argument but asking for it here would lead
    %% to a deadlock. I could add support for some sort of delayed
    %% processing in net_serv.erl but I will not.
    jsonrpc_serv:start_link([], IpAddress, Port, CertFile, [],
                            {?MODULE, node_handler, [NodeInstanceSup]},
                            undefined).

%% experimental api (must be restricted)
node_handler(_MyIpAddressPort, <<"get-route-entries">>, undefined,
             NodeInstanceSup) ->
    {ok, Res} =
        node_route_serv:get_route_entries(node_route_serv(NodeInstanceSup)),
    {ok, node_route_jsonrpc:encode_route_entries(Res)};
%% experimental api (must be restricted)
node_handler(_MyIpAddressPort, <<"get-nodes">>, undefined, NodeInstanceSup) ->
    {ok, Nodes} = node_route_serv:get_nodes(node_route_serv(NodeInstanceSup)),
    {ok, node_route_jsonrpc:encode_nodes(Nodes)};
%% experimental api (must be restricted)
node_handler(_MyIpAddressPort,<<"enable-recalc">>, undefined,
             NodeInstanceSup) ->
    ok = node_route_serv:enable_recalc(node_route_serv(NodeInstanceSup)),
    {ok, true};
%% experimental api (must be restricted)
node_handler(_MyIpAddressPort, <<"disable-recalc">>, undefined,
             NodeInstanceSup) ->
    ok = node_route_serv:disable_recalc(node_route_serv(NodeInstanceSup)),
    {ok, true};
%% experimental api (must be restricted)
node_handler(_MyIpAddressPort, <<"recalc">>, undefined, NodeInstanceSup) ->
    ok = node_route_serv:recalc(node_route_serv(NodeInstanceSup)),
    {ok, true};
node_handler(_MyIpAddressPort, Method, Params, _NodeInstanceSup) ->
    ?error_log({invalid_request, Method, Params}),
    JsonError = #json_error{code = ?JSONRPC_INVALID_REQUEST},
    {error, JsonError}.

node_route_serv(NodeInstanceSup) ->
    case get(node_route_serv) of
        undefined ->
            {ok, NodeRouteServ} =
                node_instance_sup:lookup_child(NodeInstanceSup,
                                               node_route_serv),
            put(node_route_serv, NodeRouteServ),
            NodeRouteServ;
        NodeRouteServ ->
            NodeRouteServ
    end.
