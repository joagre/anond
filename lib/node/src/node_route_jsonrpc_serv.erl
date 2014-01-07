-module(node_route_jsonrpc_serv).

%%% external exports
-export([start_link/2]).

%%% internal exports
-export([node_handler/3]).

%%% include files
-include_lib("node/include/node.hrl").
-include_lib("node/include/node_route.hrl").
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

start_link({IpAddress, Port}, NodeInstanceSup) ->
    %% I would prefer to use NodeRouteServ instead of NodeInstanceSup
    %% as handler function argument but asking for it here would lead
    %% to a deadlock. I could add support for some sort of delayed
    %% processing in tcp_serv.erl but I will not.
    jsonrpc_serv:start_link(IpAddress, Port, [],
                            {?MODULE, node_handler, [NodeInstanceSup]}).

node_handler(<<"get-route-entries">>, undefined, NodeInstanceSup) ->
    {ok, Res} =
        node_route_serv:get_route_entries(node_route_serv(NodeInstanceSup)),
    {ok, node_route_jsonrpc:encode_route_entries(Res)};
node_handler(<<"get-nodes">>, undefined, NodeInstanceSup) ->
    {ok, Nodes} = node_route_serv:get_nodes(node_route_serv(NodeInstanceSup)),
    {ok, node_route_jsonrpc:encode_nodes(Nodes)};
node_handler(<<"enable-recalc">>, undefined, NodeInstanceSup) ->
    ok = node_route_serv:enable_recalc(node_route_serv(NodeInstanceSup)),
    {ok, true};
node_handler(<<"disable-recalc">>, undefined, NodeInstanceSup) ->
    ok = node_route_serv:disable_recalc(node_route_serv(NodeInstanceSup)),
    {ok, true};
node_handler(<<"recalc">>, undefined, NodeInstanceSup) ->
    ok = node_route_serv:recalc(node_route_serv(NodeInstanceSup)),
    {ok, true};
node_handler(Method, Params, _NodeInstanceSup) ->
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
