-module(node_jsonrpc_serv).

%%% external exports
-export([start_link/2]).

%%% internal exports
-export([node_handler/3]).

%%% include files
-include_lib("util/include/jsonrpc.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").
-include_lib("node/include/node.hrl").
-include_lib("node/include/node_route.hrl").

%%% constants

%%% records

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link(na(), supervisor:sup_ref()) -> {ok, pid()}.

start_link({IpAddress, Port}, NodeInstanceSup) ->
    %% I would prefer to use NodeRouteServ instead of NodeInstanceSup as handler
    %% argument but asking for it here would mean a deadlock. I could
    %% add support for some sort of delayed prcoessing in tcp_serv.erl 
    %% but I will not.
    jsonrpc_serv:start_link(IpAddress, Port, [],
                            {?MODULE, node_handler, [NodeInstanceSup]}).

node_handler(<<"get-route-entries">>, undefined, NodeInstanceSup) ->
    {ok, Res} = node_route_serv:get_route_entries(node_route_serv(NodeInstanceSup)),
    {ok, [json_route_entry(Re) || Re <- Res]};
node_handler(<<"get-nodes">>, undefined, NodeInstanceSup) ->
    {ok, Nodes} = node_route_serv:get_nodes(node_route_serv(NodeInstanceSup)),
    {ok, [json_node(Node) || Node <- Nodes]};
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
            {ok, NodeRouteServ} = node_instance_sup:lookup_child(NodeInstanceSup, node_route_serv),
            put(node_route_serv, NodeRouteServ),
            NodeRouteServ;
        NodeRouteServ ->
            NodeRouteServ
    end.

json_route_entry(Re) ->
    [{<<"oa">>, Re#route_entry.oa},
     %%{<<"na">>, encode_na(Re#route_entry.na)},
     {<<"path-cost">>, Re#route_entry.path_cost},
     {<<"flags">>, Re#route_entry.flags}].

%%encode_na({IpAddress, Port}) ->
%%    ?l2b([net_tools:string_address(IpAddress), ":", ?i2l(Port)]).

json_node(Node) ->
    [{<<"public-key">>, Node#node.public_key},
     {<<"path-cost">>, Node#node.path_cost}].
