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

start_link({IpAddress, Port}, NodeSup) ->
    %% I would prefer to use NodeServ instead of NodeSup as handler
    %% argument but asking for it here would mean a deadlock. I could
    %% add support for some sort of delayed prcoessing in tcp_serv.erl 
    %% but I will not.
    jsonrpc_serv:start_link(IpAddress, Port, [],
                            {?MODULE, node_handler, [NodeSup]}).

node_handler(<<"get-route-entries">>, undefined, NodeSup) ->
    {ok, Res} = node_route_serv:get_route_entries(node_route_serv(NodeSup)),
    {ok, [json_route_entry(Re) || Re <- Res]};
node_handler(<<"get-nodes">>, undefined, NodeSup) ->
    {ok, Nodes} = node_route_serv:get_nodes(node_route_serv(NodeSup)),
    {ok, [json_node(Node) || Node <- Nodes]};
node_handler(<<"enable-recalc">>, undefined, NodeSup) ->
    ok = node_route_serv:enable_recalc(node_route_serv(NodeSup)),
    {ok, true};
node_handler(<<"disable-recalc">>, undefined, NodeSup) ->
    ok = node_route_serv:disable_recalc(node_route_serv(NodeSup)),
    {ok, true};
node_handler(<<"recalc">>, undefined, NodeSup) ->
    ok = node_route_serv:recalc(node_route_serv(NodeSup)),
    {ok, true};
node_handler(Method, Params, _NodeSup) ->
    ?error_log({invalid_request, Method, Params}),
    JsonError = #json_error{code = ?JSONRPC_INVALID_REQUEST},
    {error, JsonError}.

node_route_serv(NodeSup) ->
    case get(node_route_serv) of
        undefined ->
            {ok, NodeServ} = node_sup:lookup_child(NodeSup, node_route_serv),
            put(node_route_serv, NodeServ),
            NodeServ;
        NodeServ ->
            NodeServ
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
