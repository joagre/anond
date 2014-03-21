-module(node_route_jsonrpc).

%% example use:
%% node_route_jsonrpc:get_route_entries(undefined, {{127,0,0,1}, 50010}).
%% node_route_jsonrpc:get_nodes(undefined, {{127,0,0,1}, 50010}).
%% node_route_jsonrpc:enable_recalc(undefined, {{127,0,0,1}, 50010}).
%% node_route_jsonrpc:disable_recalc(undefined, {{127,0,0,1}, 50010}).
%% node_route_jsonrpc:recalc(undefined, {{127,0,0,1}, 50010}).

%%% external exports
-export([format_error/1]).
-export([get_route_entries/2, get_nodes/2, enable_recalc/2, disable_recalc/2,
         recalc/2]).
-export([encode_route_entries/1, encode_route_entry/1, decode_route_entries/1,
         decode_route_entry/1]).
-export([encode_nodes/1, encode_node/1, decode_nodes/1, decode_node/1]).

%%% internal exports

%%% include files
-include_lib("node/include/node_route.hrl").
-include_lib("util/include/jsonrpc_serv.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants

%%% records

%%% types
-type error_reason() :: jsonrpc:error_reason() | 'einval'.

%%%
%%% exported: format_error
%%%

format_error(Reason) ->
    inet:format_error(Reason).

%%%
%%% exported: get_route_entries
%%%

-spec get_route_entries(httplib:ip_address_port() | 'undefined',
                        httplib:ip_address_port()) ->
                               {'ok', [#route_entry{}]} |
                               {'error', error_reason()}.

get_route_entries(LocalIpAddressPort, IpAddressPort) ->
    case jsonrpc:call(LocalIpAddressPort, IpAddressPort,
                      <<"get-route-entries">>) of
        {ok, Res} ->
            decode_route_entries(Res);
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: get_nodes
%%%

-spec get_nodes(httplib:ip_address_port() | 'undefined',
                httplib:ip_address_port()) ->
                       {'ok', [#node{}]} | {'error', error_reason()}.

get_nodes(LocalIpAddressPort, IpAddressPort) ->
    case jsonrpc:call(LocalIpAddressPort, IpAddressPort, <<"get-nodes">>) of
        {ok, Nodes} ->
            decode_nodes(Nodes);
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: enable_recalc
%%%

-spec enable_recalc(httplib:ip_address_port() | 'undefined',
                    httplib:ip_address_port()) ->
                           'ok' | {'error', error_reason()}.

enable_recalc(LocalIpAddressPort, IpAddressPort) ->
    case jsonrpc:call(LocalIpAddressPort, IpAddressPort, <<"enable-recalc">>) of
        {ok, true} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: disable_recalc
%%%

-spec disable_recalc(httplib:ip_address_port() | 'undefined',
                     httplib:ip_address_port()) ->
                            'ok' | {'error', error_reason()}.

disable_recalc(LocalIpAddressPort, IpAddressPort) ->
    case jsonrpc:call(LocalIpAddressPort, IpAddressPort,
                      <<"disable-ecalc">>) of
        {ok, true} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: recalc
%%%

-spec recalc(httplib:ip_address_port() | 'undefined',
             httplib:ip_address_port()) ->
                    'ok' | {'error', error_reason()}.

recalc(LocalIpAddressPort, IpAddressPort) ->
    case jsonrpc:call(LocalIpAddressPort, IpAddressPort, <<"recalc">>) of
        {ok, true} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: encode_route_entries
%%%

-spec encode_route_entries([#route_entry{}]) -> jsx:json_term().

encode_route_entries(Res) ->
    [encode_route_entry(Re) || Re <- Res].

%%%
%%% exported: encode_route_entry
%%%

-spec encode_route_entry(#route_entry{}) -> jsx:json_term().

encode_route_entry(Re) ->
    [{<<"oa">>, node_jsonrpc:encode_oa(Re#route_entry.oa)},
     {<<"na">>, node_jsonrpc:encode_na(Re#route_entry.na)},
     {<<"path-cost">>, Re#route_entry.path_cost},
     {<<"hops">>, node_jsonrpc:encode_nas(Re#route_entry.hops)},
     {<<"psp">>, base64:encode(Re#route_entry.psp)}].

%%%
%%% exported: decode_route_entries
%%%

-spec decode_route_entries(jsx:json_term()) ->
                                  {'ok', [#route_entry{}]} |
                                  {'error', 'einval'}.

decode_route_entries(Res) ->
    jsonrpc:decode(Res, fun decode_route_entry/1).

%%%
%%% exported: decode_route_entry
%%%

-spec decode_route_entry(jsx:json_term()) ->
                                {'ok', #route_entry{}} | {'error', 'einval'}.

decode_route_entry([{<<"oa">>, Oa},
                    {<<"na">>, Na},
                    {<<"path-cost">>, Pc},
                    {<<"hops">>, Hops},
                    {<<"psp">>, Psp}]) when is_integer(Pc), is_binary(Psp) ->
    case node_jsonrpc:decode_oa(Oa) of
        {ok, DecodedOa} ->
            case node_jsonrpc:decode_na(Na) of
                {ok, DecodedNa} ->
                    case node_jsonrpc:decode_nas(Hops) of
                        {ok, DecodedHops} ->
                            {ok, #route_entry{oa = DecodedOa,
                                              na = DecodedNa,
                                              path_cost = Pc,
                                              hops = DecodedHops,
                                              psp = base64:decode(Psp)}};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
decode_route_entry(_Re) ->
    {error, einval}.

%%%
%%% exported: encode_nodes
%%%

-spec encode_nodes([#node{}]) -> jsx:json_term().

encode_nodes(Nodes) ->
    [encode_node(Node) || Node <- Nodes].

%%%
%%% exported: encode_node
%%%

-spec encode_node(#node{}) -> jsx:json_term().

encode_node(Node) ->
    [{<<"na">>, node_jsonrpc:encode_na(Node#node.na)},
     {<<"public-key">>, base64:encode(Node#node.public_key)},
     {<<"path-cost">>, encode_path_cost(Node#node.path_cost)},
     {<<"flags">>, Node#node.flags}].

encode_path_cost(undefined) ->
    -1;
encode_path_cost(PathCost) ->
    PathCost.

%%%
%%% exported: decode_nodes
%%%

-spec decode_nodes(jsx:json_term()) -> {'ok', [#node{}]} | {'error', 'einval'}.

decode_nodes(Nodes) ->
    jsonrpc:decode(Nodes, fun decode_node/1).

%%%
%%% exported: decode_node
%%%

-spec decode_node(jsx:json_term()) -> {'ok', #node{}} | {'error', 'einval'}.

decode_node([{<<"na">>, Na},
             {<<"public-key">>, PublicKey},
             {<<"path-cost">>, Pc},
             {<<"flags">>, Flags}])
  when is_binary(PublicKey), is_integer(Pc), is_integer(Flags) ->
    case node_jsonrpc:decode_na(Na) of
        {ok, DecodedNa} ->
            {ok, #node{na = DecodedNa,
                       public_key = base64:decode(PublicKey),
                       path_cost = Pc,
                       flags = Flags}};
        {error, Reason} ->
            {error, Reason}
    end;
decode_node(_Re) ->
    {error, einval}.
