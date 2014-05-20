-module(ds_jsonrpc_conversion).

%%% external exports
-export([encode_nas/1, encode_na/1, decode_nas/1, decode_na/1]).
-export([encode_oas/1, encode_oa/1, decode_oas/1, decode_oa/1]).
-export([encode_network_topology/1]).

%%% internal exports

%%% include files
-include_lib("ds/include/ds.hrl").
-include_lib("node/include/node_route.hrl").
-include_lib("util/include/bits.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants

%%% records

%%% types

%%%
%%% exported: encode_nas
%%%

-spec encode_nas([na()]) -> [binary()].

encode_nas(Nas) ->
    [encode_na(Na) || Na <- Nas].

%%%
%%% exported: encode_na
%%%

-spec encode_na(na()) -> binary().

encode_na({IpAddress, Port}) ->
    ?l2b([net_tools:string_address(IpAddress), ":", ?i2l(Port)]).

%%%
%%% exported: decode_nas
%%%

-spec decode_nas([binary()]) -> {'ok', [na()]} | {'error', 'einval'}.

decode_nas(Nas) ->
    decode(Nas, fun decode_na/1).

decode(List, F) ->
    decode(List, F, []).

decode([], _F, Acc) ->
    {ok, lists:reverse(Acc)};
decode([Entity|Rest], F, Acc) ->
    case F(Entity) of
        {ok, MappedEntity} ->
            decode(Rest, F, [MappedEntity|Acc]);
        {error, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: decode_na
%%%

-spec decode_na(binary()) -> {'ok', na()} | {'error', 'einval'}.

decode_na(Na) ->
    case string:tokens(?b2l(Na), ":") of
        [IpAddressString, PortString] ->
            case inet:parse_address(IpAddressString) of
                {ok, IpAddress} ->
                    case catch ?l2i(PortString) of
                        Port when is_integer(Port) ->
                            {ok, {IpAddress, Port}};
                        _ ->
                            {error, einval}
                    end;
                {error, einval} ->
                    {error, einval}
            end;
        _ ->
            {error, einval}
    end.

%%%
%%% exported: encode_oas
%%%

-spec encode_oas([oa()]) -> [binary()].

encode_oas(Oas) ->
    [encode_oa(Oa) || Oa <- Oas].

%%%
%%% exported: encode_oa
%%%

-spec encode_oa(oa()) -> binary().

encode_oa(Oa) ->
    ?l2b(net_tools:string_address(Oa)).

%%%
%%% exported: decode_oas
%%%

-spec decode_oas([binary()]) -> {'ok', [oa()]} | {'error', 'einval'}.

decode_oas(Oas) ->
    decode(Oas, fun decode_oa/1).

%%%
%%% exported: decode_oa
%%%

-spec decode_oa(binary()) -> {'ok', oa()} | {'error', 'einval'}.

decode_oa(Oa) ->
    inet:parse_address(?b2l(Oa)).

%%%
%%% exported: encode_nodes
%%%

-spec encode_network_topology([{#node_descriptor{}, [#node{}],
                                [#route_entry{}]}]) ->
                                     jsx:json_term().

encode_network_topology([]) ->
    [];
encode_network_topology([{NodeId, Na, Neighbours, Res}|Rest]) ->
    [[{<<"node-id">>, NodeId},
      {<<"na">>, ds_jsonrpc_conversion:encode_na(Na)},
      {<<"neighbours">>, encode_neighbours(Neighbours)},
      {<<"route-entries">>, encode_route_entries(Res)}]|
     encode_network_topology(Rest)].

encode_neighbours(Neighbours) ->
    [encode_neighbour(Neighbour) || Neighbour <- Neighbours].

encode_neighbour(Neighbour) ->
    [{<<"node-id">>, Neighbour#node.node_id},
     {<<"na">>, encode_na(Neighbour#node.na)},
     {<<"path-cost">>, Neighbour#node.path_cost},
     {<<"incoming-neighbour">>,
      ?bit_is_set(Neighbour#node.flags, ?F_NODE_IS_INCOMING_NEIGHBOUR)}].

encode_route_entries(Res) ->
    [[{<<"path-cost">>, PathCost},
      {<<"route">>, Hops}] ||
        #route_entry{path_cost = PathCost, hops = Hops} <- Res,
        Hops /= []].
