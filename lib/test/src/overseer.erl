-module(overseer).

%%% external exports
-export([get_all_nodes/1, get_nodes/1]).
-export([get_all_route_entries/1, get_route_entries/1]).
-export([enable_recalc_for_all/1, enable_recalc/1,
         disable_recalc_for_all/1, disable_recalc/1]).
-export([recalc_all/1, recalc/1]).

%%% internal exports

%%% include files
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").
-include_lib("node/include/node.hrl").
-include_lib("node/include/node_route.hrl").
-include_lib("ds/include/ds.hrl").

%%% constants

%%% records

%%% types
-type ds_ip_address_port() :: {inet:ip4_address(), inet:port_number()}.

%%%
%%% exported: get_nodes
%%%

-spec get_all_nodes(ds_ip_address_port()) ->
                           {'ok',
                            [{na(),
                              [#node{}] |
                              {'not_available', jsonrpc:error_reason()}}]} |
                           {'error', jsonrpc:error_reason()}.

get_all_nodes(DsIpAddressPort) ->
    case ds_jsonrpc:get_all_nodes(undefined, DsIpAddressPort) of
        {ok, NodeDescriptors} ->
            AllNodes =
                lists:map(
                  fun(#node_descriptor{na = Na}) ->
                          case node_route_jsonrpc:get_nodes(undefined, Na) of
                              {ok, Nodes} ->
                                  {Na, Nodes};
                              {error, Reason} ->
                                  {Na, {not_available, Reason}}
                          end
                  end, NodeDescriptors),
            {ok, AllNodes};
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_nodes(na()) ->
                       {'ok',
                        [{na(),
                          [#node{}] |
                          {'not_available', jsonrpc:error_reason()}}]} |
                       {'error', jsonrpc:error_reason()}.

get_nodes(Na) ->
    node_route_jsonrpc:get_nodes(undefined, Na).

%%%
%%% exported: get_route_entries
%%%

-spec get_all_route_entries(ds_ip_address_port()) ->
                                   {'ok',
                                    [{na(),
                                      [#route_entry{} |
                                       {'not_available',
                                        jsonrpc:error_reason()}]}]} |
                                   {'error', jsonrpc:error_reason()}.

get_all_route_entries(DsIpAddressPort) ->
    case ds_jsonrpc:get_all_nodes(undefined, DsIpAddressPort) of
        {ok, NodeDescriptors} ->
            AllRouteTables =
                lists:map(
                  fun(#node_descriptor{na = Na}) ->
                          case node_route_jsonrpc:get_route_entries(
                                 undefined, Na) of
                              {ok, Res} ->
                                  {Na, Res};
                              {error, Reason} ->
                                  {Na, {not_available, Reason}}
                          end
                  end, NodeDescriptors),
            {ok, AllRouteTables};
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_route_entries(na()) ->
                               {'ok', [{na(),
                                        [#route_entry{} |
                                         {'not_available',
                                          jsonrpc:error_reason()}]}]} |
                               {'error', jsonrpc:error_reason()}.

get_route_entries(Na) ->
    node_route_jsonrpc:get_route_entries(undefined, Na).

%%%
%%% exported: enable_recalc
%%%

-spec enable_recalc_for_all(ds_ip_address_port()) -> 'ok'.

enable_recalc_for_all(DsIpAddressPort) ->
    case ds_jsonrpc:get_all_nodes(undefined, DsIpAddressPort) of
        {ok, NodeDescriptors} ->
            lists:foreach(
              fun(#node_descriptor{na = Na}) ->
                      node_route_jsonrpc:enable_recalc(undefined, Na)
              end, NodeDescriptors),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec enable_recalc(na()) -> 'ok' | {'error', jsonrpc:error_reason()}.

enable_recalc(Na) ->
    node_route_jsonrpc:enable_recalc(undefined, Na).

%%%
%%% exported: disable_recalc
%%%

-spec disable_recalc_for_all(ds_ip_address_port()) -> 'ok'.

disable_recalc_for_all(DsIpAddressPort) ->
    case ds_jsonrpc:get_all_nodes(undefined, DsIpAddressPort) of
        {ok, NodeDescriptors} ->
            lists:foreach(
              fun(#node_descriptor{na = Na}) ->
                      node_route_jsonrpc:disable_recalc(undefined, Na)
              end, NodeDescriptors),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec disable_recalc(na()) -> 'ok' | {'error', jsonrpc:error_reason()}.

disable_recalc(Na) ->
    node_route_jsonrpc:disable_recalc(undefined, Na).

%%%
%%% exported: recalc
%%%

-spec recalc_all(ds_ip_address_port()) -> 'ok'.

recalc_all(DsIpAddressPort) ->
    case ds_jsonrpc:get_all_nodes(undefined, DsIpAddressPort) of
        {ok, NodeDescriptors} ->
            lists:foreach(
              fun(#node_descriptor{na = Na}) ->
                      node_route_jsonrpc:recalc(undefined, Na)
              end, NodeDescriptors),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec recalc(na()) -> 'ok' | {'error', jsonrpc:error_reason()}.

recalc(Na) ->
    node_route_jsonrpc:recalc(undefined, Na).
