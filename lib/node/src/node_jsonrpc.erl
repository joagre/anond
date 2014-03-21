-module(node_jsonrpc).

%%% external exports
-export([encode_nas/1, encode_na/1, decode_nas/1, decode_na/1]).
-export([encode_oas/1, encode_oa/1, decode_oas/1, decode_oa/1]).

%%% internal exports

%%% include files
-include_lib("node/include/node.hrl").
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
    jsonrpc:decode(Nas, fun decode_na/1).

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
    jsonrpc:decode(Oas, fun decode_oa/1).

%%%
%%% exported: decode_oa
%%%

-spec decode_oa(binary()) -> {'ok', oa()} | {'error', 'einval'}.

decode_oa(Oa) ->
    inet:parse_address(?b2l(Oa)).
