-module(net_tools).

%%% external exports
-export([string_addresses/1, string_address/1, tcp_sockets/0]).

%%% internal exports

%%% include files
-include_lib("util/include/shorthand.hrl").

%%% constants

%%% records

%%% types

%%%
%%% exported: string_addresses
%%%

-spec string_addresses([{inet:ip4_address(), inet:port_number()}] |
                       [inet:ip_address()]) -> string().

string_addresses([]) ->
    "<no addresses>";
string_addresses([Address]) ->
    string_address(Address);
string_addresses([Address|Rest]) ->
    [string_address(Address), ", "|string_addresses(Rest)].

%%%
%%% exported: string_address
%%%

-spec string_address({inet:ip4_address(), inet:port_number()} |
                     inet:ip_address()) -> string().

string_address({{A, B, C, D}, Port}) ->
    lists:flatten([?i2l(A), ".", ?i2l(B), ".", ?i2l(C), ".", ?i2l(D), ":",
                   ?i2l(Port)]);
string_address({A, B, C, D}) ->
    lists:flatten([?i2l(A), ".", ?i2l(B), ".", ?i2l(C), ".", ?i2l(D)]);
string_address({A, B, C, D, E, F, G, H}) ->
    lists:flatten(
      [?i2l(A, 16), ":", ?i2l(B, 16), ":", ?i2l(C, 16), ":", ?i2l(D, 16), ":",
       ?i2l(E, 16), ":", ?i2l(F, 16), ":", ?i2l(G, 16), ":", ?i2l(H, 16)]).

%%%
%%% exported: tcp_sockets
%%%

-spec tcp_sockets() -> integer().

tcp_sockets() ->
    port_list("tcp_inet").

port_list(Name) ->
    length(lists:filter(
             fun(Port) ->
                     case erlang:port_info(Port, name) of
                         {name, Name} -> true;
                         _ -> false
                     end
             end, erlang:ports())).
