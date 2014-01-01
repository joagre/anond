-module(net_tools).

%%% external exports
-export([string_address/1, tcp_sockets/0]).

%%% internal exports

%%% include files
-include_lib("util/include/shorthand.hrl").

%%% constants

%%% records

%%% types

%%%
%%% exported: string_address
%%%

-spec string_address(inet:ip_address()) -> string().

string_address({A0, A1, A2, A3}) ->
    lists:flatten([?i2l(A0), ".", ?i2l(A1), ".", ?i2l(A2), ".", ?i2l(A3)]);
string_address({A0, A1, A2, A3, A4, A5, A6, A7}) ->
    lists:flatten(
      [integer_to_list(A0, 16), ":", integer_to_list(A1, 16), ":",
       integer_to_list(A2, 16), ":", integer_to_list(A3, 16), ":",
       integer_to_list(A4, 16), ":", integer_to_list(A5, 16), ":",
       integer_to_list(A6, 16), ":", integer_to_list(A7, 16)]).

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
