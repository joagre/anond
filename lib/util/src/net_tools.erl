-module(net_tools).

%%% external exports
-export([tcp_sockets/0]).

%%% internal exports

%%% include files

%%% constants

%%% records

%%% types

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
