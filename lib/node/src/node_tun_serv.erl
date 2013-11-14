-module(node_tun_serv).
-export([test/0]).

-include_lib("util/include/shorthand.hrl").

test() ->
    spawn(fun() ->
                  start_tunnel(<<"tun0">>, {10,0,0,1}, {255,255,255,0},
                               "wlan0:0", {192,168,0,34}, 8004,
                               {192,168,0,35}, 8005)
          end),
    spawn(fun() ->
                  start_tunnel(<<"tun1">>, {10,0,1,1}, {255,255,255,0},
                               "wlan0:1", {192,168,0,35}, 8005,
                               {192,168,0,34}, 8004)
          end).

start_tunnel(OaDevice, Oa, OaNetmask, SrcDevice, SrcIp, SrcPort, DestIp,
             DestPort) ->
    %% Initialize overlay address
    {ok, OaTunPid} = tuncer:create(OaDevice, [tun, no_pi, {active, true}]),
    ok = tuncer:up(OaTunPid, Oa),
    "" = setup_ip(OaDevice, Oa, OaNetmask),
    node_dummy_tcp_serv:start_link(Oa, 10000),
    %% Intialize udp tunnel
    "" = setup_nic_alias(SrcDevice, SrcIp),
    {ok, Socket} = gen_udp:open(SrcPort, [binary, {ip, SrcIp}, {active, true}]),
    tunnel(OaDevice, Oa, SrcIp, SrcPort, DestIp, DestPort, OaTunPid, Socket).

tunnel(OaDevice, Oa, SrcIp, SrcPort, DestIp, DestPort, OaTunPid, Socket) ->
    receive
        {tuntap, OaTunPid, Packet} ->
            ?iof("~w arrived on ~p (~w) and has been forwarded to ~w:~w.~n",
                 [Packet, OaDevice, Oa, DestIp, DestPort]),
            ok = gen_udp:send(Socket, DestIp, DestPort, Packet),
            tunnel(OaDevice, Oa, SrcIp, SrcPort, DestIp, DestPort, OaTunPid,
                   Socket);
        {udp, Socket, DestIp, DestPort, Packet} ->
            ?iof("~w arrived from ~w:~w and has been forwarded to ~p (~w).~n",
                 [Packet, DestIp, DestPort, OaDevice, Oa]),
            ok = tuncer:send(OaTunPid, Packet),
            tunnel(OaDevice, Oa, SrcIp, SrcPort, DestIp, DestPort, OaTunPid,
                   Socket);
        UnknownMessage ->
            ?iof("~w got an unknown message: ~p~n", [self(), UnknownMessage])
    end.

setup_ip(Device, Ip, Netmask) ->
    Cmd =
        lists:flatten(
          io_lib:format(
            "ifconfig ~s ~s netmask ~s 2>&1",
            [binary_to_list(Device), string_addr(Ip), string_addr(Netmask)])),
    io:format("~s~n", [Cmd]),
    os:cmd(Cmd).

string_addr({A, B, C, D}) ->
    [integer_to_list(A), ".", integer_to_list(B), ".", integer_to_list(C), ".",
     integer_to_list(D)].

setup_nic_alias(Device, Ip) ->
    Cmd =
        lists:flatten(
          io_lib:format(
            "ifconfig ~s ~s up 2>&1", [Device, string_addr(Ip)])),
    io:format("~s~n", [Cmd]),
    os:cmd(Cmd).
