-module(tuning).
-export([test/0]).

test() ->
    spawn(fun() ->
                  start_tunnel(
                    <<"tun0">>, {10,0,0,1}, {255,255,255,0}, "wlan0:0",
                    {192,168,0,34}, 8004, {192,168,0,35}, 8005, true)
          end),
    spawn(fun() ->
                  start_tunnel(
                    <<"tun1">>, {10,0,0,2}, {255,255,255,0}, "wlan0:1",
                    {192,168,0,35}, 8005, {192,168,0,34}, 8004, false)
          end).

start_tunnel(TunDevice, TunIp, TunNetmask, UdpSrcDevice, UdpSrcIp, UdpSrcPort,
             UdpDestIp, UdpDestPort, IsGateway) ->
    %% Initialize tun
    {ok, TunPid} = tuncer:create(TunDevice, [tun, no_pi, {active, true}]),
    ok = tuncer:up(TunPid, TunIp),
    ok = setup_ip(TunDevice, TunIp, TunNetmask, IsGateway),
    %% Intialize udp tunnel
    ok = setup_nic_alias(UdpSrcDevice, UdpSrcIp),
    {ok, UdpSocket} =
        gen_udp:open(UdpSrcPort, [binary, {ip, UdpSrcIp}, {active, true}]),
    tunnel(TunDevice, TunIp, UdpSrcIp, UdpSrcPort, UdpDestIp, UdpDestPort,
           TunPid, UdpSocket).

tunnel(TunDevice, TunIp, UdpSrcIp, UdpSrcPort, UdpDestIp, UdpDestPort,
       TunPid, UdpSocket) ->
    receive
        {tuntap, TunPid, Packet} ->
            io:format(
              "An IP packet of size ~w arrived on ~p (~w) and has been "
              "tunneled to ~w (port ~w) using UDP.~n",
              [size(Packet), TunDevice, TunIp, UdpDestIp, UdpDestPort]),
            ok = gen_udp:send(UdpSocket, UdpDestIp, UdpDestPort, Packet),
            tunnel(TunDevice, TunIp, UdpSrcIp, UdpSrcPort, UdpDestIp,
                   UdpDestPort, TunPid, UdpSocket);
        {udp, UdpSocket, UdpDestIp, UdpDestPort, Packet} ->
            io:format(
              "An UDP packet received from ~w (port ~w) and its payload "
              "(IP packet) of size ~w has been put on ~p (~w).~n",
              [UdpDestIp, UdpDestPort, size(Packet), TunDevice, TunIp]),
            ok = tuncer:send(TunPid, Packet),
            tunnel(TunDevice, TunIp, UdpSrcIp, UdpSrcPort, UdpDestIp,
                   UdpDestPort, TunPid, UdpSocket);
        UnknownMessage ->
            io:format(
              "~w got an unknown message: ~p~n", [self(), UnknownMessage])
    end.

setup_ip(Device, Ip, Netmask, true) ->
    case eval_cmd("ifconfig ~s ~s netmask ~s",
                  [binary_to_list(Device), string_addr(Ip),
                   string_addr(Netmask)]) of
        ok -> eval_cmd("ip route del ~s table local", [string_addr(Ip)]);
        {error, Reason} -> {error, Reason}
    end;
setup_ip(_Device, Ip, _Netmask, false) ->
    eval_cmd("ip route del ~s table local", [string_addr(Ip)]).

eval_cmd(Format, Args) ->
    Cmd = lists:flatten(io_lib:format(Format++" 2>&1", Args)),
    io:format("~s~n", [Cmd]),
    case os:cmd(Cmd) of
        "" -> ok;
        Reason -> {error, Reason}
    end.

string_addr({A, B, C, D}) ->
    [integer_to_list(A), ".", integer_to_list(B), ".", integer_to_list(C), ".",
     integer_to_list(D)].

setup_nic_alias(Device, Ip) ->
    eval_cmd("ifconfig ~s ~s up", [Device, string_addr(Ip)]).
