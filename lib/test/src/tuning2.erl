-module(tuning2).
-export([test/0]).
-export([start_tunnel/5]).

test() ->
    IpAddress = {192,168,1,80},
    spawn(?MODULE, start_tunnel,
          [<<"tun0">>, {10,0,0,1}, IpAddress, 9000, 9001]),
    spawn(?MODULE, start_tunnel,
          [<<"tun1">>, {11,0,0,2}, IpAddress, 9001, 9000]).

start_tunnel(TunDevice, TunIpAddress, IpAddress, SrcPort, DestPort) ->
    %% Initialize tun
    {ok, TunPid} = tuncer:create(TunDevice, [tun, no_pi, {active, true}]),
    ok = tuncer:up(TunPid, TunIpAddress),
    %% Intialize udp tunnel
    {ok, UdpSocket} =
        gen_udp:open(SrcPort, [binary, {ip, IpAddress}, {active, true}]),
    tunnel(TunDevice, TunIpAddress, IpAddress, SrcPort, DestPort, TunPid,
           UdpSocket).

tunnel(TunDevice, TunIpAddress, IpAddress, SrcPort, DestPort, TunPid,
       UdpSocket) ->
    receive
        {tuntap, TunPid, Packet} ->
            io:format(
              "An IP packet of size ~w arrived on ~p (~w) and has been "
              "tunneled to ~w (port ~w) using UDP.~n",
              [size(Packet), TunDevice, TunIpAddress, IpAddress, DestPort]),
            ok = gen_udp:send(UdpSocket, IpAddress, DestPort, Packet),
            tunnel(TunDevice, TunIpAddress, IpAddress, SrcPort, DestPort,
                   TunPid, UdpSocket);
        {udp, UdpSocket, IpAddress, DestPort, Packet} ->
            io:format(
              "An UDP packet received from ~w (port ~w) and its payload "
              "(IP packet) of size ~w has been put on ~p (~w).~n",
              [IpAddress, DestPort, size(Packet), TunDevice, TunIpAddress]),
            ok = tuncer:send(TunPid, Packet),
            tunnel(TunDevice, TunIpAddress, IpAddress, SrcPort, DestPort,
                   TunPid, UdpSocket);
        UnknownMessage ->
            io:format(
              "~w got an unknown message: ~p~n", [self(), UnknownMessage])
    end.
