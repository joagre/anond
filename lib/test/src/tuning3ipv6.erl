-module(tuning3ipv6).
-compile(export_all).

%% http://www.simpledns.com/private-ipv6.aspx
%% fdef:89d0:60c5:b36e:0:0:0:0
%% {65007,35280,24773,45934,7792,47266,13446, 1}
%% FDEF:89D0:60C5:B36E:1E70:B8A2:3486:1

start80() ->
    start_tunnel(<<"tun0">>,
                 {65007,35280,24773,45934,7792,47266,13446,1},
                 {65007,35280,24773,45934,7792,47266,13446,2},
                 {192,168,1,80}, 5000,
                 {192,168,1,95}, 5001).

start95() ->
    start_tunnel(<<"tun0">>,
                 {65007,35280,24773,45934,7792,47266,13446,2},
                 {65007,35280,24773,45934,7792,47266,13446,1},
                 {192,168,1,95}, 5001,
                 {192,168,1,80}, 5000).

start_tunnel(TunDevice, TunIp, RemoteTunIp, SrcIp, SrcPort, DestIp, DestPort) ->
    spawn(?MODULE, init_tunnel,
          [TunDevice, TunIp, RemoteTunIp, SrcIp, SrcPort, DestIp, DestPort]).

init_tunnel(TunDevice, TunIp, RemoteTunIp, SrcIp, SrcPort, DestIp, DestPort) ->
    register(?MODULE, self()),
    %% initialize tun
    {ok, TunPid} = tuncer:create(TunDevice, [tun, no_pi, {active, true}]),
    ok = tuncer:up(TunPid, TunIp),
    %% intialize udp tunnel
    {ok, Socket} = gen_udp:open(SrcPort, [binary, {ip, SrcIp}, {active, true}]),
    pingpong:start(TunIp, 9000, RemoteTunIp, 9000),
    tunnel(TunDevice, TunIp, SrcIp, SrcPort, DestIp, DestPort, TunPid, Socket).

tunnel(TunDevice, TunIp, SrcIp, SrcPort, DestIp, DestPort, TunPid, Socket) ->
    receive
        {tuntap, TunPid, Packet} ->
            io:format(
              "An IP packet of size ~w arrived on ~p (~w) and has been "
              "tunneled to ~w (port ~w) using UDP.~n",
              [size(Packet), TunDevice, TunIp, DestIp, DestPort]),
            ok = gen_udp:send(Socket, DestIp, DestPort, Packet),
            tunnel(TunDevice, TunIp, SrcIp, SrcPort, DestIp, DestPort, TunPid,
                   Socket);
        {udp, Socket, DestIp, DestPort, Packet} ->
            io:format(
              "An UDP packet received from ~w (port ~w) and its payload "
              "(IP packet) of size ~w has been put on ~p (~w).~n",
              [DestIp, DestPort, size(Packet), TunDevice, TunIp]),
            ok = tuncer:send(TunPid, Packet),
            tunnel(TunDevice, TunIp, SrcIp, SrcPort, DestIp, DestPort, TunPid,
                   Socket);
        UnknownMessage ->
            io:format(
              "~w got an unknown message: ~p~n", [self(), UnknownMessage])
    end.
