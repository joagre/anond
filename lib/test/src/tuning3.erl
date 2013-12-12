-module(tuning3).
-compile(export_all).

start80_95() ->
    start_tunnel(<<"tun0">>, {10,0,0,1}, {192,168,1,80}, 5000,
                 {192,168,1,95}, 5001).

start95_80() ->
    start_tunnel(<<"tun0">>, {10,0,0,2}, {192,168,1,95}, 5001,
                 {192,168,1,80}, 5000).

start_tunnel(TunDevice, TunIp, SrcIp, SrcPort, DestIp, DestPort) ->
    spawn(?MODULE, init_tunnel,
          [TunDevice, TunIp, SrcIp, SrcPort, DestIp, DestPort]).

init_tunnel(TunDevice, TunIp, SrcIp, SrcPort, DestIp, DestPort) ->
    register(?MODULE, self()),
    %% Initialize tun
    {ok, TunPid} = tuncer:create(TunDevice, [tun, no_pi, {active, true}]),
    ok = tuncer:up(TunPid, TunIp),
    ok = setup_routing(TunDevice, TunIp, {255,255,255,0}),
    %% Intialize udp tunnel
    {ok, Socket} = gen_udp:open(SrcPort, [binary, {ip, SrcIp}, {active, true}]),
    %% setup routing
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

setup_routing(TunDevice, TunIp, Netmask) ->
    ok = eval_cmd("ifconfig ~s ~s netmask ~s",
                  [binary_to_list(TunDevice), addr(TunIp), addr(Netmask)]),
    ok = eval_cmd("ip route del ~s table local", [addr(TunIp)]).

eval_cmd(Format, Args) ->
    Cmd = lists:flatten(io_lib:format(Format++" 2>&1", Args)),
    io:format("~s~n", [Cmd]),
    case os:cmd(Cmd) of
        "" ->
            ok;
        Reason ->
            {error, Reason}
    end.

addr({A, B, C, D}) ->
    [integer_to_list(A), ".", integer_to_list(B), ".", integer_to_list(C), ".",
     integer_to_list(D)].
