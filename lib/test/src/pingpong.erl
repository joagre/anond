-module(pingpong).
-compile(export_all).

start() ->
    spawn(fun() ->
                  echo({192,168,1,80}, 9002, {192,168,1,80}, 9003, false)
          end),
    timer:sleep(1000),
    spawn(fun() ->
                  echo({192,168,1,80}, 9003, {192,168,1,80}, 9002, true)
          end).

echo(IpAddress, Port, RemoteIpAddress, RemotePort, Ping) ->
    {ok, Socket} = gen_udp:open(Port, [list, {ip, IpAddress}, {active, true}]),
    if
        Ping ->
            self() ! ping;
        true ->
            ok
    end,
    loop(IpAddress, Port, RemoteIpAddress, RemotePort, Socket).

loop(IpAddress, Port, RemoteIpAddress, RemotePort,  Socket) ->
    receive
        stop ->
            ok;
        ping ->
            ok = gen_udp:send(Socket, RemoteIpAddress, RemotePort, "ping"),
            ok = gen_udp:send(Socket, RemoteIpAddress, RemotePort, "ping"),
            ok = gen_udp:send(Socket, RemoteIpAddress, RemotePort, "ping"),
            ok = gen_udp:send(Socket, RemoteIpAddress, RemotePort, "ping"),
            loop(IpAddress, Port, RemoteIpAddress, RemotePort, Socket);
        {udp, Socket, RemoteIpAddress, RemotePort, Packet} ->
            io:format("Incoming from ~p:~w -> ~p~n",
                      [RemoteIpAddress, RemotePort, Packet]),
            timer:sleep(5000),
            ok = gen_udp:send(Socket, RemoteIpAddress, RemotePort, Packet),
            loop(IpAddress, Port, RemoteIpAddress, RemotePort, Socket);
        UnknownMessage ->
            io:format("Unknown message: ~p~n", [UnknownMessage])
    end.
