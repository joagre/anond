-module(pingpong).
-compile(export_all).

start(IpAddress, Port, RemoteIpAddress, RemotePort) ->
    spawn(fun() ->
                  echo(IpAddress, Port, RemoteIpAddress, RemotePort)
          end).

echo(IpAddress, Port, RemoteIpAddress, RemotePort) ->
    register(?MODULE, self()),
    {ok, Socket} = gen_udp:open(Port, [list, {ip, IpAddress}, {active, true}]),
    loop(IpAddress, Port, RemoteIpAddress, RemotePort, Socket).

loop(IpAddress, Port, RemoteIpAddress, RemotePort,  Socket) ->
    receive
        stop ->
            ok;
        ping ->
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
