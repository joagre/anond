-module(pingpong).
-compile(export_all).

start1_2() ->
    spawn(fun() ->
                  echo({10,0,0,1}, 9005, {10,0,0,2}, 9006)
          end).

start2_1() ->
    spawn(fun() ->
                  echo({10,0,0,2}, 9006, {10,0,0,1}, 9005)
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
