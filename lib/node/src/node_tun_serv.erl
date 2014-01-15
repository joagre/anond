-module(node_tun_serv).

%%% external exports
-export([start_link/2, stop/1, stop/2]).

%%% internal exports
-export([init/3]).

%%% include files
-include_lib("node/include/node.hrl").
-include_lib("node/include/node_route.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/log.hrl").

%%% constants

%%% records

-record(state, {
          parent            :: pid(),
          node_db           :: node_db(),
          route_db          :: route_db(),
          node_recv_serv    :: pid(),
          tun_fd            :: tun_fd(),
          tun_pid           :: pid() | 'undefined',
          %% anond.conf parameters
          create_tun_device :: boolean(),
          na                :: na(),              
          oa                :: oa()}).

%%% types
-type tun_fd() :: integer() | 'undefined'. %% tunctl has no type specifications

%%%
%%% exported: start_link
%%%

-spec start_link(na(), supervisor:sup_ref()) -> {'ok', pid()}.

start_link(Na, NodeInstanceSup) ->
    Args = [self(), Na, NodeInstanceSup],
    Pid = proc_lib:spawn_link(?MODULE, init, Args),
    receive
	{Pid, started} ->
	    {ok, Pid}
    end.

%%%
%%% exported: stop
%%%

-spec stop(pid()) -> 'ok'.

stop(Pid) ->
    stop(Pid, 15000).

-spec stop(pid(), timeout()) -> 'ok'.

stop(Pid, Timeout) ->
    serv:call(Pid, stop, Timeout).

%%%
%%% server loop
%%%

init(Parent, Na, NodeInstanceSup) ->
    process_flag(trap_exit, true),
    ok = config_json_serv:subscribe(),
    S = read_config(#state{parent = Parent, na = Na}),
    Parent ! {self(), started},
    {ok, NodeRouteServ} =
        node_instance_sup:lookup_child(NodeInstanceSup, node_route_serv),
    {ok, NodeRecvServ} =
        node_instance_sup:lookup_child(NodeInstanceSup, node_recv_serv),
    {ok, NodeDb, RouteDb} = node_route_serv:handshake(NodeRouteServ, ?MODULE),
    ok = node_recv_serv:handshake(NodeRecvServ, {?MODULE, S#state.tun_fd}),
    {TunFd, TunPid} = manage_tun_device(S#state.create_tun_device, S#state.oa),
    loop(S#state{node_db = NodeDb, route_db = RouteDb,
                 node_recv_serv = NodeRecvServ, tun_fd = TunFd,
                 tun_pid = TunPid}).

loop(#state{parent = Parent,
            node_db = NodeDb,
            route_db = RouteDb,
            node_recv_serv = NodeRecvServ,
            create_tun_device = _CreateTunDevice,
            tun_fd = _TunFd,
            tun_pid = TunPid,
            oa = Oa} = S) ->
    receive
        config_updated ->
            ?daemon_log("Configuration changed...", []),
            UpdatedS = read_config(S),
            {UpdatedTunFd, UpdatedTunPid} =
                manage_tun_device(UpdatedS#state.create_tun_device,
                                  UpdatedS#state.oa, 
                                  S#state.oa, S#state.tun_fd),
            ok = node_recv_serv:handshake(NodeRecvServ,
                                          {?MODULE, UpdatedTunFd}),
            loop(UpdatedS#state{tun_fd = UpdatedTunFd,
                                tun_pid = UpdatedTunPid});
        {tuntap, TunPid,
         <<_:128,
           Oa0:16, Oa1:16, Oa2:16, Oa3:16, Oa4:16, Oa5:16, Oa6:16, Oa7:16,
           _/binary>>  = Ipv6Packet} ->
            Oa = {Oa0, Oa1, Oa2, Oa3, Oa4, Oa5, Oa6, Oa7},
            send(Oa, NodeDb, RouteDb, Ipv6Packet),
            loop(S);
	{From, stop} ->
	    From ! {self(), ok};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

send(Oa, NodeDb, RouteDb, Packet) ->
    case node_route:lookup_node_send_serv(NodeDb, RouteDb, Oa) of
        {ok, NodeSendServ} ->
            ok = node_send_serv:send(NodeSendServ, {ip_packet, Packet});
        {error, _Reason} ->
            ok
    end.

%%%
%%% init
%%%

read_config(S) ->
    NodeInstance = ?config([nodes, {'node-address', S#state.na}]),
    read_config(S, NodeInstance).

read_config(S, []) ->
    S;
read_config(S, [{'create-tun-device', Value}|Rest]) ->
    read_config(S#state{create_tun_device = Value}, Rest);
read_config(S, [{'overlay-addresses', [Oa]}|Rest]) ->
    read_config(S#state{oa = Oa}, Rest);
read_config(_S, [{'overlay-addresses', _Oa}|_Rest]) ->
    throw(nyi);
read_config(S, [_|Rest]) ->
    read_config(S, Rest).

manage_tun_device(CreateTunDevice, Oa) ->
    manage_tun_device(CreateTunDevice, Oa, undefined, undefined).

manage_tun_device(CreateTunDevice, Oa, CurrentOa, CurrentTunTid) ->
    case toggle_tun_device(CreateTunDevice, Oa, CurrentOa, CurrentTunTid) of
        undefined ->
            {undefined, undefined};
        TunTid ->
            {tuncer:getfd(TunTid), TunTid}
    end.

toggle_tun_device(true, Oa, _CurrentOa, undefined) ->
    create_tun_device(Oa);
toggle_tun_device(true, Oa, Oa, CurrentTunTid)
  when CurrentTunTid /= undefined ->
    CurrentTunTid;
toggle_tun_device(true, Oa, CurrentOa, CurrentTunTid) ->
    remove_tun_device(CurrentOa, CurrentTunTid),
    create_tun_device(Oa);
toggle_tun_device(false, _Oa, _CurrentOa, undefined) ->
    undefined;
toggle_tun_device(false, _Oa, CurrentOa, CurrentTunTid) ->
    remove_tun_device(CurrentOa, CurrentTunTid).

create_tun_device(Oa) ->
    case tuncer:create(<<"anond">>, [tun, no_pi, {active, true}]) of
        {ok, TunPid} ->
            case tuncer:up(TunPid, Oa) of
                ok ->
                    ?daemon_log("Created tun device with ip address ~s",
                                [net_tools:string_address(Oa)]),
                    TunPid;
                {error, Reason} ->
                    tuncer:destroy(TunPid),
                    ?daemon_log("Could not add ip address ~s to tun device: ~s",
                                [net_tools:string_address(Oa),
                                 inet:format_error(Reason)]),
                    undefined
            end;
        {error, Reason} ->
            ?daemon_log("Could not create tun device: ~s",
                        [inet:format_error(Reason)]),
            undefined
    end.

remove_tun_device(Oa, TunPid) ->
    tuncer:down(TunPid),
    case tuncer:destroy(TunPid) of
        ok ->
            undefined;
        {error, Reason} ->
            ?daemon_log("Could not remove tun device: ~s: ~s",
                        [net_tools:string_address(Oa),
                         inet:format_error(Reason)]),
            undefined
    end.
