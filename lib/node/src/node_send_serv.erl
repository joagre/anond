-module(node_send_serv).

%%% external exports
-export([start_link/4, stop/1, stop/2]).
-export([send/2]).

%%% internal exports
-export([init/5]).

%%% include files
-include_lib("node/include/node.hrl").
-include_lib("node/include/node_path_cost.hrl").
-include_lib("node/include/node_recv_send.hrl").
-include_lib("node/include/node_route.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants

%%% records
-record(state, {
	  parent  :: pid(),
          peer_na :: na(),
          socket  :: gen_udp:socket()
	 }).

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link(na(), na(), supervisor:sup_ref(), supervisor:sup_ref()) ->
                        {'ok', pid()}.

start_link(Na, PeerNa, NodeRouteServ, NodeRecvServ) ->
    Args = [self(), Na, PeerNa, NodeRouteServ, NodeRecvServ],
    Pid = proc_lib:spawn_link(?MODULE, init, Args),
    receive
	{Pid, started} ->
	    {ok, Pid};
	{Pid, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: send
%%%

-spec send(pid() | 'undefined',
           binary() | {'ip_packet', oa(), binary()} |
           #route_entry{} | #echo_request{}) ->
                  'ok'.

send(NodeSendServ, Data) ->
    NodeSendServ ! Data,
    ok.

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

init(Parent, Na, PeerNa, NodeRouteServ, NodeRecvServ) ->
    process_flag(trap_exit, true),
    ok = node_route_serv:handshake(NodeRouteServ, {?MODULE, Na, self()}),
    {ok, Socket} = node_recv_serv:handshake(NodeRecvServ, ?MODULE),
    Parent ! {self(), started},
    loop(#state{parent = Parent, peer_na = PeerNa, socket = Socket}).

loop(#state{parent = Parent, peer_na = PeerNa, socket = Socket} = S) ->
    receive
        %% forward cell from node_recv_serv
        Cell when is_binary(Cell) ->
            udp_send(PeerNa, Socket, Cell),
            loop(S);
        %% send cell from node_tun_serv
        {ip_packet, {Oa0, Oa1, Oa2, Oa3, Oa4, Oa5, Oa6, Oa7}, Packet} ->
            Length = size(Packet),
            Cell =
                <<?IP_PACKET:4,
                  Oa0:16, Oa1:16, Oa2:16, Oa3:16, Oa4:16, Oa5:16, Oa6:16,
                  Oa7:16,
                  Length:12,
                  Packet/binary>>,
            udp_send(PeerNa, Socket, Cell),
            loop(S);
        %% send route entry from node_route_serv
        #route_entry{oa = {Oa0, Oa1, Oa2, Oa3, Oa4, Oa5, Oa6, Oa7},
                     path_cost = Pc, hops = Hops, psp = Psp} ->
            NumberOfHops = length(Hops),
            BinaryHops =
                ?l2b([<<Na0:8, Na1:8, Na2:8, Na3:8, Port:16>> ||
                         {{Na0, Na1, Na2, Na3}, Port} <- Hops]),
            PspSize = size(Psp),
            Cell =
                <<?ROUTE_ENTRY:4,
                  Oa0:16, Oa1:16, Oa2:16, Oa3:16, Oa4:16, Oa5:16, Oa6:16,
                  Oa7:16,
                  Pc:16,
                  NumberOfHops:4,
                  BinaryHops/binary,
                  PspSize:16,
                  Psp/binary>>,
            udp_send(PeerNa, Socket, Cell),
            loop(S);
        %% send echo request from node_path_cost_serv
        #echo_request{sequence_number = SeqNumber, unique_id = UniqueId,
                      timestamp = Timestamp} ->
            Cell = <<?ECHO_REQUEST:4, SeqNumber:12, UniqueId:16, Timestamp:32>>,
            udp_send(PeerNa, Socket, Cell),
            loop(S);
	{From, stop} ->
	    From ! {self(), ok};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

udp_send({PeerIpAddress, PeerPort}, Socket, Cell) ->
    case gen_udp:send(Socket, PeerIpAddress, PeerPort, Cell) of
        ok ->
            ok;
        {error, Reason} ->
            ?daemon_log("Sending data to ~s:~w failed: ~s",
                        [inet_tools:string_address(PeerIpAddress), PeerPort,
                         inet:format_error(Reason)])
    end.
