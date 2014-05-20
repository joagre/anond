-module(ds_udp_serv).

%%% external exports
-export([start_link/0, stop/1, stop/2]).
-export([get_network_topology/3]).

%%% internal exports
-export([init/1, udp_receiver/1]).

%%% include files
-include_lib("ds/include/ds.hrl").
-include_lib("node/include/node.hrl").
-include_lib("node/include/node_recv_send.hrl").
-include_lib("node/include/node_route.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants
-define(PURGE_SHARED_KEY_DB, 16#ff).
-define(SHARED_KEY_NOT_OLDER_THAN, {1, minutes}).
-define(PURGE_SHARED_KEY_DB_INTERVAL, {30, seconds}).
-define(PURGE_GET_NETWORK_TOPOLOGY_MESSAGES_INTERVAL, {10, seconds}).

%%% records
-record(get_network_topology_message, {
          caller               :: pid(),
          node_id              :: node_id(),
          na                   :: na(),
          purge_timeout        :: timeout(),
          message_id           :: message_id(),
          fragment_buffer = [] :: [{non_neg_integer(), binary()}]
         }).

-record(state, {
          parent                        :: pid(),
          socket                        :: gen_udp:socket(),
          ds_id                         :: ds_id(),
          shared_key_db                 :: ets:tid(),
          udp_receiver                  :: pid(),
          get_network_topology_messages :: [#get_network_topology_message{}],
          next_message_id = 0           :: message_id(),
          %% anond.conf parameters
          ip_address                    :: inet:ip_address(),
          port                          :: inet:port_number()
         }).

-record(udp_receiver_state, {
          parent        :: pid(),
          socket        :: gen_udp:socket(),
          ds_id         :: node_id(),
          shared_key_db :: ets:tid(),
          %% anond.conf parameters
          ip_address    :: inet:ip_address(),
          port          :: inet:port_number()
	 }).

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link() -> {'ok', pid()} |
                      {'error',
                       {'udp_failure', inet:posix()} |
                       'already_started'}.

start_link() ->
    Args = [self()],
    Pid = proc_lib:spawn_link(?MODULE, init, Args),
    receive
	{Pid, started} ->
	    {ok, Pid};
	{Pid, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: stop
%%%

stop(Pid) ->
    stop(Pid, 15000).

-spec stop(pid(), timeout()) -> 'ok'.

stop(Pid, Timeout) ->
    serv:call(Pid, stop, Timeout).

%%%
%%% exported: get_network_topology
%%%

-spec get_network_topology(pid(), node_id(), timeout()) -> 'ok'.

get_network_topology(Caller, NodeId, Timeout) ->
    ?MODULE ! {get_network_topology, Caller, NodeId, Timeout},
    ok.

%%%
%%% server loop
%%%

init(Parent) ->
    process_flag(trap_exit, true),
    case catch register(?MODULE, self()) of
        true ->
            DsId = ds_serv:get_ds_id(),
            S = read_config(#state{ds_id = DsId,
                                   get_network_topology_messages = []}),
            Options = [binary, {ip, S#state.ip_address}, {active, false}],
            case gen_udp:open(S#state.port, Options) of
                {ok, Socket} ->
                    Parent ! {self(), started},
                    SharedKeyDb = create_shared_key_db(),
                    UdpReceiverS = #udp_receiver_state{
                      parent = self(), socket = Socket, ds_id = DsId,
                      shared_key_db = SharedKeyDb,
                      ip_address = S#state.ip_address, port = S#state.port},
                    UdpReceiver =
                        proc_lib:spawn_link(?MODULE, udp_receiver,
                                            [UdpReceiverS]),
                    timelib:start_timer(
                      ?PURGE_SHARED_KEY_DB_INTERVAL, UdpReceiver,
                      purge_shared_key_db),
                    timelib:start_timer(
                      ?PURGE_GET_NETWORK_TOPOLOGY_MESSAGES_INTERVAL,
                      purge_get_network_topology_messages),
                    loop(S#state{parent = Parent, socket = Socket,
                                 shared_key_db = SharedKeyDb,
                                 udp_receiver = UdpReceiver});
                {error, Reason} ->
                    Parent ! {self(), {udp_failure, Reason}}
            end;
        _ ->
            Parent ! {self(), already_started}
    end.

loop(#state{parent = Parent,
            socket = Socket,
            ds_id = DsId,
            shared_key_db = SharedKeyDb,
            udp_receiver = UdpReceiver,
            get_network_topology_messages = GetNetworkTopologyMessages,
            next_message_id = NextMessageId,
            ip_address = IpAddress,
            port = Port} = S) ->
    receive
        purge_shared_key_db ->
            ok = gen_udp:send(
                   Socket, IpAddress, Port, <<?PURGE_SHARED_KEY_DB:8>>),
            timelib:start_timer(
              ?PURGE_SHARED_KEY_DB_INTERVAL, purge_shared_key_db),
            loop(S);
        {get_network_topology, Caller, NodeId, Timeout} ->
            case ds_serv:lookup_node(NodeId) of
                [#node_descriptor{na = {NaIpAddress, NaPort} = Na,
                                  shared_key = SharedKey}] ->
                    Nonce = salt:crypto_random_bytes(24),
                    RandomBytes = salt:crypto_random_bytes(42),
                    Payload = <<?NODE_GET_NETWORK_TOPOLOGY:8, NextMessageId:24,
                                RandomBytes/binary>>,
                    send_message(
                      DsId, Socket, NaIpAddress, NaPort, Nonce, SharedKey,
                      Payload),
                    IncrementedNextMessageId = next_message_id(NextMessageId),
                    NewGetNetworkTopologyMessages =
                        [#get_network_topology_message{
                            caller = Caller, node_id = NodeId, na = Na,
                            purge_timeout = timelib:ugnow()+Timeout,
                            message_id = NextMessageId}|
                         GetNetworkTopologyMessages],
                    loop(S#state{
                           get_network_topology_messages =
                               NewGetNetworkTopologyMessages,
                           next_message_id = IncrementedNextMessageId});
                [] ->
                    ?error_log({unknown_node, NodeId}),
                    loop(S)
            end;
        purge_get_network_topology_messages ->
            NewGetNetworkTopologyMessages =
                lists:foldl(
                  fun(#get_network_topology_message{
                         purge_timeout = PurgeTimeout} =
                          GetNetworkTopologyMessage, Acc) ->
                          Delta = timelib:ugnow_delta({minus, PurgeTimeout}),
                          if
                              Delta > 0 ->
                                  [];
                              true ->
                                  [GetNetworkTopologyMessage|Acc]
                          end
                  end, [], GetNetworkTopologyMessages),
            timelib:start_timer(?PURGE_GET_NETWORK_TOPOLOGY_MESSAGES_INTERVAL,
                                purge_get_network_topology_messages),
            loop(S#state{get_network_topology_messages =
                             NewGetNetworkTopologyMessages});
        {ds_network_topology, MessageId, FragmentCounter, Fragment,
         LastFragmentBit} ->
            case lists:keysearch(
                   MessageId, #get_network_topology_message.message_id,
                   GetNetworkTopologyMessages) of
                false ->
                    ?dbg_log({stale_ds_network_topology, MessageId}),
                    loop(S);
                {value, #get_network_topology_message{
                   caller = Caller, node_id = NodeId, na = Na,
                   fragment_buffer = FragmentBuffer} =
                     GetNetworkTopologyMessage}
                  when LastFragmentBit == 1 ->
                    case missing_fragments(
                           {FragmentCounter, Fragment}, FragmentBuffer) of
                        {no, NewFragmentBuffer} ->
                            NewGetNetworkTopologyMessages =
                                lists:keydelete(
                                  MessageId,
                                  #get_network_topology_message.message_id,
                                  GetNetworkTopologyMessages),
                            case decode_network_topology(NewFragmentBuffer) of
                                {ok, Neighbours, Res} ->
                                    Caller ! {network_topology, NodeId, Na,
                                              Neighbours, Res},
                                    loop(S#state{
                                           get_network_topology_messages =
                                               NewGetNetworkTopologyMessages});
                                {error, Reason} ->
                                    ?error_log(Reason),
                                    loop(S#state{
                                           get_network_topology_messages =
                                               NewGetNetworkTopologyMessages})
                            end;
                        {yes, NewFragmentBuffer} ->
                            NewGetNetworkTopologyMessages =
                                update_fragment_buffer(
                                  MessageId, GetNetworkTopologyMessages,
                                  GetNetworkTopologyMessage, NewFragmentBuffer),
                            loop(S#state{get_network_topology_messages =
                                             NewGetNetworkTopologyMessages})
                    end;
                {value, #get_network_topology_message{
                   fragment_buffer = FragmentBuffer} =
                     GetNetworkTopologyMessage} ->
                    NewFragmentBuffer =
                        [{FragmentCounter, Fragment}|FragmentBuffer],
                    NewGetNetworkTopologyMessages =
                        update_fragment_buffer(
                          MessageId, GetNetworkTopologyMessages,
                          GetNetworkTopologyMessage, NewFragmentBuffer),
                    loop(S#state{get_network_topology_messages =
                                     NewGetNetworkTopologyMessages})
            end;
	{From, stop} ->
            gen_udp:close(Socket),
            delete_shared_key_db(SharedKeyDb),
	    From ! {self(), ok};
        {'EXIT', Parent, Reason} ->
            gen_udp:close(Socket),
            delete_shared_key_db(SharedKeyDb),
            exit(Reason);
        {'EXIT', UdpReceiver, Reason} ->
            gen_udp:close(Socket),
            delete_shared_key_db(SharedKeyDb),
            exit(Reason);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

%%%
%%% init
%%%

read_config(S) ->
    {IpAddress, Port} = ?config(['directory-server', listen]),
    S#state{ip_address = IpAddress, port = Port}.

%%%
%%% get_network_topology
%%%

send_message(DsId, Socket, IpAddress, Port, Nonce, SharedKey, Payload) ->
    case catch salt:crypto_stream_xor(Payload, Nonce, SharedKey) of
        {'EXIT', Reason} ->
            ?error_log(Reason);
        EncryptedPayload ->
            Message = <<DsId:32, Nonce/binary, EncryptedPayload/binary>>,
            ok = gen_udp:send(Socket, IpAddress, Port, Message)
    end.

next_message_id(?LARGEST_MESSAGE_ID) ->
    0;
next_message_id(MessageId) ->
    MessageId+1.

missing_fragments({FragmentCounter, Fragment}, FragmentBuffer) ->
    NewFragmentBuffer =
        lists:keysort(1, [{FragmentCounter, Fragment}|FragmentBuffer]),
    {any_missing_fragments(NewFragmentBuffer), NewFragmentBuffer}.

any_missing_fragments(FragmentBuffer) ->
    any_missing_fragments(FragmentBuffer, 0).

any_missing_fragments([], _FragmentCounter) ->
    no;
any_missing_fragments([{FragmentCounter, _Fragment}|Rest], FragmentCounter) ->
    any_missing_fragments(Rest, FragmentCounter+1);
any_missing_fragments([{_BadFragmentCounter, _Fragment}|_Rest],
                      _FragmentCounter) ->
    yes.

decode_network_topology(FragmentBuffer) when is_list(FragmentBuffer) ->
    NetworkTopology =
        ?l2b([Fragment || {_FragmentCounter, Fragment} <- FragmentBuffer]),
    decode_network_topology(NetworkTopology);
decode_network_topology(<<NumberOfNeighbours:32, Rest/binary>>) ->
    case decode_neighbours(NumberOfNeighbours, Rest) of
        {ok, Neighbours,
         <<NumberOfRes:32, RemainingFragmentBuffer/binary>>} ->
            case decode_route_entries(NumberOfRes, RemainingFragmentBuffer) of
                {ok, Res} ->
                    {ok, Neighbours, Res};
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, Neighbours, <<>>} ->
            {ok, Neighbours, []};
        {error, Reason} ->
            {error, Reason}
    end.

decode_neighbours(NumberOfNeighbours, RemainingFragmentBuffer) ->
    decode_neighbours(NumberOfNeighbours, RemainingFragmentBuffer, []).

decode_neighbours(0, RemainingFragmentBuffer, Acc) ->
    {ok, lists:reverse(Acc), RemainingFragmentBuffer};
decode_neighbours(
  NumberOfNeighbours,
  <<NodeId:32, A:8, B:8, C:8, D:8, Port:16, Pc:16, Flags:8, Rest/binary>>,
  Acc) ->
    Neighbour = #node{node_id = NodeId, na = {{A, B, C, D}, Port},
                      path_cost = Pc, flags = Flags},
    decode_neighbours(NumberOfNeighbours-1, Rest, [Neighbour|Acc]);
decode_neighbours(_NumberOfNeighbours, BadFragmentBuffer, _Acc) ->
    {error, {bad_fragment_buffer, BadFragmentBuffer}}.

decode_route_entries(NumberOfRes, RemainingFragmentBuffer) ->
    decode_route_entries(NumberOfRes, RemainingFragmentBuffer, []).

decode_route_entries(0, <<>>, Acc) ->
    {ok, lists:reverse(Acc)};
decode_route_entries(0, BadFragmentBuffer, _Acc) ->
    {error, {bad_fragment_buffer, BadFragmentBuffer}};
decode_route_entries(NumberOfRes,
                     <<Pc:16, HopsSize:16, Hops:HopsSize/binary, Rest/binary>>,
                     Acc) ->
    case node_recv_serv:decode_hops(Hops) of
        {ok, DecodedHops} ->
            Re = #route_entry{path_cost = Pc, hops = DecodedHops},
            decode_route_entries(NumberOfRes-1, Rest, [Re|Acc]);
        {error, Reason} ->
            {error, Reason}
    end;
decode_route_entries(_NumberOfRes, BadFragmentBuffer, _Acc) ->
    {error, {bad_fragment_buffer, BadFragmentBuffer}}.

update_fragment_buffer(MessageId, GetNetworkTopologyMessages,
                       GetNetworkTopologyMessage, NewFragmentBuffer) ->
    lists:keyreplace(
      MessageId, #get_network_topology_message.message_id,
      GetNetworkTopologyMessages,
      GetNetworkTopologyMessage#get_network_topology_message{
        fragment_buffer = NewFragmentBuffer}).

%%%
%%% UDP receiver loop
%%%

udp_receiver(#udp_receiver_state{
                parent = Parent,
                socket = Socket,
                ds_id = DsId,
                shared_key_db = SharedKeyDb,
                ip_address = MyIpAddress,
                port = MyPort} = S) ->
    case gen_udp:recv(Socket, 0) of
        {ok, {MyIpAddress, MyPort, <<?PURGE_SHARED_KEY_DB:8>>}} ->
            purge_shared_key_db(SharedKeyDb),
            udp_receiver(S);
        {ok, {IpAddress, Port,
              <<NodeId:32, Nonce:24/binary, Payload/binary>>}} ->
            case ds_serv:lookup_node(NodeId) of
                [#node_descriptor{shared_key = SharedKey}] ->
                    case catch salt:crypto_stream_xor(
                                 Payload, Nonce, SharedKey) of
                        {'EXIT', Reason} ->
                            ?error_log(Reason),
                            udp_receiver(S);
                        DecryptedPayload ->
                            handle_payload(
                              Parent, Socket, DsId, SharedKeyDb, IpAddress,
                              Port, NodeId, Nonce, SharedKey, DecryptedPayload),
                            udp_receiver(S)
                    end;
                [] ->
                    udp_receiver(S)
            end;
        {error, Reason} ->
            ?error_log(Reason),
            udp_receiver(S)
    end.

handle_payload(
  _Parent, Socket, DsId, _SharedKeyDb, IpAddress, Port, NodeId, Nonce,
  SharedKey,
  <<?DS_REGISTER:8, MessageId:24, _RandomBytes:42/binary>>) ->
    ?daemon_log("Node ~w registered itself as ~s",
                [NodeId, net_tools:string_address({IpAddress, Port})]),
    ok = ds_serv:update_node(NodeId, {IpAddress, Port}),
    RandomBytes = salt:crypto_random_bytes(42),
    Payload = <<?NODE_REGISTERED:8, MessageId:24, RandomBytes/binary>>,
    send_message(DsId, Socket, IpAddress, Port, Nonce, SharedKey, Payload);
handle_payload(
  _Parent, _Socket, _DsId, _SharedKeyDb, _IpAddress, _Port, _NodeId, _Nonce,
  _SharedKey,
  <<?DS_KEEPALIVE:8, _RandomBytes:45/binary>>) ->
    ok;
handle_payload(
  _Parent, Socket, DsId, SharedKeyDb, {A, B, C, D} = IpAddress, Port, NodeId,
  Nonce, _SharedKey,
  <<?DS_ESTABLISH_TUNNEL:8, MessageId:24, DestNodeId:32,
    _RandomBytes:38/binary>>) ->
    ?daemon_log(
       "Node ~w (~s) tries to establish a tunnel with node ~w",
       [NodeId, net_tools:string_address({IpAddress, Port}), DestNodeId]),
    case ds_serv:lookup_node(DestNodeId) of
        [#node_descriptor{na = {DestIpAddress, DestPort},
                          shared_key = DestSharedKey}] ->
            NewTunnelSharedKey =
                new_shared_key(SharedKeyDb, MessageId, NodeId, DestNodeId),
            Payload = <<?NODE_ESTABLISH_TUNNEL:8, MessageId:24, NodeId:32,
                        A:8, B:8, C:8, D:8, Port:16,
                        NewTunnelSharedKey/binary>>,
            send_message(DsId, Socket, DestIpAddress, DestPort, Nonce,
                         DestSharedKey, Payload);
        [] ->
            ok
    end;
handle_payload(
  _Parent, Socket, DsId, SharedKeyDb, {A, B, C, D} = IpAddress, Port, NodeId,
  Nonce, _SharedKey,
  <<?DS_TUNNEL_ESTABLISHED:8, MessageId:24, SrcNodeId:32,
    _RandomBytes:38/binary>>) ->
    ?daemon_log(
       "Node ~w (~s) accepted a request to establish a tunnel with node ~w",
       [NodeId, net_tools:string_address({IpAddress, Port}), SrcNodeId]),
    case ds_serv:lookup_node(SrcNodeId) of
        [#node_descriptor{na = {SrcIpAddress, SrcPort},
                          shared_key = SrcSharedKey}] ->
            case lookup_shared_key(SharedKeyDb, MessageId, SrcNodeId, NodeId) of
                {ok, NewTunnelSharedKey} ->
                    Payload = <<?NODE_TUNNEL_ESTABLISHED:8, MessageId:24,
                                NodeId:32, A:8, B:8, C:8, D:8, Port:16,
                                NewTunnelSharedKey/binary>>,
                    delete_shared_key(SharedKeyDb, MessageId, SrcNodeId,
                                      NodeId),
                    send_message(DsId, Socket, SrcIpAddress, SrcPort, Nonce,
                                 SrcSharedKey, Payload);
                not_found ->
                    ?error_log(
                       {unknown_shared_key, {MessageId, SrcNodeId, NodeId}})
            end;
        [] ->
            ?error_log({unknown_node, NodeId})
    end;
handle_payload(
  Parent, _Socket, _DsId, _SharedKeyDb, IpAddress, Port, NodeId, _Nonce,
  _SharedKey,
  <<?DS_NETWORK_TOPOLOGY:8, MessageId:24, FragmentCounter:16, FragmentSize:15,
    LastFragmentBit:1, Fragment:FragmentSize/binary>>) ->
    ?daemon_log(
       "Node ~w (~s) replied with its network topology",
       [NodeId, net_tools:string_address({IpAddress, Port})]),
    Parent ! {ds_network_topology, MessageId, FragmentCounter, Fragment,
              LastFragmentBit};
handle_payload(
  _Parent, _Socket, _DsId, _SharedKeyDb, _IpAddress, _Port, _NodeId, _Nonce,
  _SharedKey, UnknownPayload) ->
    ?error_log({unknown_payload, UnknownPayload}).

%%%
%%% shared key db
%%%

create_shared_key_db() ->
    ets:new(shared_key_db, [public]).

delete_shared_key_db(SharedKeyDb) ->
    ets:delete(SharedKeyDb).

new_shared_key(SharedKeyDb, MessageId, SrcNodeId, DestNodeId) ->
    SharedKey = salt:crypto_random_bytes(32),
    true = ets:insert(SharedKeyDb,
                      {{MessageId, SrcNodeId, DestNodeId},
                       {SharedKey, timelib:ugnow()}}),
    SharedKey.

delete_shared_key(SharedKeyDb, MessageId, SrcNodeId, NodeId) ->
    true = ets:delete(SharedKeyDb,  {MessageId, SrcNodeId, NodeId}).

lookup_shared_key(SharedKeyDb, MessageId, SrcNodeId, NodeId) ->
    case ets:lookup(SharedKeyDb,  {MessageId, SrcNodeId, NodeId}) of
        [{_, {SharedKey, _Inserted}}] ->
            {ok, SharedKey};
        [] ->
            not_found
    end.

purge_shared_key_db(SharedKeyDb) ->
    NotOlderThan = timelib:ugnow_delta({minus, ?SHARED_KEY_NOT_OLDER_THAN}),
    MatchSpec = [{{'_', {'_', '$1'}},
                  [{'<', '$1', NotOlderThan}],
                  [true]}],
    NumberOfPurgedSharedKeys = ets:select_delete(SharedKeyDb, MatchSpec),
    ?daemon_log("~w stale shared keys have been purged",
                [NumberOfPurgedSharedKeys]).
