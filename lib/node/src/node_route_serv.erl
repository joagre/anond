-module(node_route_serv).

%%% external exports
-export([start_link/2, stop/1, stop/2]).
-export([handshake/2]).
-export([get_my_node_id/1]).
-export([lookup_node/2]).
-export([insert_node/2]).
-export([get_route_entries/1, new_route_entry/2]).
-export([get_nodes/1]).
-export([enable_recalc/1, disable_recalc/1, recalc/1]).
-export([update_path_cost/3]).

%%% internal exports
-export([init/3]).

%%% include files

-include_lib("ds/include/ds.hrl").
-include_lib("node/include/node.hrl").
-include_lib("node/include/node_route.hrl").
-include_lib("util/include/bits.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/log.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants
-define(INITIAL_REFRESH_NEIGHBOURS_INTERVAL, 4*1000). % 4 seconds
-define(INITIAL_RECALC_INTERVAL, 8*1000). % 8 seconds

%%% records
-record(state, {
          parent                      :: pid(),
          system_db                   :: dets:tid(),
          node_db                     :: node_db(),
          route_db                    :: route_db(),
          psp_db                      :: node_psp:psp_db(),
          ds_id                       :: ds_id(),
          my_node_id                  :: node_id(),
          shared_key                  :: binary(),
          ttl                         :: non_neg_integer(),
          neighbour_node_ids = []     :: [node_id()],
          node_instance_sup           :: supervisor:child(),
          node_recv_serv              :: pid(),
          node_path_cost_serv         :: pid(),
          %% anond.conf parameters
          mode                        :: common_config_json_serv:mode(),
          my_na                       :: na(),
          logging                     :: boolean(),
          experimental_api            :: boolean(),
          db_directory                :: binary(),
          db_clear_on_start           :: boolean(),
          directory_server            :: {inet:ip4_address(),
                                          inet:port_number()},
          my_oa                       :: oa(),
	  public_key                  :: binary(),
	  secret_key                  :: binary(),
          number_of_neighbours        :: non_neg_integer(),
          refresh_neighbours_interval :: non_neg_integer(),
          recalc_interval             :: non_neg_integer(),
          auto_recalc                 :: boolean()
	 }).

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link(na(), supervisor:sup_ref()) ->
                        {'ok', pid()} |
                        {'error', 'system_node_db_not_available'}.

start_link(MyNa, NodeInstanceSup) ->
    Args = [self(), MyNa, NodeInstanceSup],
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
%%% exported: handshake
%%%

-spec handshake(pid(),
                {'node_path_cost_serv', pid()} |
                'node_tun_serv') ->
                       'ok' | {'ok', node_db(), route_db()}.

handshake(Pid, node_tun_serv) ->
    serv:call(Pid, {handshake, node_tun_serv});
handshake(Pid, {node_path_cost_serv, NodePathCostServ}) ->
    serv:call(Pid, {handshake, {node_path_cost_serv, NodePathCostServ}}).

%%%
%%% exported:get_my_node_id
%%%

-spec get_my_node_id(pid()) -> node_id().

get_my_node_id(Pid) ->
    serv:call(Pid, get_my_node_id).

%%%
%%% exported: lookup_node
%%%

-spec lookup_node(pid(), node_id()) -> [#node{}].

lookup_node(Pid, NodeId) ->
    serv:call(Pid, {lookup_node, NodeId}).

%%%
%%% exported: insert_node
%%%

-spec insert_node(pid(), #node{}) -> 'ok'.

insert_node(Pid, Node) ->
    Pid ! {insert_node, Node},
    ok.

%%%
%%% exported: get_route_entries
%%%

-spec get_route_entries(pid()) -> {'ok', [#route_entry{}]}.

get_route_entries(Pid) ->
    serv:call(Pid, get_route_entries).

%%%
%%% exported: route_entry
%%%

-spec new_route_entry(pid(), #route_entry{}) -> 'ok'.

new_route_entry(Pid, Re) ->
    Pid ! {new_route_entry, Re},
    ok.

%%%
%%% exported: get_nodes
%%%

-spec get_nodes(pid()) -> {'ok', [#node{}]}.

get_nodes(Pid) ->
    serv:call(Pid, get_nodes).

%%%
%%% exported: enable_recalc
%%%

-spec enable_recalc(pid()) -> 'ok'.

enable_recalc(Pid) ->
    Pid ! enable_recalc,
    ok.

%%%
%%% exported: disable_recalc
%%%

-spec disable_recalc(pid()) -> 'ok'.

disable_recalc(Pid) ->
    Pid ! disable_recalc,
    ok.

%%%
%%% exported: recalc
%%%

-spec recalc(pid()) -> 'ok'.

recalc(Pid) ->
    Pid ! recalc,
    ok.

%%%
%%% exported: update_path_cost
%%%

-spec update_path_cost(pid(), node_id(), path_cost()) -> 'ok'.

update_path_cost(Pid, NeighbourNodeId, Pc) ->
    Pid ! {update_path_cost, NeighbourNodeId, Pc},
    ok.

%%%
%%% server loop
%%%

init(Parent, MyNa, NodeInstanceSup) ->
    process_flag(trap_exit, true),
    ok = config_json_serv:subscribe(),
    {A1, A2, A3} = erlang:now(),
    random:seed({A1, A2, A3}),
    S = read_config(#state{my_na = MyNa}),
    SystemDbFilename =
        filename:join([S#state.db_directory,
                       "system"++net_tools:string_address(MyNa)++".db"]),

    if
        S#state.db_clear_on_start ->
            file:delete(SystemDbFilename);
        true ->
            ok
    end,
    case dets:open_file(SystemDbFilename, [{file, SystemDbFilename}]) of
        {ok, SystemDb} ->
            {ok, NodeDb} = node_route:create_node_db(),
            {ok, RouteDb} = node_route:create_route_db(),
            PspDb = node_psp:init(),
            MyNodeId = read_my_node_id(SystemDb),
            self() ! bootstrap,
            Parent ! {self(), started},
            %% Note: The supervisor will not be available until all its children
            %% have been started, i.e. calls to
            %% node_instance_sup:lookup_child/2 must be delayed until now
            {ok, NodeRecvServ} =
                node_instance_sup:lookup_child(NodeInstanceSup, node_recv_serv),
            ok = node_recv_serv:handshake(NodeRecvServ,
                                          {?MODULE, NodeDb, RouteDb, self()}),
            ok = log_serv:toggle_logging(self(), S#state.logging),
            loop(S#state{
                   parent = Parent, system_db = SystemDb, node_db = NodeDb,
                   route_db = RouteDb, psp_db = PspDb, my_node_id = MyNodeId,
                   node_instance_sup = NodeInstanceSup,
                   node_recv_serv = NodeRecvServ});
        {error, Reason} ->
            ?daemon_log("~s: ~p", [SystemDbFilename, Reason]),
            Parent ! {self(), {system_db_not_available, Reason}}
    end.

loop(#state{parent = Parent,
            system_db = SystemDb,
            node_db = NodeDb,
	    route_db = RouteDb,
            psp_db = PspDb,
            ds_id = _DsId,
            my_node_id = MyNodeId,
            shared_key = _SharedKey,
            ttl = _TTL,
            neighbour_node_ids = NeighbourNodeIds,
            node_instance_sup = NodeInstanceSup,
            node_recv_serv = NodeRecvServ,
            node_path_cost_serv = NodePathCostServ,
            mode = Mode,
            my_na = {MyIpAddress, _} = MyNa,
            logging = _Logging,
            experimental_api = _ExperimentalApi,
            db_directory = _DbDirectory,
            db_clear_on_start = _DbClearOnStart,
            directory_server = DsIpAddressPort,
	    my_oa = MyOa,
	    public_key = PublicKey,
	    secret_key = SecretKey,
            number_of_neighbours = NumberOfNeighbours,
            refresh_neighbours_interval = RefreshNeighboursInterval,
            recalc_interval = RecalcInterval,
            auto_recalc = AutoRecalc} = S) ->
    receive
        config_updated ->
            ?daemon_log("Node ~w (~s) starts to update its configuration",
                        [MyNodeId, net_tools:string_address(MyNa)]),
            ?MODULE ! {republish_self, false},
            loop(read_config(S));
        bootstrap ->
            case ds_jsonrpc_client:publish_node(
                   MyNodeId, MyIpAddress, DsIpAddressPort, SecretKey,
                   MyNa, PublicKey) of
                {ok, NewDsId, NewMyNodeId, NewTTL, NewSharedKey} ->
                    ?daemon_log("Node ~s published itself as node id ~w",
                                [net_tools:string_address(MyNa), NewMyNodeId]),
                    set_my_node_id(SystemDb, NewMyNodeId),
                    ok = node_recv_serv:ds_register(
                           NodeRecvServ, NewMyNodeId, NewDsId, NewSharedKey),
                    case ds_jsonrpc_client:reserve_oa(
                           NewMyNodeId, MyIpAddress, DsIpAddressPort,
                           SecretKey, MyOa) of
                        ok ->
                            ?daemon_log(
                               "Node ~w (~s) reserved overlay address ~s",
                               [NewMyNodeId, net_tools:string_address(MyNa),
                                net_tools:string_address(MyOa)]),
                            Psp = node_psp:new(PspDb),
                            Re =
                                #route_entry{oa = MyOa, node_id = NewMyNodeId,
                                             path_cost = 0, psp = Psp},
                            got_new =
                                node_route:update_route_entry(RouteDb, Re),
                            RandomRefreshNeighboursInterval =
                                random:uniform(
                                  ?INITIAL_REFRESH_NEIGHBOURS_INTERVAL),
                            timelib:start_timer(
                              RandomRefreshNeighboursInterval,
                              refresh_neighbours),
                            timelib:start_timer(trunc(NewTTL/2),
                                                {republish_self, true}),
                            if
                                S#state.auto_recalc ->
                                    RandomRecalcInterval =
                                        ?INITIAL_REFRESH_NEIGHBOURS_INTERVAL+
                                        random:uniform(
                                          ?INITIAL_RECALC_INTERVAL),
                                    timelib:start_timer(
                                      RandomRecalcInterval, recalc),
                                    loop(S#state{ds_id = NewDsId,
                                                 my_node_id = NewMyNodeId,
                                                 shared_key = NewSharedKey,
                                                 ttl = NewTTL});
                                true ->
                                    loop(S#state{ds_id = NewDsId,
                                                 my_node_id = NewMyNodeId,
                                                 shared_key = NewSharedKey,
                                                 ttl = NewTTL})
                            end;
                        {error, Reason} ->
                            ?error_log(Reason),
                            ?daemon_log(
                               "Node ~w (~s) could not reserve overlay "
                               "address ~s and will retry again in 5 seconds",
                               [NewMyNodeId, net_tools:string_address(MyNa),
                                net_tools:string_address(MyOa)]),
                            timelib:start_timer({5, seconds}, bootstrap),
                            loop(S#state{ds_id = NewDsId,
                                         my_node_id = NewMyNodeId,
                                         shared_key = NewSharedKey,
                                         ttl = NewTTL})
                    end;
                {error, Reason} ->
                    ?error_log(Reason),
                    ?daemon_log(
                       "Node ~s could not publish itself and will retry again "
                       "in 5 seconds", [net_tools:string_address(MyNa)]),
                    timelib:start_timer({5, seconds}, bootstrap),
                    loop(S)
            end;
        {republish_self, RestartTimer} ->
            case ds_jsonrpc_client:publish_node(
                   MyNodeId, MyIpAddress, DsIpAddressPort, SecretKey,
                   MyNa, PublicKey) of
                {ok, NewDsId, NewMyNodeId, NewTTL, NewSharedKey} ->
                    ?daemon_log("Node ~w (~s) republished itself",
                                [NewMyNodeId, net_tools:string_address(MyNa)]),
                    ok = node_recv_serv:ds_register(
                           NodeRecvServ, NewMyNodeId, NewDsId, NewSharedKey),
                    if
                        RestartTimer ->
                            timelib:start_timer(trunc(NewTTL/2), {republish_self, true});
                        true ->
                            ok
                    end,
                    loop(S#state{ds_id = NewDsId, my_node_id = NewMyNodeId,
                                 shared_key = NewSharedKey, ttl = NewTTL});
                {error, Reason}->
                    ?error_log(Reason),
                    ?daemon_log(
                       "Node ~w (~s) could not republish itself and will retry "
                       "in 5 seconds",
                       [MyNodeId, net_tools:string_address(MyNa)]),
                    timelib:start_timer({5, seconds}, {republish_self, RestartTimer}),
                    loop(S)
            end;
        refresh_neighbours ->
            ?daemon_log("Node ~w (~s) starts to refresh its neighbour nodes",
                        [MyNodeId, net_tools:string_address(MyNa)]),
            case refresh_neighbours(
                   NodeDb, RouteDb, PspDb, MyNodeId, NeighbourNodeIds,
                   NodeInstanceSup, NodeRecvServ, NodePathCostServ,
                   DsIpAddressPort, Mode, MyNa, SecretKey, NumberOfNeighbours,
                   AutoRecalc) of
                {ok, NewNeighbourNodeIds} ->
                    timelib:start_timer(
                      RefreshNeighboursInterval, refresh_neighbours),
                    loop(S#state{neighbour_node_ids = NewNeighbourNodeIds});
                {error, Reason} ->
                    ?error_log(Reason),
                    ?daemon_log(
                       "Node ~w (~s) could not refresh neighbours nodes and "
                       "will retry in 5 seconds",
                       [MyNodeId, net_tools:string_address(MyNa)]),
                    timelib:start_timer({5, seconds}, refresh_neighbours),
                    loop(S)
            end;
	{From, stop} ->
	    ok = node_route:delete_node_db(NodeDb),
	    ok = node_route:delete_route_db(RouteDb),
	    From ! {self(), ok};
        {From, {handshake, node_tun_serv}} ->
	    From ! {self(), {ok, NodeDb, RouteDb}},
            loop(S);
        {From, {handshake, {node_path_cost_serv, NewNodePathCostServ}}} ->
	    From ! {self(), {ok, NodeDb, RouteDb}},
            loop(S#state{node_path_cost_serv = NewNodePathCostServ});
        {From, get_my_node_id} ->
            From ! {self(), MyNodeId},
            loop(S);
        {insert_node, #node{node_id = NodeId} = Node} ->
            case node_route:is_member_node(NodeDb, NodeId) of
                true ->
                    ok = node_route:insert_node(NodeDb, Node),
                    loop(S);
                false ->
                    ok = node_route:insert_node(NodeDb, Node),
                    UpdatedNeighbourNodeIds = [NodeId|NeighbourNodeIds],
                    ok = node_path_cost_serv:set_neighbour_node_ids(
                           NodePathCostServ, UpdatedNeighbourNodeIds),
                    loop(S#state{neighbour_node_ids = UpdatedNeighbourNodeIds})
            end;
        {From, {lookup_node, NodeId}} ->
            From ! {self(), node_route:lookup_node(NodeDb, NodeId)},
            loop(S);
	{From, get_route_entries} ->
	    {ok, Res} = node_route:get_route_entries(RouteDb),
	    From ! {self(), {ok, Res}},
	    loop(S);
	{new_route_entry, #route_entry{oa = MyOa}} ->
	    loop(S);
        {new_route_entry, #route_entry{node_id = ViaNodeId} = Re} ->
            case node_route:is_member_node(NodeDb, ViaNodeId) of
                true ->
                    update_route_entry(RouteDb, PspDb, MyNodeId, MyNa, Re),
                    loop(S);
                false ->
                    ?daemon_log(
                       "Node ~w (~s) discarded route entry from ~w",
                       [MyNodeId, net_tools:string_address(MyNa), ViaNodeId]),
                    loop(S)
            end;
	{From, get_nodes} ->
	    {ok, Nodes} = node_route:get_nodes(NodeDb),
	    From ! {self(), {ok, Nodes}},
	    loop(S);
	enable_recalc ->
            self() ! recalc,
            loop(S#state{auto_recalc = true});
	disable_recalc ->
            loop(S#state{auto_recalc = false});
	recalc ->
	    ?daemon_log("Node ~w (~s) recalculates its route table",
                        [MyNodeId, net_tools:string_address(MyNa)]),
            ok = node_route:recalc(MyNodeId, NodeDb, RouteDb, PspDb),
            if
                AutoRecalc ->
                    RandomRecalcInterval = random:uniform(RecalcInterval),
                    timelib:start_timer(RandomRecalcInterval, recalc),
                    loop(S);
                true ->
                    loop(S)
            end;
	{update_path_cost, NeighbourNodeId, Pc}  ->
	    ok = node_route:update_path_cost(NodeDb, NeighbourNodeId, Pc),
	    loop(S);
	{'EXIT', Parent, Reason} ->
	    ok = node_route:delete_node_db(NodeDb),
	    ok = node_route:delete_route_db(RouteDb),
            exit(Reason);
	UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

%%%
%%% init
%%%

read_config(S) ->
    Mode = ?config([mode]),
    NodeInstance = ?config([nodes, {'node-address', S#state.my_na}]),
    read_config(S#state{mode = Mode}, NodeInstance).

read_config(S, []) ->
    S;
read_config(S, [{'logging', Value}|Rest]) ->
    read_config(S#state{logging = Value}, Rest);
read_config(S, [{'directory-server', Value}|Rest]) ->
    read_config(S#state{directory_server = Value}, Rest);
read_config(S, [{'experimental-api', Value}|Rest]) ->
    read_config(S#state{experimental_api = Value}, Rest);
read_config(S, [{'db', [{directory, DbDirectory},
                        {'clear-on-start', DbClearOnStart}]}|Rest]) ->
    read_config(S#state{db_directory = DbDirectory,
                        db_clear_on_start = DbClearOnStart}, Rest);
read_config(S, [{'overlay-addresses', [MyOa]}|Rest]) ->
    read_config(S#state{my_oa = MyOa}, Rest);
read_config(_S, [{'overlay-addresses', _MyOas}|_Rest]) ->
    throw(nyi);
read_config(S, [{'public-key', Value}|Rest]) ->
    Key = cryptolib:read_key_file(Value),
    read_config(S#state{public_key = Key}, Rest);
read_config(S, [{'secret-key', Value}|Rest]) ->
    Key = cryptolib:read_key_file(Value),
    read_config(S#state{secret_key = Key}, Rest);
read_config(S, [{'number-of-neighbours', Value}|Rest]) ->
    read_config(S#state{number_of_neighbours = Value}, Rest);
read_config(S, [{'refresh-neighbours-interval', Value}|Rest]) ->
    read_config(S#state{refresh_neighbours_interval = Value}, Rest);
read_config(S, [{'recalc-interval', Value}|Rest]) ->
    read_config(S#state{recalc_interval = Value}, Rest);
read_config(S, [{'auto-recalc', Value}|Rest]) ->
    read_config(S#state{auto_recalc = Value}, Rest);
read_config(S, [{'path-cost', _}|Rest]) ->
    read_config(S, Rest);
read_config(S, [_|Rest]) ->
    read_config(S, Rest).

read_my_node_id(SystemDb) ->
    case dets:lookup(SystemDb, my_node_id) of
        [{my_node_id, MyNodeId}] ->
            MyNodeId;
        [] ->
            0
    end.

set_my_node_id(SystemDb, MyNodeId) ->
    ok = dets:insert(SystemDb, {my_node_id, MyNodeId}).

%%%
%%% refresh_neighbours
%%%

refresh_neighbours(
  NodeDb, RouteDb, PspDb, MyNodeId, NeighbourNodeIds, NodeInstanceSup,
  NodeRecvServ, NodePathCostServ, DsIpAddressPort, Mode,
  MyNa = {MyIpAddress, _}, SecretKey, NumberOfNeighbours, AutoRecalc) ->
    MyNaStringAddress = net_tools:string_address(MyNa),
    ?daemon_log("Node ~w (~s) known neighbour nodes: ~w",
                [MyNodeId, MyNaStringAddress, NeighbourNodeIds]),
    case ds_jsonrpc_client:still_published_nodes(
           MyNodeId, MyIpAddress, DsIpAddressPort, SecretKey,
           NeighbourNodeIds) of
        {ok, StillPublishedNeighbourNodeIds} ->
            ?daemon_log(
               "Node ~w (~s) published neighbour nodes: ~w",
               [MyNodeId, MyNaStringAddress, StillPublishedNeighbourNodeIds]),
            UnreachableNodes = node_route:unreachable_nodes(NodeDb),
            UnreachableNeighbourNodeIds =
                [Node#node.node_id || Node <- UnreachableNodes],
            ?daemon_log(
               "Node ~w (~s) unreachable neighbour nodes: ~w",
               [MyNodeId, MyNaStringAddress, UnreachableNeighbourNodeIds]),
            LivingNeighbourNodeIds =
                StillPublishedNeighbourNodeIds--UnreachableNeighbourNodeIds,
            ?daemon_log("Node ~w (~s) living neighbour nodes: ~w",
                        [MyNodeId, MyNaStringAddress, LivingNeighbourNodeIds]),
            %% NOTE: In simulation mode we always want to have the
            %% nodes refered to in ?SIMULATED_NEIGHBOUR_NODE_IDS
            %% (ds/include/ds.hrl) as neighbours. This means that we
            %% are not satisfied with the number of neighbours even
            %% though it has been satisified numerically according to
            %% the settings in anond.conf, i.e. we do not count
            %% incoming nodes in simulation mode. We do this to make
            %% the simulation behave the same each time anond is started.
            case Mode of
                normal ->
                    NumberOfMissingNeighbours =
                        NumberOfNeighbours-length(LivingNeighbourNodeIds);
                simulation ->
                    LivingNonIncomingNeighbourNodeIds =
                        lists:foldl(
                          fun(LivingNeighbourNodeId, Acc) ->
                              case node_route:lookup_node(
                                     NodeDb, LivingNeighbourNodeId) of
                                  [#node{flags = Flags}]
                                    when ?bit_is_set(
                                            Flags,
                                            ?F_NODE_IS_INCOMING_NEIGHBOUR) ->
                                      Acc;
                                  [_] ->
                                      [LivingNeighbourNodeId|Acc]
                              end
                          end, [], LivingNeighbourNodeIds),
                    NumberOfMissingNeighbours =
                        NumberOfNeighbours-
                        length(LivingNonIncomingNeighbourNodeIds)
            end,
            if
                NumberOfMissingNeighbours > 0 ->
                    ?daemon_log("Node ~w (~s) needs ~w more neighbour nodes",
                                [MyNodeId, MyNaStringAddress,
                                 NumberOfMissingNeighbours]),
                    get_more_neighbours(
                      NodeDb, RouteDb, PspDb, MyNodeId, NeighbourNodeIds,
                      NodeInstanceSup, NodeRecvServ, NodePathCostServ,
                      DsIpAddressPort, MyNa, SecretKey, AutoRecalc,
                      LivingNeighbourNodeIds, NumberOfMissingNeighbours);
                true ->
                    {ok, LivingNeighbourNodeIds}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_more_neighbours(
  NodeDb, RouteDb, PspDb, MyNodeId, NeighbourNodeIds, NodeInstanceSup,
  NodeRecvServ, NodePathCostServ, DsIpAddressPort, {MyIpAddress, _} = MyNa,
  SecretKey, AutoRecalc, LivingNeighbourNodeIds,
  NumberOfMissingNeighbours) ->
    case ds_jsonrpc_client:get_random_nodes(
           MyNodeId, MyIpAddress, DsIpAddressPort, SecretKey,
           NumberOfMissingNeighbours) of
        {ok, RandomNeighbourNodeIds} ->
            ?daemon_log(
               "Node ~w (~s) got these random neighbours suggested to it: ~w",
               [MyNodeId, net_tools:string_address(MyNa),
                RandomNeighbourNodeIds]),
            purge_neighbours(NodeDb, RouteDb, MyNodeId, NodeInstanceSup,
                             LivingNeighbourNodeIds, NeighbourNodeIds),
            NewNeighbourNodeIds =
                lists:foldl(
                  fun(RandomNeighbourNodeId, Acc) ->
                          case node_route:lookup_node(
                                 NodeDb, RandomNeighbourNodeId) of
                              [] ->
                                  Node = #node{node_id = RandomNeighbourNodeId},
                                  ok = node_route:insert_node(NodeDb, Node),
                                  [RandomNeighbourNodeId|Acc];
                              [#node{flags = Flags} = Node]->
                                  NewNode =
                                      Node#node{
                                        flags =
                                            ?bit_clr(
                                               Flags,
                                               ?F_NODE_IS_INCOMING_NEIGHBOUR)},
                                  ok = node_route:insert_node(NodeDb, NewNode),
                                  Acc
                          end
                  end, [], RandomNeighbourNodeIds),
            case NewNeighbourNodeIds of
                [] ->
                    ok;
                _ ->
                    ok = node_recv_serv:ds_establish_tunnels(
                           NodeRecvServ, NewNeighbourNodeIds)
            end,
            UpdatedNeighbourNodeIds =
                RandomNeighbourNodeIds++LivingNeighbourNodeIds,
            ok = node_path_cost_serv:set_neighbour_node_ids(
                   NodePathCostServ, UpdatedNeighbourNodeIds),
            if
                AutoRecalc ->
                    ok = node_route:recalc(MyNodeId, NodeDb, RouteDb, PspDb),
                    {ok, UpdatedNeighbourNodeIds};
                true ->
                    {ok, UpdatedNeighbourNodeIds}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

purge_neighbours(
  _NodeDb, _RouteDb, _MyNodeId, _NodeInstanceSup, _LivingNeighbourNodeIds,
  []) ->
    ok;
purge_neighbours(
  NodeDb, RouteDb, MyNodeId, NodeInstanceSup, LivingNeighbourNodeIds,
  [NeighbourNodeId|Rest]) ->
    case lists:member(NeighbourNodeId, LivingNeighbourNodeIds) of
        false ->
            ok = node_route:delete_node(NodeDb, NeighbourNodeId),
            ok = node_send_sup:stop_node_send_serv(
                   NodeInstanceSup, NeighbourNodeId),
            ok = node_route:update_path_costs(
                   RouteDb, NeighbourNodeId, ?NODE_UNREACHABLE),
            purge_neighbours(NodeDb, RouteDb, MyNodeId, NodeInstanceSup,
                             LivingNeighbourNodeIds, Rest);
        true ->
            purge_neighbours(
              NodeDb, RouteDb, MyNodeId, NodeInstanceSup,
              lists:delete(NeighbourNodeId, LivingNeighbourNodeIds), Rest)
    end.

%%%
%%% handle incoming #route_entry{}
%%%

update_route_entry(RouteDb, PspDb, MyNodeId, MyNa,
                   #route_entry{oa = DestOa, path_cost = Pc, hops = Hops,
                                psp = Psp} = Re) ->
    PspLoop = node_psp:is_loop(PspDb, Psp),
    HopsLoop = lists:member(MyNodeId, Hops),
    if
        PspLoop == HopsLoop ->
            ok;
        true ->
            ?error_log({psp_loop_mismatch, MyNodeId, PspLoop, HopsLoop, Hops})
    end,
    case HopsLoop of
        true ->
            ?dbg_log({loop_rejected, MyNodeId, Hops});
        false ->
            case node_route:update_route_entry(RouteDb, Re) of
                {updated, #route_entry{path_cost = CurrentPc}} ->
                    ?daemon_log(
                       "Node ~w (~s) updated existing route to ~s via ~w with "
                       "new path cost ~w (~w)",
                       [MyNodeId, net_tools:string_address(MyNa),
                        net_tools:string_address(DestOa), Hops, Pc, CurrentPc]);
                {kept, _CurrentRe} ->
                    ?daemon_log(
                       "Node ~w (~s) kept existing route to ~s via ~w with "
                       "path cost ~w",
                       [MyNodeId, net_tools:string_address(MyNa),
                        net_tools:string_address(DestOa), Hops, Pc]);
                got_new ->
                    ?daemon_log(
                       "Node ~w (~s) got new route to ~s via ~w with path "
                       "cost ~w",
                       [MyNodeId, net_tools:string_address(MyNa),
                        net_tools:string_address(DestOa), Hops, Pc]);
                {got_better,
                 #route_entry{path_cost = CurrentPc, hops = CurrentHops}} ->
                    ?daemon_log(
                       "Node ~w (~s) got better route to ~s via ~w with path "
                       "cost ~w, i.e. replacing route via ~w with path cost ~w",
                       [MyNodeId, net_tools:string_address(MyNa),
                        net_tools:string_address(DestOa), Hops, Pc, CurrentHops,
                        CurrentPc]);
                {got_worse,
                 #route_entry{path_cost = CurrentPc, hops = CurrentHops}} ->
                    ?daemon_log(
                       "Node ~w (~s) got worse route to ~s via ~w with path "
                       "cost ~w, i.e. not replacing route via ~w with path "
                       "cost ~w",
                       [MyNodeId, net_tools:string_address(MyNa),
                        net_tools:string_address(DestOa), Hops, Pc, CurrentHops,
                        CurrentPc])
            end
    end.
