-ifndef(NODE_ROUTE_HRL).
-define(NODE_ROUTE_HRL, true).

-include_lib("node/include/node.hrl").

-type route_db() :: ets:tid().

-define(NODE_UDPRPC_HOLE_PUNCHED, 1).

-record(route_entry, {
	  oa             :: oa() | '_',
          na             :: na() | '_',
	  path_cost      :: path_cost() | '_',
% patrik
%          path_cost_auth :: node_path_cost:auth() | '_',
          path_cost_auth :: any(),
	  flags = 0      :: integer() | '_',
          hops = []      :: [na()] | '_',
          psp            :: binary() | '_'
	 }).

-define(F_RE_UPDATED, (1 bsl 0)).

-type node_db() :: ets:tid().

-record(node, {
          na             :: na() | '_',
	  public_key     :: binary() | '_',
	  path_cost      :: path_cost() | '_',
	  flags = 0      :: integer() | '_',
          node_send_serv :: pid() | '_'
	 }).

-define(F_NODE_UPDATED, (1 bsl 0)).
-define(F_NODE_IS_INCOMING_NEIGHBOUR, (1 bsl 1)).

-endif.
