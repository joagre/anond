-ifndef(NODE_ROUTE_HRL).
-define(NODE_ROUTE_HRL, true).

-include_lib("node/include/node.hrl").

-type route_db() :: ets:tid().

-define(NODE_UNREACHABLE, 16#ffff).

-record(route_entry, {
	  oa                            :: oa() | '_',
          node_id                       :: node_id() | '_',
	  path_cost = ?NODE_UNREACHABLE :: path_cost() | '_',
% patrik
%         path_cost_auth                :: node_path_cost:auth() | '_',
          path_cost_auth                :: any(),
	  flags = 0                     :: non_neg_integer() | '_',
          hops = []                     :: [node_id()] | '_',
          psp                           :: binary() | '_'
	 }).

%% #route_entry.flags
-define(F_RE_UPDATED, (1 bsl 0)).

-type node_db() :: ets:tid().

-record(node, {
          node_id                       :: node_id() | '_',
          na                            :: na() | 'undefined' | '_',
	  shared_key                    :: binary() | 'undefined' | '_',
	  path_cost = ?NODE_UNREACHABLE :: path_cost() | '_',
	  flags = 0                     :: non_neg_integer() | '_',
          node_send_serv                :: pid() | 'undefined' | '_'
	 }).

%% #node.flags
-define(F_NODE_UPDATED, (1 bsl 0)).
-define(F_NODE_IS_INCOMING_NEIGHBOUR, (1 bsl 1)).

-endif.
