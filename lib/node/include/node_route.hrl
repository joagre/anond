-ifndef(NODE_ROUTE_HRL).
-define(NODE_ROUTE_HRL, true).

-include_lib("node/include/node.hrl").

-type routing_db() :: ets:tid().

-record(routing_entry, {
	  oa        :: oa() | '_',
	  ip        :: ip() | '_',
	  path_cost :: path_cost() | '_',
	  flags = 0 :: integer() | '_',
          hops = [] :: [ip()] | '_',
          psp       :: binary() | '_' % patrik: should be binary?
	 }).

-define(F_RE_UPDATED, (1 bsl 0)).

-type node_db() :: ets:tid().

-record(node, {
	  ip         :: ip() | '_' | '$1',
	  public_key :: public_key:rsa_public_key() | '_',
	  path_cost  :: path_cost() | '_',
	  flags = 0  :: integer() | '_'
	 }).

-define(F_NODE_UPDATED, (1 bsl 0)).
-define(F_NODE_IS_INCOMING_PEER, (1 bsl 1)).

-endif.
