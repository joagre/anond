-ifndef(NODE_ROUTE_HRL).
-define(NODE_ROUTE_HRL, true).

-include_lib("node/include/node.hrl").

-type routing_db() :: ets:tid().

-record(routing_entry, {
	  oa :: oa(),
	  ip :: ip(),
	  link_quality :: link_quality(),
	  flags = 0 :: integer(),
          hops = [] :: [ip()]
	 }).

-define(F_RE_UPDATED, (1 bsl 0)).

-type node_db() :: ets:tid().

-record(node, {
	  ip :: ip(),
	  public_key  :: public_key:rsa_public_key(),
	  link_quality :: link_quality(),
	  flags = 0 :: integer()
	 }).

-define(F_NODE_UPDATED, (1 bsl 0)).
-define(F_NODE_IS_INCOMING_PEER, (1 bsl 1)).

-endif.
