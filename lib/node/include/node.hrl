-ifndef(NODE_HRL).
-define(NODE_HRL, true).

-type node_id() :: 0..4294967295.
-type na() :: {inet:ip4_address(), inet:port_number()}.
-type oa() :: inet:ip6_address().
-type path_cost() :: 0..65535.

-endif.
