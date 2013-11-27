-ifndef(NODE_HRL).
-define(NODE_HRL, true).

-type noa() :: inet:ip6_address(). % should replace oa()
-type oa() :: integer().
-type ip() :: pid(). % should be removed by na()
-type na() :: {inet:ip4_address(), inet:port_number()}.
-type path_cost() :: integer().

-endif.
