-ifndef(DS_HRL).
-define(DS_HRL, true).

-include_lib("node/include/node.hrl").

-define(DS_JSONRPC_PERMISSION_DENIED, 1).
-define(DS_JSONRPC_UNKNOWN_NODE, 2).
-define(DS_JSONRPC_TOO_FEW_NODES, 3).
-define(DS_JSONRPC_NO_RESERVED_OAS, 4).
-define(DS_JSONRPC_TOO_MANY_NODES, 5).
-define(DS_JSONRPC_INVALID_NODE_DESCRIPTOR, 6).

-record(node_descriptor, {
	  na           :: na(),
          public_key   :: node_crypto:pki_key(),
          last_updated :: timelib:gsecs(),
          flags = 0    :: non_neg_integer()
	 }).

-define(F_DS_NOT_REPUBLISHED, (1 bsl 1)).
-define(F_DS_EXPERIMENTAL_API, (1 bsl 2)).

%% see doc/small_simulation.jpg
-define(NON_RANDOM_NODES,
        [{50001, [50002, 50007]},
         {50002, [50010, 50006]},
         {50003, [50009, 50008]},
         {50004, [50002, 50005]},
         {50005, [50008, 50004]},
         {50006, [50005, 50003]},
         {50007, [50005, 50008]},
         {50008, [50009, 50007]},
         {50009, [50007, 50008]},
         {50010, [50006, 50003]}]).

-endif.
