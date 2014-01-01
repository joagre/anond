-ifndef(DS_HRL).
-define(DS_HRL, true).

-include_lib("node/include/node.hrl").

-define(DS_JSONRPC_PERMISSION_DENIED, 1).
-define(DS_JSONRPC_UNKNOWN_PEER, 2).
-define(DS_JSONRPC_TOO_FEW_PEERS, 3).
-define(DS_JSONRPC_NO_RESERVED_OAS, 4).

-record(peer, {
	  na           :: na(),
          public_key   :: public_key:rsa_public_key(),
          last_updated :: timelib:gsecs(),
          flags = 0    :: non_neg_integer()
	 }).

-define(F_DS_EXPERIMENTAL, (1 bsl 1)).

-define(NON_RANDOM_PEERS,
        [{1, [2, 7]},
         {2, [10, 6]},
         {3, [9, 8]},
         {4, [2, 5]},
         {5, [8, 4]},
         {6, [5, 3]},
         {7, [5, 8]},
         {8, [9, 7]},
         {9, [7, 8]},
         {10, [6, 3]}]).

-endif.
