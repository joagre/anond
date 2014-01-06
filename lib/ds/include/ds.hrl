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

%% see doc/small_simulation.jpg
-define(NON_RANDOM_PEERS,
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
