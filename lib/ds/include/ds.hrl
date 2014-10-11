-ifndef(DS_HRL).
-define(DS_HRL, true).

-include_lib("node/include/node.hrl").

-type ds_id() :: integer().

%% JSON-RPC error codes
-define(DS_JSONRPC_PERMISSION_DENIED, 1).
-define(DS_JSONRPC_UNKNOWN_NODE, 2).
-define(DS_JSONRPC_TOO_FEW_NODES, 3).
-define(DS_JSONRPC_NO_RESERVED_OAS, 4).
-define(DS_JSONRPC_TOO_MANY_NODES, 5).
-define(DS_JSONRPC_INVALID_NODE_DESCRIPTOR, 6).

%% UDP message types
-define(DS_REGISTER, 16#0).
-define(DS_KEEPALIVE, 16#1).
-define(DS_ESTABLISH_TUNNEL, 16#2).
-define(DS_TUNNEL_ESTABLISHED, 16#3).
-define(DS_NETWORK_TOPOLOGY, 16#4).

-record(node_descriptor, {
          node_id      :: node_id(),
	  na           :: na() | 'undefined',
          shared_key   :: binary() | 'undefined',
          public_key   :: binary(),
          last_updated :: timelib:gsecs() | 'undefined',
          flags = 0    :: non_neg_integer()
	 }).

%% #node_descriptor.flags
-define(F_DS_NOT_REPUBLISHED, (1 bsl 1)).
-define(F_DS_EXPERIMENTAL_API, (1 bsl 2)).

-endif.
