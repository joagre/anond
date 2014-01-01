-ifndef(JSONRPC_SERV_HRL).
-define(JSONRPC_SERV_HRL, true).

-define(JSONRPC_PARSE_ERROR, -32700).
-define(JSONRPC_INVALID_REQUEST, -32600).
-define(JSONRPC_METHOD_NOT_FOUND, -32601).
-define(JSONRPC_INVALID_PARAMS, -32602).
-define(JSONRPC_INTERNAL_ERROR, -32603).

-record(json_state, {
          ip_adress :: inet:ip_address(),
          port      :: inet:port_number(),
          abs_path  :: binary(),
          request   :: binary()
         }).

-record(json_error, {
          code      :: integer(),
          message   :: binary(),
          data      :: jsx:json_term()
         }).

-endif.
