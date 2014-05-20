-ifndef(NODE_RECV_SEND_HRL).
-define(NODE_RECV_SEND, true).

-type message_id() :: 0..16777215. % 24 bits

-define(LARGEST_MESSAGE_ID, 16777215). % 24 bits

%% UDP message types
-define(NODE_REGISTERED, 0).
-define(NODE_ESTABLISH_TUNNEL, 16#1).
-define(NODE_TUNNEL_ESTABLISHED, 16#2).
-define(NODE_GET_NEIGHBOURS, 16#3).
-define(NODE_GET_ROUTE_ENTRIES, 16#4).
-define(NODE_KEEPALIVE, 16#5).
-define(NODE_GET_NETWORK_TOPOLOGY, 16#6).
-define(NODE_CELL_IP_PACKET, 16#7).
-define(NODE_CELL_ROUTE_ENTRY, 16#8).
-define(NODE_CELL_ECHO_REQUEST, 16#9).
-define(NODE_CELL_ECHO_REPLY, 16#A).
-define(NODE_CELL_PADDING, 16#B).

-endif.
