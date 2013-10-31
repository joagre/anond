-ifndef(SOCKS_HRL).
-define(SOCKS_HRL, true).

%% version
-define(SOCKS_VER, 16#5).

%% authentication methods
-define(SOCKS_NO_AUTHENTICATION_REQUIRED, 16#0).
-define(SOCKS_GSSAPI, 16#1).
-define(SOCKS_USERNAME_PASSWORD, 16#2).
-define(SOCKS_NO_ACCEPTABLE_METHODS, 16#FF).

%% request command
-define(SOCKS_CONNECT, 16#1).
-define(SOCKS_BIND, 16#2).
-define(SOCKS_UDP_ASSOCIATE, 16#3).

%% address type
-define(SOCKS_IP_V4, 16#1).
-define(SOCKS_DOMAINNAME, 16#3).
-define(SOCKS_IP_V6, 16#4).

%% request reply field
-define(SOCKS_SUCCEEDED, 16#0).
-define(SOCKS_GENERAL_SOCKS_SERVER_FAILURE, 16#1).
-define(SOCKS_CONNECTION_NOT_ALLOWED_BY_RULESET, 16#2).
-define(SOCKS_NETWORK_UNREACHABLE, 16#3).
-define(SOCKS_HOST_UNREACHABLE, 16#4).
-define(SOCKS_CONNECTION_REFUSED, 16#5).
-define(SOCKS_TTL_EXPIRED, 16#6).
-define(SOCKS_COMMAND_NOT_SUPPORTED, 16#7).
-define(SOCKS_ADDRESS_TYPE_NOT_SUPPORTED, 16#7).
-define(SOCKS_UNASSIGNED, 16#FF).

-endif.
