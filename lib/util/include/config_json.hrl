-ifndef(CONFIG_JSON_HRL).
-define(CONFIG_JSON_HRL, true).

-type type_name() ::
        'bool' |
        {'int', integer(), integer() | 'unbounded'} |
        'ipv4address:port' |
        'ipv6address' |
        'base64' |
        'filename' |
        'string'.

-type ip_address_port() :: {inet:ip4_address(), inet:port_number()}.

-type json_value() :: integer() | boolean() | binary() | ip_address_port().

-record(json_type, {
          name    :: type_name(),
          info    :: binary(),
          typical :: json_value(),
          convert :: fun((json_value()) -> json_value())
         }).

-type json_schema() :: [{atom(), #json_type{}}] | [json_schema()].

-type json_name() :: atom() | {atom(), json_value()}.
-type json_path() :: [json_name()].

-endif.
