-ifndef(CONFIG_HRL).
-define(CONFIG_HRL, true).

-define(config(JsonPath), config_json_serv:lookup(JsonPath)).
-define(configd(JsonPath, DefaultJsonValue),
        config_json_serv:lookup(JsonPath, DefaultJsonValue)).

-endif.
