-ifndef(CONFIG_HRL).
-define(CONFIG_HRL, true).

-define(cfg(Path), config_serv:lookup(Path)).
-define(cfgd(Path, DefaultMultiValue),
        config_serv:lookup(Path, DefaultMultiValue)).

-endif.
