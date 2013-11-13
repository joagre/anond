-ifndef(LOG_SERV_HRL).
-define(LOG_SERV_HRL, true).

-record(daemon_log_info, {
          enabled :: boolean(),
          tty     :: boolean(),
          file    :: {Enabled :: boolean(), Filename :: binary()}
        }).

-record(dbg_log_info, {
          enabled       :: boolean(),
          tty           :: boolean(),
          file          :: {Enabled :: boolean(), Filename :: binary()},
          module_filter :: [{'show', binary()} | {'hide', binary()}]
         }).

-record(error_log_info, {
          enabled :: boolean(),
          tty     :: boolean(),
          file    :: {Enabled :: boolean(), Filename :: binary()}
        }).

-endif.
