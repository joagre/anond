-ifndef(LOG_HRL).
-define(LOG_HRL, true).

-define(error_log(Term),
	error_logger:error_report({?MODULE, ?LINE, (Term), 
                                   erlang:get_stacktrace()})).
-define(daemon_log(Format, Args),
        log_serv:daemon_log(self(), ?MODULE, ?LINE, Format, Args)).
-define(dbg_log(_Term), log_serv:dbg_log(?MODULE, ?LINE, (_Term))).

-endif.
