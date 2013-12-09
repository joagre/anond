-module(common_log_serv).

%%% external exports
-export([start_link/0]).

%%% internal exports
-export([read_config/0]).

%%% include files
-include_lib("util/include/shorthand.hrl").
-include_lib("util/include/config.hrl").
-include_lib("util/include/log_serv.hrl").

%%% constants

%%% records

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link() -> {'ok', pid()} | {'error', log_serv:error_reson()}.

start_link() ->
    case log_serv:start_link(fun read_config/0) of
        {ok, Pid} ->
            {ok, Pid};
        {error, Reason} ->
            stderr:print(log_serv:format_error(Reason)),
            {error, Reason}
    end.

%%
%% Read log configuration
%%

read_config() ->
    {daemon_log_info(), dbg_log_info(), error_log_info()}.

daemon_log_info() ->
    Enabled = ?config([logs, daemon, enabled]),
    Tty = ?config([logs, daemon, tty]),
    FileEnabled = ?config([logs, daemon, file, enabled]),
    FilePath = ?config([logs, daemon, file, path]),
    #daemon_log_info{enabled = Enabled,
                     tty = Tty,
                     file = {FileEnabled, FilePath}}.

dbg_log_info() ->
    Enabled = ?config([logs, dbg, enabled]),
    Tty = ?config([logs, dbg, tty]),
    ShowModuleFilters = ?config([logs, dbg, filter, show]),
    HideModuleFilters = ?config([logs, dbg, filter, hide]),
    FileEnabled = ?config([logs, dbg, file, enabled]),
    FilePath = ?config([logs, dbg, file, path]),
    #dbg_log_info{enabled = Enabled,
                  tty = Tty,
                  show_module_filters = ShowModuleFilters,
                  hide_module_filters = HideModuleFilters,
                  file = {FileEnabled, FilePath}}.

error_log_info() ->
    Enabled = ?config([logs, error, enabled]),
    Tty = ?config([logs, error, tty]),
    FileEnabled = ?config([logs, error, file, enabled]),
    FilePath = ?config([logs, error, file, path]),
    #error_log_info{enabled = Enabled,
                    tty = Tty,
                    file = {FileEnabled, FilePath}}.
