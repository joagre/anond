-module(common_log_serv).

%%% external exports
-export([start_link/0]).

%%% internal exports
-export([read_config/0]).

%%% include files
-include_lib("util/include/config.hrl").
-include_lib("util/include/log_serv.hrl").

%%% constants

%%% records

%%% types

%%%
%%% exported: start_link
%%%

-spec start_link() -> {'ok', pid()} |
                          {'error',
                           {'not_started', log_serv:error_reson()} |
                           'already_started'}.

start_link() ->
    case log_serv:start_link(fun read_config/0) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {not_started, Reason}} ->
            stderr:print(log_serv:format_error(Reason)),
            {error, {not_started, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

%%
%% Read log configuration
%%

read_config() ->
    {daemon_log_info(), dbg_log_info(), error_log_info()}.

daemon_log_info() ->
    [Enabled] = ?cfg([logs, daemon, enabled]),
    [Tty] = ?cfg([logs, daemon, enabled]),
    [FileEnabled] = ?cfg([logs, daemon, file, enabled]),
    [FilePath] = ?cfg([logs, daemon, file, path]),
    #daemon_log_info{enabled = Enabled,
                     tty = Tty,
                     file = {FileEnabled, FilePath}}.

dbg_log_info() ->
    [Enabled] = ?cfg([logs, dbg, enabled]),
    [Tty] = ?cfg([logs, dbg, enabled]),
    ModuleFilters = ?cfg([logs, dbg, filter]),
    [FileEnabled] = ?cfg([logs, dbg, file, enabled]),
    [FilePath] = ?cfg([logs, dbg, file, path]),
    #dbg_log_info{enabled = Enabled,
                  tty = Tty,
                  module_filter = ModuleFilters,
                  file = {FileEnabled, FilePath}}.

error_log_info() ->
    [Enabled] = ?cfg([logs, error, enabled]),
    [Tty] = ?cfg([logs, error, enabled]),
    [FileEnabled] = ?cfg([logs, error, file, enabled]),
    [FilePath] = ?cfg([logs, error, file, path]),
    #error_log_info{enabled = Enabled,
                    tty = Tty,
                    file = {FileEnabled, FilePath}}.
