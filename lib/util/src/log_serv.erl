-module(log_serv).

%%% external exports
-export([start_link/1]).
-export([daemon_log/5]).
-export([dbg_log/3]).
-export([format_error/1]).

%%% internal exports
-export([init/3]).

%%% include files
-include_lib("util/include/log.hrl").
-include_lib("util/include/log_serv.hrl").
-include_lib("util/include/shorthand.hrl").

%%% constants

%%% records
-record(state, {
          parent               :: pid(),
          tty_available        :: boolean(),
          read_config_callback :: read_config_callback(),
          daemon_log_info      :: #daemon_log_info{},
          daemon_disk_log      :: disk_log:log(),
          dbg_log_info         :: #dbg_log_info{},
          dbg_disk_log         :: disk_log:log(),
          error_log_info       :: #error_log_info{}
        }).

%%% types
-type read_config_callback() ::
        fun(() -> {#daemon_log_info{}, #dbg_log_info{}, #error_log_info{}}).
-type error_reason() :: 'already_started' | disk_log:open_error_rsn().

%%%
%%% exported: start_link
%%%

-spec start_link(read_config_callback()) ->
                        {'ok', pid()} | {'error', error_reason()}.

start_link(ReadConfigCallback) ->
    Args = [self(), ReadConfigCallback, tty_available()],
    Pid = proc_lib:spawn_link(?MODULE, init, Args),
    receive
	{Pid, started} ->
	    {ok, Pid};
	{Pid, Reason} ->
            {error, Reason}
    end.

%%%
%%% exported: daemon_log
%%%

-spec daemon_log(Pid :: pid(), Module :: atom(), Line :: integer(),
                 Format :: string(), Args :: [any()]) -> 'ok'.

daemon_log(Pid, Module, Line, Format, Args) ->
    ?MODULE ! {daemon_log, Pid, Module, Line, Format, Args},
    ok.

%%%
%%% exported: dbg_log
%%%

-spec dbg_log(Module :: atom(), Line :: integer(), term()) -> 'ok'.

dbg_log(Module, Line, Term) ->
    ?MODULE ! {dbg_log, Module, Line, Term},
    ok.

%%%
%%% exported: format_error
%%%

-spec format_error(error_reason()) -> iolist().

format_error(already_started) ->
    "Already started";
format_error(Reason) ->
    disk_log:format_error(Reason).

%%%
%%% server loop
%%%

init(Parent, ReadConfigCallback, TtyAvailable) ->
    process_flag(trap_exit, true),
    case catch register(?MODULE, self()) of
        true ->
            case setup(Parent, ReadConfigCallback, TtyAvailable) of
                {ok, S} ->
                    Parent ! {self(), started},
                    loop(S);
                {error, Reason} ->
                    Parent ! {self(), Reason}
            end;
        _ ->
            Parent ! {self(), already_started}
    end.

setup(Parent, ReadConfigCallback, TtyAvailable) ->
    {DaemonLogInfo, DbgLogInfo, ErrorLogInfo} = ReadConfigCallback(),
    case open_log(DaemonLogInfo) of
        {ok, DaemonDiskLog} ->
            case open_log(DbgLogInfo) of
                {ok, DbgDiskLog} ->
                    ok = config_json_serv:subscribe(),
                    {ok, #state{parent = Parent,
                                tty_available = TtyAvailable,
                                read_config_callback = ReadConfigCallback,
                                daemon_log_info = DaemonLogInfo,
                                daemon_disk_log = DaemonDiskLog,
                                dbg_log_info = DbgLogInfo,
                                dbg_disk_log = DbgDiskLog,
                                error_log_info = ErrorLogInfo}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

loop(#state{parent = Parent,
            tty_available = TtyAvailable,
            read_config_callback = ReadConfigCallback,
            daemon_log_info = DaemonLogInfo,
            daemon_disk_log = DaemonDiskLog,
            dbg_log_info = DbgLogInfo,
            dbg_disk_log = DbgDiskLog,
            error_log_info = _ErrorLogInfo} = S) ->
    receive
        {daemon_log, Pid, Module, Line, Format, Args} ->
            write_to_daemon_log(TtyAvailable, DaemonLogInfo, DaemonDiskLog,
                                Pid, Module, Format, Args),
            write_to_dbg_log(false, DbgLogInfo, DbgDiskLog, Module, Line,
                             {daemon_log, Format, Args}),
            loop(S);
        {dbg_log, Module, Line, Term} ->
            write_to_dbg_log(TtyAvailable, DbgLogInfo, DbgDiskLog,
                             Module, Line, Term),
            loop(S);
        config_updated ->
            {NewDaemonLogInfo, NewDbgLogInfo, _NewErrorLogInfo} =
                ReadConfigCallback(),
            NewDaemonDiskLog =
                reopen_log(TtyAvailable, NewDaemonLogInfo, DaemonDiskLog,
                           NewDaemonLogInfo, DaemonDiskLog,
                           #daemon_log_info.file),
            NewDbgDiskLog =
                reopen_log(TtyAvailable, NewDaemonLogInfo, DaemonDiskLog,
                           NewDbgLogInfo, DbgDiskLog, #dbg_log_info.file),
            loop(S#state{daemon_log_info = NewDaemonLogInfo,
                         daemon_disk_log = NewDaemonDiskLog,
                         dbg_log_info = NewDbgLogInfo,
                         dbg_disk_log = NewDbgDiskLog});
        {'EXIT', Parent, shutdown} ->
            exit(shutdown);
        UnknownMessage ->
	    ?error_log({unknown_message, UnknownMessage}),
	    loop(S)
    end.

tty_available() ->
    case init:get_argument('detached') of
        {ok, [[]]} ->
            false;
        error ->
            true
    end.

%%% (re)open and close logs

open_log(#daemon_log_info{enabled = true, file = {true, Path}}) ->
    disk_log:open([{name, daemon_log}, {file, ?b2l(Path)}, {format, external}]);
open_log(#dbg_log_info{enabled = true, file = {true, Path}}) ->
    disk_log:open([{name, dbg_log}, {file, ?b2l(Path)}, {format, external}]);
open_log(_LogInfo) ->
    {ok, undefined}.

reopen_log(TtyAvailable, DaemonLogInfo, DaemonDiskLog, LogInfo, DiskLog,
           FileField) ->
    close_log(DiskLog),
    case open_log(LogInfo) of
        {ok, DiskLog} when DiskLog /= undefined ->
            {true, Path} = element(FileField, LogInfo),
            write_to_daemon_log(TtyAvailable, DaemonLogInfo, DaemonDiskLog,
                                self(), ?MODULE, "~s: reopened", [Path]),
            DiskLog;
        {ok, undefined} ->
            undefined;
        {error, DiskLogReason} ->
            ?error_log(DiskLogReason),
            undefined
    end.

close_log(undefined) -> ok;
close_log(Log) -> disk_log:close(Log).

%%% daemon log

write_to_daemon_log(true, #daemon_log_info{
                      enabled = true,
                      tty = Tty,
                      file = {FileEnabled, _Path},
                      show_module_filters = ShowModuleFilters,
                      hide_module_filters = HideModuleFilters},
                    DaemonDiskLog, Pid, Module, Format, Args)
  when Tty == true; FileEnabled == true ->
    case show_modules(?a2b(Module), ShowModuleFilters, HideModuleFilters) of
        true ->
            %% this is too costly
            case erlang:process_info(Pid, registered_name) of
                [] ->
                    String =
                        io_lib:format("~w: ~w: "++Format, [Module, Pid|Args]);
                undefined ->
                    String =
                        io_lib:format("~w: ~w: "++Format, [Module, Pid|Args]);
                {registered_name, Module} ->
                    String = io_lib:format("~w: "++Format, [Module|Args]);
                {registered_name, Name} ->
                    String =
                        io_lib:format("~w: ~w"++Format, [Module, Name|Args])
            end,
            write_to_daemon_log(DaemonDiskLog, String),
            write_to_daemon_tty(Tty, String);
        false ->
            skip
    end;
write_to_daemon_log(_TtyAvailable, _DaemonLogInfo, _DaemonDiskLog, _Pid,
                    _Module, _Format, _Args) ->
    skip.

write_to_daemon_log(undefined, _String) -> ok;
write_to_daemon_log(Log, String) ->
    Now = timelib:ugnow(),
    disk_log:balog(Log, ["== ", ?i2l(Now), " ", timelib:format_now(), $\n,
                         String, $\n]).

write_to_daemon_tty(false, _String) ->
    ok;
write_to_daemon_tty(true, String) ->
    io:format("~s", [lists:flatten(["<DAEMON> ", String, $\n])]).

%%% dbg log

write_to_dbg_log(true, #dbg_log_info{enabled = true,
                                     tty = Tty,
                                     file = {FileEnabled, _Path},
                                     show_module_filters = ShowModuleFilters,
                                     hide_module_filters = HideModuleFilters},
                 DaemonDiskLog, Module, Line, Term)
  when Tty == true; FileEnabled == true ->
    case show_modules(?a2b(Module), ShowModuleFilters, HideModuleFilters) of
        true ->
            String = io_lib:format("~w: ~w: ~p", [Module, Line, Term]),
            write_to_dbg_log(DaemonDiskLog, String),
            write_to_dbg_tty(Tty, String);
        false ->
            skip
    end;
write_to_dbg_log(_TtyAvailable, _DbgLogInfo, _DbgDiskLog, _Module, _Line,
                 _Term) ->
    skip.

show_modules(Module, ShowModuleFilters, HideModuleFilters) ->
    module_member(Module, ShowModuleFilters) and
        not(module_member(Module, HideModuleFilters)).

module_member(_Module, []) ->
    false;
module_member(_Module, [<<"*">>|_]) ->
    true;
module_member(Module, [Module|_]) ->
    true;
module_member(Module, [_|Rest]) ->
    module_member(Module, Rest).

write_to_dbg_log(undefined, _String) ->
    ok;
write_to_dbg_log(Log, String) ->
    disk_log:balog(Log, [String, $\n]).

write_to_dbg_tty(false, _String) ->
    ok;
write_to_dbg_tty(true, String) ->
    io:format("~s", [["<DBG> ", String, $\n]]).
