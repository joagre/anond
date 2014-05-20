-module(timelib).

%%% external exports
-export([mk_timestamp/0]).
-export([ugnow/0, gnow/0]).
-export([ugnow_delta/1, gnow_delta/1]).
-export([stop_timer/1, start_timer/2, start_timer/3]).
-export([format_now/0]).

%%% internal exports

%%% include files

%%% constants
-type gsecs() :: non_neg_integer().
-type delta_op() :: 'plus' | 'minus'.
-type delta_unit() :: 'seconds' | 'minutes' | 'hours' | 'days'.
-type timer_ref() :: reference().

%%% records

%%% types

%%%
%%% mk_timestamp
%%%

-spec mk_timestamp() -> timeout().

mk_timestamp() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    trunc((MegaSecs*1000000+Secs)*1000+MicroSecs/1000).

%%%
%%% exported: ugnow
%%%

-spec ugnow() -> gsecs().

ugnow() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

%%%
%%% exported: gnow
%%%

-spec gnow() -> gsecs().

gnow() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).

%%%
%%% exported: ugnow_delta
%%%

-spec ugnow_delta({delta_op(), gsecs() | {N :: integer(), delta_unit()}}) ->
                         gsecs().

ugnow_delta(DeltaSpec) ->
    delta(ugnow(), DeltaSpec).

delta(Now, {minus, Delta}) when is_integer(Delta) ->
    Now-Delta;
delta(Now, {plus, Delta}) when is_integer(Delta) ->
    Now+Delta;
delta(Now, DeltaSpec) ->
    Delta =
        case element(2, DeltaSpec) of
            {GSecs, seconds} -> GSecs;
            {GSecs, minutes} -> 60*GSecs;
            {GSecs, hours} -> 60*60*GSecs;
            {GSecs, days} -> 60*60*24*GSecs
        end,
    case element(1, DeltaSpec) of
	plus -> Now+Delta;
	minus -> Now-Delta
    end.

%%%
%%% exported: gnow_delta
%%%

-spec gnow_delta({delta_op(), gsecs() | {N :: integer(), delta_unit()}}) ->
                        gsecs().

gnow_delta(DeltaSpec) ->
    delta(gnow(), DeltaSpec).

%%%
%%% exported: stop_timer
%%%

-spec stop_timer(timer_ref() | 'undefined') -> boolean().

stop_timer(undefined) ->
    true;
stop_timer(TimerRef) ->
    erlang:cancel_timer(TimerRef).

%%%
%%% exported: start_timer
%%%

start_timer(Timeout, Msg) ->
    start_timer(Timeout, self(), Msg).

-spec start_timer(gsecs() | {N :: integer(), delta_unit()}, pid(),
                  Msg :: any()) ->
                         timer_ref().

start_timer(Timeout, Pid, Msg) when is_integer(Timeout) ->
    erlang:send_after(Timeout, Pid, Msg);
start_timer({Timeout, seconds}, Pid, Msg) ->
    erlang:send_after(Timeout*1000, Pid, Msg);
start_timer({Timeout, minutes}, Pid, Msg) ->
    erlang:send_after(Timeout*1000*60, Pid, Msg);
start_timer({Timeout, hours}, Pid, Msg) ->
    erlang:send_after(Timeout*1000*60*60, Pid, Msg);
start_timer({Timeout, days}, Pid, Msg) ->
    erlang:send_after(Timeout*1000*60*60*24, Pid, Msg).

%%%
%%% exported: format_now
%%%

-spec format_now() -> iolist().

format_now() ->
    format_now(erlang:now()).

format_now(Now) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:now_to_local_time(Now),
    MilliSeconds = element(3, Now) div 1000,
    io_lib:format("~w-~s-~w::~2..0w:~2..0w:~2..0w.~3..0w",
		  [Day, month2string(Month), Year, Hour, Minute, Second,
                   MilliSeconds]).

month2string(1) -> "Jan";
month2string(2) -> "Feb";
month2string(3) -> "Mar";
month2string(4) -> "Apr";
month2string(5) -> "May";
month2string(6) -> "Jun";
month2string(7) -> "Jul";
month2string(8) -> "Aug";
month2string(9) -> "Sep";
month2string(10) -> "Oct";
month2string(11) -> "Nov";
month2string(12) -> "Dec".
