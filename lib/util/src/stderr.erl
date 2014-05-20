-module(stderr).

%%% external exports
-export([print/1, print/2, print/4]).

%%% internal exports

%%% include files

%%% constants

%%% records

%%% types

%%%
%%% exported: print
%%%

print(Format) ->
    print(1, false, Format, []).

print(Format, Args) ->
    print(1, false, Format, Args).

-spec print(integer(), boolean(), string(), [any()]) ->
                   'ok' | no_return().

print(ExitStatus, HaltP, Format, Args) ->
    io:format(standard_error, Format++"~n", Args),
    case HaltP of
	true ->
            %% allow data on stderr to be printed
            timer:sleep(1000),
            erlang:halt(ExitStatus);
	_ ->
            ok
    end.
