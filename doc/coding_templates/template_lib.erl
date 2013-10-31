-module(template_lib).

%%% external exports
-export([foo/0, foo/1]).

%%% internal exports

%%% include files

%%% constants
-define(BAZ, #data{bar = 42}).

%%% records
-record(data, {
	  bar = 0 :: integer(),
	  baz     :: zap()
	 }).

%%% types
-type zap() :: #data{}.

%%%
%%% exported: foo
%%%

-spec foo(any()) -> {ok, any()}.

foo() ->
    foo(undefined).

foo(Type) ->
    {ok, Type}.

%%%
%%% major partition (may consist of minor partitions)
%%%

%%% minor partition

%%% minor partition

%%%
%%% major partition (may consist of minor partitions)
%%%
