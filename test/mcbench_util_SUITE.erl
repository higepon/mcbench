%%%-------------------------------------------------------------------
%%% File    : memcached_SUITE.erl
%%% Author  : higepon <higepon@labs.cybozu.co.jp>
%%% Description : Tests for mcbench utilities
%%%
%%% Created :  7 Dec 2009 by higepon <higepon@labs.cybozu.co.jp>
%%%-------------------------------------------------------------------
-module(mcbench_util_SUITE).

-compile(export_all).

-include("ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].


%% Tests start.
test_gen_random_keys(_Config) ->
    N = 10000,
    MaxKey = 100000,
    Keys = mcbench_util:gen_random_keys(N, MaxKey),
    true = is_list(Keys),
    N = length(Keys),
    is_unique_list(Keys),
    lists:all(fun(X) -> X =< MaxKey end, Keys),
    ok.


%% Tests end.
all() ->
    [
     test_gen_random_keys
    ].


%% helper
is_unique_list(List) ->
    GBSet = gb_sets:from_list(List),
    UList = gb_sets:to_list(GBSet),
    length(List) =:= length(UList).
