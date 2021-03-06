%%    Copyright (C) 2010 Cybozu Labs, Inc., written by Taro Minowa(Higepon) <higepon@labs.cybozu.co.jp>
%%
%%    Redistribution and use in source and binary forms, with or without
%%    modification, are permitted provided that the following conditions
%%    are met:
%%
%%    1. Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%
%%    2. Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%
%%    3. Neither the name of the authors nor the names of its contributors
%%       may be used to endorse or promote products derived from this
%%       software without specific prior written permission.
%%
%%    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%    TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%%%-------------------------------------------------------------------
%%% File    : mcbench_sup.erl
%%% Author  : Taro Minowa(Higepon) <higepon@labs.cybozu.co.jp>
%%% Description : mcbench supervisor
%%%
%%% Created :  10 Dec 2009 by higepon <higepon@labs.cybozu.co.jp>
%%%-------------------------------------------------------------------

-module(mcbench_sup).
-behaviour(supervisor).

%% API
-export([start_worker/1]).

%% callback functions
-export([init/1]).


start_worker(Args) ->
    supervisor:start_child(
      mcbench_sup,
      {getRandomId(),
       {mcbench_worker, start_link, [Args]},
       temporary, brutal_kill, worker, [mcbench_worker]}).

init(_Args) ->
    crypto:start(),

    %% get application arguments
    {ok, CommandType} = mcbench_app:get_env(command_type, set),
    {ok, Concurrency} = mcbench_app:get_env(concurrency, 10),
    {ok, CommandCount} = mcbench_app:get_env(command_count, 100),
    {ok, BaseDataCount} = mcbench_app:get_env(base_data_count, 0),
    {ok, Host} = mcbench_app:get_env(host, "127.0.0.1"),
    {ok, Port} = mcbench_app:get_env(port, 11211),

    {ok, {{one_for_one, 10, 20},
          [{mcbench_benchmarker, {mcbench_benchmarker, start_link, [[CommandType, Concurrency, CommandCount, BaseDataCount, Host, Port]]},
            transient, brutal_kill, worker, [mcbench_benchmarker]}]}}.

getRandomId() ->
    integer_to_list(crypto:rand_uniform(1, 65536 * 65536)).
