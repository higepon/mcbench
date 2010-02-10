%%    Copyright (c) 2009  Taro Minowa(Higepon) <higepon@users.sourceforge.jp>
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
%%% File    : mcbench_benchmarker.erl
%%% Author  : Taro Minowa(Higepon) <higepon@users.sourceforge.jp>
%%% Description : mcbench benchmarker
%%%
%%% Created :  10 Dec 2009 by higepon <higepon@users.sourceforge.jp>
%%%-------------------------------------------------------------------

-module(mcbench_benchmarker).

-export([start_link/1, benchmark/6]).

start_link(Args)->
    Pid = spawn_link(?MODULE, benchmark, Args),
    {ok, Pid}.


benchmark(CommandType, Concurrency, CommandCount, BaseDataCount, Host, Port) when is_integer(Concurrency) andalso is_integer(CommandCount) ->
    Commands = CommandCount * Concurrency,
    io:format("~n==== mcbench started ====~n"),
    io:format("    Server: ~s:~p~n", [Host, Port]),
    io:format("    ~p threads x ~p = ~p commands.~n", [Concurrency, CommandCount, Commands]),
    if BaseDataCount > 0 ->
            io:format("    Inserting base data ~p ... ", [BaseDataCount]),
            insert_base_data(Host, Port, BaseDataCount),
            io:format("done~n");
       true -> []
    end,
    MaxKey = 1000000,
    {ok, Workers} = start_workers(CommandType, Concurrency, CommandCount, MaxKey, Host, Port, []),
    ok = wait_workers(Concurrency, make_keys_done),
    io:format("    Sending ~p Commands ... ", [CommandType]),
    lists:foreach(fun(Worker) ->
                          go = Worker ! go
                          end,
                 Workers),

    Start = erlang:now(),
    ok = wait_workers(Concurrency, done),
    io:format("done~n"),
    End = erlang:now(),
    io:format("~n==== mcbench result ====~n"),
    io:format("    Interval: ~p msec~n", [round(timer:now_diff(End, Start) / 1000)]),
    io:format("    Performance: ~p commands/sec~n", [round(Commands / (timer:now_diff(End, Start) / 1000 / 1000))]),
    io:format("====~n~n"),
    init:stop().

insert_base_data(Host, Port, BaseDataCount) ->
    WorkerCount = 10,
    Keys = mcbench_util:gen_random_keys(BaseDataCount, BaseDataCount * 10),
    KeysPerWorker = BaseDataCount div WorkerCount,
    Parent = self(),
    do_workers(WorkerCount,
               fun(MyId, AllKeys) ->
                       case memcached:connect(Host, Port) of
                           {ok, Conn} ->
                               lists:foreach(fun(Key) ->
                                                     SKey = integer_to_list(Key),
                                                     ok = memcached:set(Conn, SKey, SKey)
                                             end,
                                             lists:sublist(AllKeys, MyId * KeysPerWorker + 1, KeysPerWorker)),
                               ok = memcached:disconnect(Conn),
                               Parent ! done,
                               ok;
                           Other ->
                               Other
                       end
               end,
               [Keys]).


%% insert_random_data(Host, Port, Count) ->
%%     case memcached:connect(Host, Port) of
%%         {ok, Conn} ->
%%             ok = do_times(Count,
%%                           fun(C) ->
%%                                   Key = integer_to_list(crypto:rand_uniform(1, 100000)),
%%                                   ok = memcached:set(C, Key, "1")
%%                           end,
%%                           [Conn]),
%%             ok = memcached:disconnect(Conn);
%%         Other ->
%%             Other
%%     end.

%% insert_base_data(Host, Port, BaseDataCount) ->
%%     WorkersCount = 10,
%%     if
%%         BaseDataCount > WorkersCount andalso BaseDataCount rem WorkersCount =:= 0 ->
%%             CountPerWorker = BaseDataCount div WorkersCount,
%%             do_workers(WorkersCount,
%%                        fun() ->
%%                                ok = insert_random_data(Host, Port, CountPerWorker),
%%                                io:format("."),
%%                                ok
%%                        end,
%%                        []);
%%        true ->
%%             insert_random_data(Host, Port, BaseDataCount)
%%     end.


do_times(0, _Fun, _Args) ->
    ok;
do_times(N, Fun, Args) ->
    apply(Fun, Args),
    do_times(N - 1, Fun, Args).

do_workers(N, Fun, Args) ->
    do_workers(N, N, Fun, Args).
do_workers(Max, 0, _Fun, _Args) ->
    wait_workers(Max, done),
    ok;
do_workers(Max, N, Fun, Args) ->
    Self = self(),
    spawn(fun() ->
                  ok = apply(Fun, [Max - N | Args]),
                  Self ! done
          end),
    do_workers(Max, N - 1, Fun, Args).


start_workers(_CommandType, 0, _CommandCount, _MaxKey, _Host, _Port, Pids) ->
    {ok, Pids};

start_workers(CommandType, Concurrency, CommandCount, MaxKey, Host, Port, Pids) ->
    Parent = self(),
    {ok, Pid} = mcbench_sup:start_worker([Parent, CommandType, CommandCount, MaxKey, Host, Port]),
    start_workers(CommandType, Concurrency - 1, CommandCount, MaxKey, Host, Port, [Pid | Pids]).

wait_workers(0, _Msg) ->
    ok;
wait_workers(Concurrency, Msg) ->
    receive
        Msg -> []
    after 1000 * 60 * 5->
          io:format("timeout~n")
    end,
    wait_workers(Concurrency - 1, Msg).
