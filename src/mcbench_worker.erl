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
%%% File    : mcbench_worker.erl
%%% Author  : Taro Minowa(Higepon) <higepon@labs.cybozu.co.jp>
%%% Description : mcbench worker
%%%
%%% Created :  10 Dec 2009 by higepon <higepon@labs.cybozu.co.jp>
%%%-------------------------------------------------------------------

-module(mcbench_worker).

-export([start_link/1, worker/6]).

start_link(Args) ->
    Pid = spawn_link(?MODULE, worker, Args),
    {ok, Pid}.

worker(Parent, CommandType, CommandCount, MaxKey, Host, Port) ->
    case memcached:connect(Host, Port) of
        {ok, Conn} ->
            Keys = make_keys(CommandCount, MaxKey),
            Parent ! make_keys_done,
            receive go ->
                    case CommandType of
                        set ->
                            ok = do_set(Conn, Keys);
                        get ->
                            ok = do_get(Conn, Keys);
                        set_get ->
                            ok = do_set_get(Conn, Keys);
                        Other ->
                            io:format("    Error:invalid command type: ~p~n", [Other]),
                            exit(invalid_command_type)
                    end
            end,
            ok = memcached:disconnect(Conn),
            Parent ! done;
        {error, Reason} ->
            io:format("    Error:can't connect to ~p:~p<~p>~n", [Host, Port, Reason]),
            exit(Reason)
    end.

make_keys(N, MaxKey) ->
    do_ntimes(N, [], fun() -> integer_to_list(crypto:rand_uniform(1, MaxKey)) end).


do_ntimes(0, Accum, _Fun) ->
    lists:reverse(Accum);
do_ntimes(N, Accum, Fun) ->
    do_ntimes(N - 1,
              [apply(Fun, []) | Accum],
              Fun).


do_set_get(Conn, Keys) ->
    lists:foreach(fun(Key) ->
                          ok = memcached:set(Conn, Key, Key),
                          {ok, Key} =  memcached:get(Conn, Key)
                          end,
                 Keys).

do_get(Conn, Keys) ->
    lists:foreach(fun(Key) ->
                          memcached:get(Conn, Key)
                          end,
                 Keys).

do_set(Conn, Keys) ->
    lists:foreach(fun(Key) ->
                          ok = memcached:set(Conn, Key, Key)
                          end,
                 Keys).
