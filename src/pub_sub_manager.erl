-module(pub_sub_manager).

-export([get_pid/0]).

get_pid() ->
    Pid = whereis(pub_sub_pid),
    if
        is_pid(Pid) ->
            Pid;
        true ->
            NewPid = spawn(fun() -> pub_sub()
                           end),
            register(pub_sub_pid, NewPid),
            NewPid
    end.
    

pub_sub() ->
    Ets_pid = ets_manager:get_pid(),
    receive
        {sub, Channel, Pid} ->
            Ets_pid ! {put, Channel, Pid},
            pub_sub();
        {pub, Channel, Message} ->
            Ets_pid ! { get, Channel, self()},
            receive
                L ->
                    lists:map(fun({_, Pid}) -> Pid ! {ok, Message} end, L)
            end,
            pub_sub()
    end.
