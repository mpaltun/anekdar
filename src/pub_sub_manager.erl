-module(pub_sub_manager).

-export([get_pid/0]).

get_pid() ->
    Pid = whereis(pub_sub_pid),
    if
        is_pid(Pid) ->
            Pid;
        true ->
            NewPid = spawn(fun() -> pub_sub([])
                           end),
            register(pub_sub_pid, NewPid),
            NewPid
    end.
    

pub_sub(Subscribers) ->
    receive
        {sub, Channel, Pid} ->
           pub_sub([Pid | Subscribers]);
        {pub, Channel, Message} ->
            lists:map(fun(Pid) -> Pid ! {ok, Message} end,Subscribers),
            pub_sub([])
    end.
