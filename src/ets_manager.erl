-module(ets_manager).

-export([get_pid/0]).

get_pid() ->
    Pid = whereis(ets_pid),
    if
        is_pid(Pid) ->
            Pid;
        true ->
            Table = ets:new(subs, [public, bag]),
            NewPid = spawn(fun() -> loop(Table)
                           end),
            register(ets_pid, NewPid),
            NewPid
    end.
    

loop(Table) ->
    receive
        {put, Channel, Pid} ->
            ets:insert(Table, {Channel, Pid}),
            loop(Table); 
        {get, Channel, Pid} ->
            Results = ets:lookup(Table, Channel),
            Pid ! Results,
            % delete pids
            ets:delete(Table, Channel),
            loop(Table)
    end.
