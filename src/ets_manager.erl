-module(ets_manager).

-export([get_pid/0]).

get_pid() ->
    Pid = whereis(ets_pid),
    if
        is_pid(Pid) ->
            Pid;
        true ->
            NewPid = spawn(fun() -> ets:new(subs, [bag, named_table]),
                                    loop()
                           end),
            register(ets_pid, NewPid),
            NewPid
    end.
    

loop() ->
    receive
        {put, Channel, Pid} ->
            ets:insert(subs, {Channel, Pid}),
            loop(); 
        {get, Channel, Pid} ->
            Results = ets:lookup(subs, Channel),
            Pid ! Results,
            % delete pids
            %asdasdets:delete(Table, Channel),
            loop()
    end.
