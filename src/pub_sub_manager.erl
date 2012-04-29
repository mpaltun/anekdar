-module(pub_sub_manager).

-export([pub/2, sub/1]).

sub(Channel) ->
    Ets_pid = ets_manager:get_pid(),
    Ets_pid ! {put, Channel, self()}.

pub(Channel, Message) ->
    Ets_pid = ets_manager:get_pid(),
    Ets_pid ! { get, Channel, self()},
    receive
        L ->
            lists:map(fun({_, Pid}) -> Pid ! {ok, Message} end, L)
    end.
