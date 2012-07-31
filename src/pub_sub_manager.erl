-module(pub_sub_manager).
-author('Mustafa Paltun <mpaltun@gmail.com>').

-export([sub/1, pub/2, unsub/1]).

sub(Channel) ->
    ets_server:put(Channel, self()),
    stats_server:incr(Channel).

pub(Channel, Message) ->
    L = ets_server:get(Channel),
    lists:map(fun({_, Pid}) -> Pid ! {ok, Channel, Message} end, L),
    % return subscriber count
    length(L).

unsub(Channel) ->
    ets_server:remove(Channel, self()),
    stats_server:decr(Channel).