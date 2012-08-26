-module(pub_sub_manager).
-author('Mustafa Paltun <mpaltun@gmail.com>').

-export([sub/1, mpub/2, pub/2, unsub/1]).

sub(Channel) ->
    ets_server:put(Channel, self()),
    stats_server:incr(Channel).

async_pub(Channel, Message, Pid) ->
    spawn(fun() ->
        Count = pub(Channel, Message),
        Pid ! {Channel, Count}
    end).

pub(Channel, Message) ->
    L = ets_server:get(Channel),
    lists:map(fun({_, Pid}) -> Pid ! {ok, Channel, Message} end, L),
    % return subscriber count
    length(L).

mpub(Channels, Message) ->
    lists:map(fun(Channel) ->
        async_pub(Channel, Message, self())
    end, Channels),
    wait_response([], length(Channels)).

unsub(Channel) ->
    ets_server:remove(Channel, self()),
    stats_server:decr(Channel).

wait_response(Responses, 0) ->
    Responses;
wait_response(Responses, Remaining) ->
    receive
        {Channel, Count} ->
            wait_response([{Channel, Count} | Responses], Remaining - 1)
    end.
          
