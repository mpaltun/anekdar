-module(tcp_handler).

-export([start_link/4, stop/1]).
-export([init/4]).

%% @private Spawn a process to handle a new connection.
start_link(ListenerPid, Socket, Transport, Options) ->
  Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Options]),
  {ok, Pid}.

init(ListenerPid, Socket, Transport, _Options) ->
  ok = Transport:setopts(Socket, [{packet, line}, binary, {active, true}]),
  cowboy:accept_ack(ListenerPid), 
  ok = Transport:send(Socket, <<"Welcome to anekdar tcpserver\n">>),
  loop(Socket, Transport).

loop(Socket, Transport) ->
    receive
        {tcp, Socket, <<"sub ", Channel/binary>>} -> 
            % clean \r\n
            Ch = remove_rn_from_end(Channel),
            ets_server:put(Ch, self());
        {tcp, Socket, <<"pub ", Data/binary>>} ->
            D = remove_rn_from_end(Data),
            [Channel, Message] = re:split(D, <<" ">>, [{parts, 2}]),
            L = ets_server:get(Channel),
            lists:map(fun({_, Pid}) -> Pid ! {ok, Channel, Message} end, L),
            Subs_Count = length(L),
            Transport:send(Socket, 
                [<<"count ">>, Channel, " ", list_to_binary(integer_to_list(Subs_Count)), <<"\n">>]);
        {ok, Channel, Msg} ->
            Transport:send(Socket, [<<"pub ">>, Channel, <<" ">> , Msg, <<"\n">>]);
        _ ->
            ok
    end,
    loop(Socket, Transport).

stop(_State) ->
  ok.

% internal api
remove_rn_from_end(Binary) ->
    binary:part(Binary, {0, byte_size(Binary)-2}).
