-module(anekdar_tcp_handler).
-author('Mustafa Paltun <mpaltun@gmail.com>').

-export([start_link/4, stop/1]).
-export([init/4]).

-define(DELIMITER, " ").
-define(ERROR, "-").
-define(SUCCESS_STR, "+").
-define(SUCCESS_INT, ":").
-define(CRLF, "\r\n").

-define(COMMAND_SUB, "sub").
-define(COMMAND_UNSUB, "unsub").
-define(COMMAND_PUB, "pub").
-define(COMMAND_QUIT, "quit").
-define(COMMAND_PING, "ping").

-record(state, {
    socket :: inet:socket(),
    transport :: module(),
    channels = [] :: list()
}).


%% @private Spawn a process to handle a new connection.
start_link(ListenerPid, Socket, Transport, Options) ->
  Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Options]),
  {ok, Pid}.

init(ListenerPid, Socket, Transport, _Options) ->
  cowboy:accept_ack(ListenerPid), 
  ok = Transport:setopts(Socket, [{packet, line}, binary, {active, true}]),
  wait_request(#state{socket=Socket, transport=Transport}).

wait_request(State=#state{socket=Socket, transport=Transport, channels=_Channels}) ->
    receive
        {tcp, Socket, Data} ->
            handle_request(Data, State);
        {ok, Channel, Message} ->
            Response = response_sub(Channel, Message),
            Transport:send(Socket, Response),
            wait_request(State);
        {tcp_closed, Socket} ->
            terminate(State);
        _ ->
            erlang:display("sth went wrong"),
            terminate(State)
    end.


handle_request(<<?COMMAND_SUB, ?DELIMITER, Channel/binary>>, State) ->
    handle_sub(clean_crlf(Channel), State);

handle_request(<<?COMMAND_PUB, ?DELIMITER, Data/binary>>, State) ->
    L = re:split(Data, ?DELIMITER, [{parts, 2}]),
    if
        length(L) =:= 2 ->
            [Channel, Message] = L,
            handle_pub(Channel, clean_crlf(Message), State);
        true ->
            handle_warn(<<"message or channel missed">>, State)
    end;
handle_request(<<?COMMAND_UNSUB, ?DELIMITER, Channel/binary>>, State) ->
    handle_unsub(clean_crlf(Channel), State);

handle_request(<<?COMMAND_PING, ?CRLF>>, State) ->
    handle_ping(State);

handle_request(<<?COMMAND_QUIT, ?CRLF>>, State) ->
    handle_quit(State);

handle_request(Any, State) ->
    Message = clean_crlf(Any),
    handle_warn(<<"unrecognized message: ", Message/binary>>, State).


handle_sub(Channel, #state{socket=Socket, 
        transport=Transport, channels=Channels}) ->
    pub_sub_manager:sub(Channel),
    Channels1 = [Channel | Channels],
    wait_request(#state{socket=Socket, transport=Transport, channels=Channels1}).

handle_unsub(Channel, #state{socket=Socket,
        transport=Transport, channels=Channels}) ->
    pub_sub_manager:unsub(Channel),
    Channels1 = lists:delete(Channel, Channels),
    Response = response_unsub(),
    Transport:send(Socket, Response),
    wait_request(#state{socket=Socket, transport=Transport, channels=Channels1}).

handle_pub(Channel, Message, State=#state{socket=Socket,
        transport=Transport, channels=_Channels}) ->
    Count = pub_sub_manager:pub(Channel, Message),
    Response = response_pub(Count),
    Transport:send(Socket, Response),
    wait_request(State).

handle_warn(Message, State=#state{socket=Socket, 
        transport=Transport, channels=_Channels}) ->
    Response = response_error(Message),
    Transport:send(Socket, Response),
    wait_request(State).

handle_ping(State=#state{socket=Socket, 
        transport=Transport, channels=_Channels}) ->
    Response = response_ping(),
    Transport:send(Socket, Response),
    wait_request(State).

handle_quit(State=#state{socket=Socket, 
        transport=Transport, channels=_Channels}) ->
    Resp = response_quit(),
    Transport:send(Socket, Resp),
    terminate(State).


% called when someone publish message that I am interested in
response_sub(Channel, Message) ->
    [?SUCCESS_STR, Channel, ?DELIMITER, Message, ?CRLF].

response_unsub() ->
    [?SUCCESS_STR, <<"ok">>, ?CRLF].

response_pub(Count) ->
    [?SUCCESS_INT, list_to_binary(integer_to_list(Count)), ?CRLF].

response_error(Message) ->
    [?ERROR, Message, ?CRLF].

response_ping() ->
    [?SUCCESS_STR, <<"pong">>, ?CRLF].

response_quit() ->
    [?SUCCESS_STR, <<"bye">>, ?CRLF].

clean_crlf(Binary) ->
    binary:part(Binary, {0, byte_size(Binary) - 2}).

terminate(#state{socket=Socket, 
        transport=Transport, channels=Channels}) ->
    lists:map(fun(Channel) -> pub_sub_manager:unsub(Channel) end, Channels),
    Transport:close(Socket),
    ok.

stop(_State) ->
    ok.
