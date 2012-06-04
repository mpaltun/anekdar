-module(tcp_handler).

-export([start_link/4, stop/1]).
-export([init/4]).

%% @private Spawn a process to handle a new connection.
start_link(ListenerPid, Socket, Transport, Options) ->
  Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Options]),
  {ok, Pid}.

init(ListenerPid, Socket, Transport, _Options) ->
  cowboy:accept_ack(ListenerPid), 
  ok = Transport:setopts(Socket, [{packet, line}, binary, {active, true}]),
  loop(Socket, Transport).

loop(Socket, Transport) ->
    receive
        {tcp, Socket, Data} ->
            Parsed_data = anekdar_protocol:parse(Data),
            response(Parsed_data, Transport, Socket);
        {ok, Channel, Message} ->
            Response = anekdar_protocol:sub_response(Channel, Message, "\r\n"),
            Transport:send(Socket, Response),
            loop(Socket, Transport);
        {tcp_closed, Socket} ->
            terminate(Socket, Transport);
        _ ->
            erlang:display(unrecognized_message),
            terminate(Socket, Transport)

    end.

stop(_State) ->
    ok.

response({sub, Channel}, Transport, Socket) ->
    pub_sub_manager:sub(clean_crlf(Channel)),
    loop(Socket, Transport);
response({pub, Channel, Message}, Transport, Socket) ->
    Count = pub_sub_manager:pub(Channel, clean_crlf(Message)),
    Resp = anekdar_protocol:pub_response(Count, "\r\n"),
    Transport:send(Socket, Resp),
    loop(Socket, Transport);
response({error, Why}, Transport, Socket) ->
    Resp = anekdar_protocol:error_response(<<Why/binary, "\r\n">>),
    Transport:send(Socket, Resp),
    loop(Socket, Transport);
response({ping}, Transport, Socket) ->
    Resp = anekdar_protocol:ping_response(<<"\r\n">>),
    Transport:send(Socket, Resp),
    loop(Socket, Transport);
response({quit}, Transport, Socket) ->
    Resp = anekdar_protocol:quit_response(<<"\r\n">>),
    Transport:send(Socket, Resp),
    terminate(Socket, Transport);
response(_, Transport, Socket) ->
    Resp = anekdar_protocol:error_response(<<"unrecognized command\r\n">>),
    Transport:send(Socket, Resp),
    loop(Socket, Transport).

terminate(Socket, Transport) ->
    Transport:close(Socket),
    ok.
clean_crlf(Binary) ->
    binary:part(Binary, {0, byte_size(Binary) - 2}).
