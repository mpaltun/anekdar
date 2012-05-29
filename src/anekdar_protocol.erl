-module(anekdar_protocol).
-author('Mustafa Paltun <mpaltun@gmail.com>').

-export([parse/1, sub_response/3, pub_response/2, error_response/1]).

-define(DELIMITER, " ").
-define(ERROR, "-").
-define(SUCCESS_STR, "+").
-define(SUCCESS_INT, ":").

-define(COMMAND_SUB, "sub").
-define(COMMAND_PUB, "pub").

sub_response(Channel, Message, Crlf) ->
    [?SUCCESS_STR, Channel, ?DELIMITER, Message, Crlf].
pub_response(Count, Crlf) ->
    [?SUCCESS_INT, list_to_binary(integer_to_list(Count)), Crlf].
error_response(Why) ->
    [?ERROR, Why].

%Incoming binary data handling
parse(<<?COMMAND_SUB, ?DELIMITER, Channel/binary>>) ->
    {sub, Channel}; 
parse(<<?COMMAND_PUB, ?DELIMITER, Data/binary>>) ->
    L = re:split(Data, ?DELIMITER, [{parts, 2}]),
    if
        length(L) =:= 2 ->
            [Channel, Message] = L,
            {pub, Channel, Message};
        true ->
            {error, <<"message or channel missed">>} 
    end;
parse(_) ->
    {error}.
