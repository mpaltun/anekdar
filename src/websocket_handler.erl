-module(websocket_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, undefined_state}.

websocket_handle({text, <<"sub ", Channel/binary>>}, Req, State) ->
    ets_server:put(Channel, self()),
    {ok, Req, State};
websocket_handle({text, <<"pub ", Data/binary>>}, Req, State) ->
    [Channel, Message] = re:split(Data, <<" ">>, [{parts, 2}]),
    L = ets_server:get(Channel),
    lists:map(fun({_, Pid}) -> Pid ! {ok, Message} end, L),
    Subs_Count = length(L),
    {reply, {text, list_to_binary(integer_to_list(Subs_Count))}, Req, State};
websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, << "Undeterminated Message: ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({ok, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
