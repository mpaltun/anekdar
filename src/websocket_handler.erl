-module(websocket_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, undefined_state}.

websocket_handle({text, Data}, Req, State) ->
    Parsed_data = anekdar_protocol:parse(Data),
    response(Parsed_data, Req, State);
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({ok, Channel, Message}, Req, State) ->
    Response = anekdar_protocol:sub_response(Channel, Message, ""),
    {reply, {text, Response}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, {Channel, _State}) ->
    pub_sub_manager:unsub(Channel),
    ok.

response({sub, Channel}, Req, State) ->
    pub_sub_manager:sub(Channel),
    {ok, Req, {Channel, State}};
response({pub, Channel, Message}, Req, State) ->
    Count = pub_sub_manager:pub(Channel, Message),
    Resp = anekdar_protocol:pub_response(Count, ""),
    {reply, {text, Resp}, Req, State};
response(_, Req, State) ->
    Resp = anekdar_protocol:error_response(<<"unrecognized command">>),
    {reply, {text, Resp}, Req, State}.
