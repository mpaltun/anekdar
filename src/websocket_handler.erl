-module(websocket_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3]).

-record(state, {
    channels = gb_sets:new() :: gb_set()
}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, #state{channels=gb_sets:new()}}.

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

websocket_terminate(_Reason, _Req, #state{channels=S}) ->
    Channels = gb_sets:to_list(S),
    lists:map(fun(Channel) -> pub_sub_manager:unsub(Channel) end, Channels),
    ok;
websocket_terminate(_Reason, _Req, _State) ->
    ok.
 
response({sub, Channel}, Req, #state{channels=S}) ->
    pub_sub_manager:sub(Channel),
    S1 = gb_sets:add(Channel, S),
    {ok, Req, #state{channels=S1}};
response({pub, Channel, Message}, Req, State) ->
    Count = pub_sub_manager:pub(Channel, Message),
    Resp = anekdar_protocol:pub_response(Count, ""),
    {reply, {text, Resp}, Req, State};
response({unsub, Channel}, Req, #state{channels=S}) ->
    pub_sub_manager:unsub(Channel),
    S1 = gb_sets:delete_any(Channel, S),
    Resp = anekdar_protocol:unsub_response(""),
    {reply, {text, Resp}, Req, #state{channels=S1}};
response({ping}, Req, State) ->
    Resp = anekdar_protocol:ping_response(<<"">>),
    {reply, {text, Resp}, Req, State};
response({quit}, Req, State) ->
    _Resp = anekdar_protocol:quit_response(<<"">>),
    {shutdown, Req, State};
response(_, Req, State) ->
    Resp = anekdar_protocol:error_response(<<"unrecognized command">>),
    {reply, {text, Resp}, Req, State}.
