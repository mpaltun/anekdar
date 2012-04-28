-module(pub_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Channel, _} = cowboy_http_req:binding(channel, Req),
    {Message, _} = cowboy_http_req:binding(message, Req),
    
    Pub_sub_mgr = pub_sub_manager:get_pid(),
    Pub_sub_mgr ! {pub, Channel , Message},
    {ok, Req2} = cowboy_http_req:reply(200, [], <<"true">>, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.
