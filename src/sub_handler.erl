-module(sub_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Channel, Req2} = cowboy_http_req:binding(channel, Req),
    Pub_sub_mgr = pub_sub_manager:get_pid(),
    Pub_sub_mgr ! {sub, Channel, self()},
    receive
        {ok, Message} ->
            {ok, Req3} = cowboy_http_req:reply(200, [], Message, Req2)
    end,
    {ok, Req3, State}.

terminate(_Req, _State) ->
    ok.
