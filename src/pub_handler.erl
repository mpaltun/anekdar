-module(pub_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Channel, _} = cowboy_http_req:binding(channel, Req),
   
    Reply = case cowboy_http_req:method(Req) of
        {'POST', Req2} ->
            {ok, Message, Req3} = cowboy_http_req:body(Req2),
            L = ets_server:get(Channel),
            lists:map(fun({_, Pid}) -> Pid ! {ok, Channel, Message} end, L),
            Subs_Count = length(L),
            {ok, Req4} = cowboy_http_req:reply(200,
                [{'Content-Type', <<"text/plain">>},{<<"Access-Control-Allow-Origin">>, <<"*">>}],
                    list_to_binary(integer_to_list(Subs_Count)), Req3),
            Req4;
        {_, Req2} ->
            {ok, Req3} = cowboy_http_req:reply(404, [], <<"Not Found">>, Req2),
            Req3
        end,
        {ok, Reply, State}.

terminate(_Req, _State) ->
    ok.
