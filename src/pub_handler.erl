-module(pub_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Channel, _} = cowboy_http_req:binding(channel, Req),
   
    Reply = case cowboy_http_req:method(Req) of
        {'POST', _} ->
            {ok, Message, _} = cowboy_http_req:body(Req),
            L = ets_server:get(Channel),
            lists:map(fun({_, Pid}) -> Pid ! {ok, Message} end, L),
            Subs_Count = length(L),
            {ok, Req2} = cowboy_http_req:reply(200,
                [{'Content-Type', <<"application/json">>}],
                    list_to_binary(integer_to_list(Subs_Count)), Req),
            Req2;
        _ ->
            {ok, Req2} = cowboy_http_req:reply(404, [], <<"Not Found">>, Req),
            Req2
        end,
        {ok, Reply, State}.

terminate(_Req, _State) ->
    ok.
