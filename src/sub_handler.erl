-module(sub_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, info/3, terminate/2]).

-define(TIMEOUT, 30000).

init({_Any, http}, Req, _Opts) ->
    {Channel, Req2} = cowboy_http_req:binding(channel, Req),
    ets_server:put(Channel, self()),
    {loop, Req2, {undefined_state, Channel}, ?TIMEOUT, hibernate}.

handle(_Req, _State) ->
    exit(badarg).

info({ok, Message}, Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"application/json">>},{<<"Access-Control-Allow-Origin">>, <<"*">>}], Message, Req),
    {ok, Req2, State};

info(_Message, Req, State) ->
    {loop, Req, State, hibernate}.

terminate(_Req, {_State, Channel}) ->
    ets_server:remove(Channel, self()),
    ok.
