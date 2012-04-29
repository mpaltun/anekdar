-module(sub_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, info/3, terminate/2]).

-define(TIMEOUT, 30000).

init({_Any, http}, Req, _Opts) ->
    {Channel, Req2} = cowboy_http_req:binding(channel, Req),
    pub_sub_manager:sub(Channel),
    {loop, Req2, undefined_state, ?TIMEOUT, hibernate}.

handle(_Req, _State) ->
    exit(badarg).

info({ok, Message}, Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(200, [], Message, Req),
    {ok, Req2, State};

info(_Message, Req, State) ->
    {loop, Req, State, hibernate}.

%handle(Req, State) ->
%    {Channel, Req2} = cowboy_http_req:binding(channel, Req),
%    Pub_sub_mgr = pub_sub_manager:get_pid(),
%    Pub_sub_mgr ! {sub, Channel, self()},
%    receive
%        {ok, Message} ->
%            {ok, Req3} = cowboy_http_req:reply(200, [], Message, Req2)
%    end,
%    {ok, Req3, State}.
%
terminate(_Req, _State) ->
    ok.
