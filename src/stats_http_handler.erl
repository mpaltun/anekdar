-module(stats_http_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
	{Path_info, Req2} = cowboy_http_req:path_info(Req),
	handle_path_info(Path_info, Req2, State).

handle_path_info([<<"channel">>, Channel], Req, State) ->
	Count = stats_server:get_stats(Channel),
    Json = jiffy:encode(Count),
    reply(Json, Req, State);

handle_path_info([], Req, State) ->
	All = stats_server:get_stats(),
	Json = jiffy:encode({All}),
    reply(Json, Req, State);

handle_path_info(_, Req, State) ->
	{ok, Req2} = cowboy_http_req:reply(404, [], <<"404">>, Req),
    {ok, Req2, State}.

reply(Json, Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], Json, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.
