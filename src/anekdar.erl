%% Feel free to use, reuse and abuse the code in this file.

-module(anekdar).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
	application:start(cowboy),
	application:start(anekdar).

start(_Type, _Args) ->
	Dispatch = [
		{'_', [
            {[], index_http_handler, []},
            {[<<"ws">>], websocket_handler, []},
            {[<<"sub">>, channel], sub_http_handler, []},
            {[<<"pub">>, channel], pub_http_handler, []},
            {'_', default_http_handler, []}
		]}
	],
	cowboy:start_listener(my_http_listener, 100,
		cowboy_tcp_transport, [{port, 9999}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),
    cowboy:start_listener(my_tcp_listener, 100,
        cowboy_tcp_transport, [{port, 9998}],
        anekdar_tcp_handler, []
    ),
    ets_server:start_link(),
    stats_server:start_link(),
	anekdar_sup:start_link().

stop(_State) ->
	ok.
