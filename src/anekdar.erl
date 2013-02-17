%% Feel free to use, reuse and abuse the code in this file.

-module(anekdar).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
	application:start(cowboy),
	application:start(anekdar).

start(_Type, _Args) ->
  PrivDir = get_priv_dir(),
  {ok, Conf} = file:consult(PrivDir ++ "/anekdar.conf"),
  [{dispatch, Dispatch, http_address, Http_address, http_port, Http_port, tcp_address, Tcp_address, tcp_port, Tcp_port}] = Conf,
	cowboy:start_listener(my_http_listener, 100,
		cowboy_tcp_transport, [{port, Http_port}, {ip, Http_address}],
		cowboy_http_protocol, [{dispatch, [Dispatch]}]
	),
    cowboy:start_listener(my_tcp_listener, 100,
        cowboy_tcp_transport, [{port, Tcp_port}, {ip, Tcp_address}],
        anekdar_tcp_handler, []
    ),
    ets_server:start_link(),
    stats_server:start_link(),
	anekdar_sup:start_link().

get_priv_dir() ->
    case code:priv_dir(anekdar) of
        {error, bad_name} ->
            {ok, Cwd} = file:get_cwd(),
            Cwd ++ "/" ++ "priv/";
        Priv ->
            Priv ++ "/"
    end.

stop(_State) ->
	ok.
