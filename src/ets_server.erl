-module(ets_server).
-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([start_link/0, start/0, get/1, put/2, remove/2]).

-record(state, {}).

get(Channel) -> gen_server:call(?MODULE, {get, Channel}).
put(Channel, Pid) -> gen_server:call(?MODULE, {put, Channel, Pid}).
remove(Channel, Pid) -> gen_server:cast(?MODULE, {remove, Channel, Pid}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(subs, [bag, named_table]),
    {ok, #state{}}.

handle_call({put, Channel, Pid}, _From, State) ->
    ets:insert(subs, {Channel, Pid}),
    {reply, ok, State};

handle_call({get, Channel}, _From, State) ->
    Results = ets:lookup(subs, Channel),
    {reply, Results, State}.

handle_cast({remove, Channel, Pid}, State) ->
    ets:delete_object(subs, {Channel, Pid}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
