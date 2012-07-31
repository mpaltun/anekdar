-module(stats_server).
-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([start_link/0, start/0, get_stats/0, get_stats/1,
            incr/1, decr/1]).

-record(state, {}).

get_stats() -> gen_server:call(?MODULE, stats).
get_stats(Channel) -> gen_server:call(?MODULE, {stats, Channel}).
incr(Channel) -> gen_server:cast(?MODULE, {incr, Channel}).
decr(Channel) -> gen_server:cast(?MODULE, {decr, Channel}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(stats, [set, named_table]),
    {ok, #state{}}.

handle_call(stats, _From, State) ->
    All = ets:tab2list(stats),
    {reply, All, State};

handle_call({stats, Channel}, _From, State) ->
    Count = get_count(ets:lookup(stats, Channel)),
    {reply, Count, State}.

handle_cast({incr, Channel}, State) ->
    Results = ets:lookup(stats, Channel),
    incr(Channel, Results),
    {noreply, State};

handle_cast({decr, Channel}, State) ->
    Count = get_count(ets:lookup(stats, Channel)),
    decr(Channel, Count),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

incr(Channel, []) ->
    ets:insert(stats, {Channel, 1});
incr(Channel, _) ->
    ets:update_counter(stats, Channel, 1).

decr(Channel, 1) ->
    ets:delete(stats, Channel);

decr(Channel, Count) when Count > 0 ->
    ets:update_counter(stats, Channel, -1);

decr(_Channel, _Count) ->
    ok.

get_count([]) ->
    0;
get_count([{_Channel, Count} | []]) ->
    Count.
