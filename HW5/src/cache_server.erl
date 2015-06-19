-module(cache_server).
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").

-export([start_link/1,
    start_link/0,
    stop/0,
    insert/2,
    lookup/1,
    lookup_by_date/2
]).

-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
    ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(init_args, {
    clean_time=3600
}).


start_link([{ttl, Time}]) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #init_args{clean_time=Time}, []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #init_args{}, []).

init(State) ->
    cache_table:start(),
    {ok, State}.

stop() ->
    gen_server:call(?MODULE, stop).

insert(Key, Value) ->
    gen_server:call(?MODULE, {insert, Key, Value}).
lookup(Key) ->
    gen_server:call(?MODULE, {lookup, Key}).
lookup_by_date(DateFrom, DateTo) ->
    gen_server:call(?MODULE, {lookup_by_date, DateFrom, DateTo}).

handle_call({insert, Key, Value}, _From, State) ->
    cache_table:insert(Key, Value),
    timer:apply_after(State#init_args.clean_time*1000, gen_server, call, [?MODULE, {delete, Key}]),
    {reply, {ok, Value}, State};
handle_call({lookup, Key}, _From, State) ->
    Value = cache_table:get(Key),
    {reply, {ok, Value}, State};
handle_call({delete, Key}, _From, State) ->
    cache_table:delete(Key),
    {noreply, State};
handle_call({lookup_by_date, DateFrom, DateTo}, _From, State) ->
    Value = cache_table:get_by_range(DateFrom, DateTo),
    {reply, {ok, Value}, State};
handle_call(stop, _From, Tab) ->
    {stop, normal, stopped, Tab}.

handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
