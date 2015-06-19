-module(cache_table).

-include_lib("stdlib/include/ms_transform.hrl").

-export([start/0,
    stop/0,
    insert/2,
    last/1,
    delete/1,
    get/1,
    get_with_time/1,
    get_by_range/2,
    get_by_time/1,
    created/1,
    delete_by_range/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start()->
    ets:new(cache, [named_table, ordered_set]).

stop()->
    ets:delete(cache).

insert(Key, Value)->
    ets:insert(cache, {Key, Value, calendar:datetime_to_gregorian_seconds({date(), time()}) }).

last(N)->
    Last_cache_key = ets:last(cache),
    Cond = ets:fun2ms(fun({Key, Value, _}) when Key > (Last_cache_key-N) -> Value end),
    ets:select(cache, Cond).

delete(Key) ->
    ets:delete(cache, Key).

get(Key) ->
    Res = get_with_time(Key),
    case Res of
        {Value, _} -> Value;
        false -> false
    end.

get_with_time(Key) ->
    Res = ets:lookup(cache, Key),
    case Res of
        [] -> false;
        [{Key, Value, Time}] -> {Value, calendar:gregorian_seconds_to_datetime(Time)}
    end.

get_by_range(From, To) ->
    FromS=calendar:datetime_to_gregorian_seconds(From),
    ToS=calendar:datetime_to_gregorian_seconds(To),
    Cond = ets:fun2ms(fun({_, Value, Time}) when Time >= FromS andalso Time =< ToS -> Value end),
    ets:select(cache, Cond).

get_by_time(Time) ->
    TimeS=calendar:datetime_to_gregorian_seconds(Time),
    Cond = ets:fun2ms(fun({_, Value, Time0}) when TimeS =:= Time0 -> Value end),
    Res = ets:select(cache, Cond),
    case Res of
        [] -> [];
        [El] -> El
    end.

created(Key) ->
    Res = get_with_time(Key),
    case Res of
        {_, Time} -> Time;
        false -> false
    end.

delete_by_range(From, To) ->
    FromS=calendar:datetime_to_gregorian_seconds(From),
    ToS=calendar:datetime_to_gregorian_seconds(To),
    Cond = ets:fun2ms(fun({_, _, Time}) when Time >= FromS andalso Time =< ToS -> true end),
    ets:select_delete(cache, Cond).

-ifdef(TEST).
table_test() ->
    ?assertEqual(cache, cache_table:start()),
    ?assertEqual(true,cache_table:insert(1, [1,2,3])),
    {Value, Time} = cache_table:get_with_time(1),
    ?assertEqual(Value, cache_table:get(1)),
    ?assertEqual(Value, cache_table:get_by_time(Time)),
    ?assertEqual(Time, cache_table:created(1)),
    ?assertEqual(false, cache_table:get(2)),
    cache_table:delete(1),
    ?assertEqual(false, cache_table:get(1)),
    ?assertEqual(true, cache_table:stop()).
-endif.