-module(table).

-include_lib("stdlib/include/ms_transform.hrl").

-export([start/0,
    stop/0,
    insert/1,
    last/1,
    delete/1,
    get/1,
    get_with_time/1,
    get_by_range/2,
    get_by_time/1,
    created/1,
    delete_by_range/2
]).

start()->
    ets:new(cache, [private, named_table, ordered_set]).

stop()->
    ets:delete(cache).

insert(Value)->
    Last_cache_key = ets:last(cache),
    case Last_cache_key of
        '$end_of_table' -> Key=0;
        _ -> Key = Last_cache_key
    end,
    ets:insert(cache, {Key+1, Value, now()}).

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
        [{Key, Value, Time}] -> {Value, Time}
    end.

get_by_range(From, To) ->
    Cond = ets:fun2ms(fun({_, Value, Time}) when Time >= From andalso Time =< To -> Value end),
    ets:select(cache, Cond).

get_by_time(Time) ->
    Cond = ets:fun2ms(fun({_, Value, Time0}) when Time =:= Time0 -> Value end),
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
    Cond = ets:fun2ms(fun({_, _, Time}) when Time >= From andalso Time =< To -> true end),
    ets:select_delete(cache, Cond).