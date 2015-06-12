-module(table).
-export([insert/1, last/1]).

check([])->
     false;
check([cache|_])->
     true;
check([_|T])->
     check(T).

insert(Value)->
    case check(ets:all()) of
        true->Last_cache_key = ets:last(cache);
        _->cache = ets:new(cache, [private, named_table, ordered_set]),
            Last_cache_key = ets:last(cache)
    end,
    case Last_cache_key of
        '$end_of_table' ->
            Key = 0;
        _ ->
            {id, Key} = Last_cache_key
    end,
    ets:insert(cache, {{id, Key+1},{value, Value},{datetime, {date(), time()}}}).

last(N)->
    case check(ets:all()) of
        true->Last_cache_key = ets:last(cache);
        _->cache = ets:new(cache, [private, named_table, ordered_set]),
            Last_cache_key = ets:last(cache)
    end,
    case Last_cache_key of
        '$end_of_table' ->
            io:format("Empty table. Use insert(data) to add some data~n");
        _ ->
            {id, Key} = Last_cache_key,
            last(N, Key)
    end.

last(0, Key)->
    print(Key);
last(N, Key)->
    case (Key-N) < 1 of
        true ->
            last(N-1, Key);
        false ->
            print(Key-N),
            last(N-1, Key)
    end.

print(CacheId) ->
    case ets:lookup(cache, {id, CacheId}) of
        [Cache] ->
            print_cache_id(Cache),
            print_cache_value(Cache),
            print_cache_datetime(Cache);
        [] ->
            io:format("No cache with key = ~p~n", [CacheId])
    end.

print_cache_id(Cache)->
    {{id, Key},_,_}=Cache,
    io:format("Id: ~p~n", [Key]).
print_cache_value(Cache)->
    {_,{value, Value},_}=Cache,
    io:format("Data: ~p~n", [Value]).
print_cache_datetime(Cache)->
    {_,_,{datetime, {{Y,M,D}, {H,Min,S}}}}=Cache,
    io:format("Date: ~.2.0w.~.2.0w.~.4.0w~nTime: ~.2.0w:~.2.0w:~.2.0w~n~n", [D,M,Y,H,Min,S]).