-module(table).
-export([insert/1, last/1]).

insert(Value)->
    try ets:last(cache)
    catch
        _:_ -> ets:new(cache, [private, named_table, ordered_set])
    end,
    Last_cache_key = ets:last(cache),
    case Last_cache_key of
        '$end_of_table' -> Key=0;
        _ -> {id, Key} = Last_cache_key
    end,
    ets:insert(cache, {{id, Key+1},{value, Value},{datetime, {date(), time()}}}).

last(N)->
    try ets:last(cache)
    catch
        _:_ -> ets:new(cache, [private, named_table, ordered_set])
    end,
    Last_cache_key = ets:last(cache),
    case Last_cache_key of
        '$end_of_table' ->
            io:format("Empty table. Use insert(data) to add some data~n");
        _ ->
            {id, Key} = Last_cache_key,
            last(N-1, Key)
    end.

last(0, Key)->
    print(Key);
last(N, Key)->
    if
        N=<0 -> io:format("Wrong number '~p' of last~n", [N+1]);
        (Key-N) < 1, N>0 -> last(N-1, Key);
        (Key-N) >= 1, N>0 -> print(Key-N), last(N-1, Key)
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