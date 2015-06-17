-module(cache_server).

-include_lib("stdlib/include/ms_transform.hrl").

-export([start_link/1,
    stop/0,
    insert/2,
    last/1,
    delete/1,
    lookup/1,
    get_with_time/1,
    lookup_by_date/2,
    get_by_time/1,
    created/1,
    delete_by_range/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start_link([{ttl, Time}])->
    register(?MODULE, spawn(fun() -> loop() end)),
    rpc({start_link, Time}).

stop()->
    rpc({stop}).

insert(Key, Value)->
    rpc({insert, Key, Value}).

last(N)->
    rpc({last, N}).

delete(Key) ->
    rpc({delete, Key}).

lookup(Key) ->
    rpc({lookup, Key}).

get_with_time(Key) ->
    rpc({get_with_time, Key}).

lookup_by_date(From, To) ->
    rpc({lookup_by_date, calendar:datetime_to_gregorian_seconds(From), calendar:datetime_to_gregorian_seconds(To)}).

get_by_time(Time) ->
    rpc({get_by_time, calendar:datetime_to_gregorian_seconds(Time)}).

created(Key) ->
    rpc({created, Key}).

delete_by_range(From, To) ->
    rpc({delete_by_range, calendar:datetime_to_gregorian_seconds(From), calendar:datetime_to_gregorian_seconds(To)}).

rpc(Q) ->
    ?MODULE ! {self(), Q},
    receive
        {?MODULE, Reply} ->
            Reply
    end.

loop() ->
    receive
        {From, {start_link, Time}} ->
            From ! {?MODULE, ets:new(cache, [private, named_table, ordered_set])},
            timer: apply_after(Time*1000, io, format, ["~nShould be table update here!~n", []]),
            loop();
        {From, {stop}} ->
            From ! {?MODULE, ets:delete(cache)},
            loop();
        {From, {insert, Key, Value}} ->
            ets:insert(cache, {Key, Value, calendar:datetime_to_gregorian_seconds({date(), time()}) }),
            %timer: apply_after(10000, ets, delete, [cache, Key]),
            From ! {?MODULE, ok},
            loop();
        {From, {last, N}} ->
            Last_cache_key = ets:last(cache),
            Cond = ets:fun2ms(fun({Key, Value, _}) when Key > (Last_cache_key-N) -> Value end),
            From ! {?MODULE, {ok, ets:select(cache, Cond)}},
            loop();
        {From, {lookup, Key}} ->
            Res = ets:lookup(cache, Key),
            case Res of
                [] -> From ! {?MODULE, false};
                [{Key, Value, _}] -> From ! {?MODULE, {ok, Value}}
            end,
            loop();
        {From, {delete, Key}} ->
            From ! {?MODULE, ets:delete(cache, Key)},
            loop();
        {From, {get_with_time, Key}} ->
            Res = ets:lookup(cache, Key),
            case Res of
                [] -> From ! {?MODULE, false};
                [{Key, Value, Time}] -> From ! {?MODULE, {ok, {Value, calendar:gregorian_seconds_to_datetime(Time)}}}
            end,
            loop();
        {From, {lookup_by_date, From, To}} ->
            Cond = ets:fun2ms(fun({_, Value, Time}) when Time >= From andalso Time =< To -> Value end),
            From ! {?MODULE, {ok, ets:select(cache, Cond)}},
            loop();
        {From, {get_by_time, Time}} ->
            Cond = ets:fun2ms(fun({_, Value, Time0}) when Time =:= Time0 -> Value end),
            Res = ets:select(cache, Cond),
            case Res of
                [] -> From ! {?MODULE, []};
                [X] -> From ! {?MODULE, {ok, X}}
            end,
            loop();
        {From, {created, Key}} ->
            Res = ets:lookup(cache, Key),
            case Res of
                [] -> From ! {?MODULE, false};
                [{Key, _, Time}] -> From ! {?MODULE, {ok, calendar:gregorian_seconds_to_datetime(Time)}}
            end,
            loop();
        {From, {delete_by_range, From, To}} ->
            Cond = ets:fun2ms(fun({_, Value, Time}) when Time >= From andalso Time =< To -> Value end),
            From ! {?MODULE, {ok, ets:select_delete(cache, Cond)}},
            loop()
    end.


-ifdef(TEST).
table_test() ->
    ?assertEqual(cache, start_link([{ttl,3600}])),
    ?assertEqual(ok, insert(1, [1,2,3])),
    {ok, {Value, Time}} = get_with_time(1),
    ?assertEqual({ok, Value}, lookup(1)),
    ?assertEqual({ok, Value}, get_by_time(Time)),
    ?assertEqual({ok, Time}, created(1)),
    ?assertEqual(false, lookup(2)),
    delete(1),
    ?assertEqual(false, lookup(1)),
    ?assertEqual(true, stop()).
-endif.