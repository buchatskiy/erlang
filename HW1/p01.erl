-module(p01).
-export([last/1]).
last([H|[]])->
    H;
last([H|T])->
    last(T).
