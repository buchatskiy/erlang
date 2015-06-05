-module(p05).
-export([reverse/1]).
reverse(L)->
    reverse(L,[]).
reverse([],L)->
    L;
reverse([H|T],L)->
    reverse(T,[H|L]).