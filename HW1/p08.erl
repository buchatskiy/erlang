-module(p08).
-export([compress/1]).

compress(L)->
    compress(L,[]).
compress([H,H|T],L)->
    compress([H|T],L);
compress([H|[]],L)->
    p05:reverse([H|L]);
compress([H|T],L)->
    compress(T,[H|L]).