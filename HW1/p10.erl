-module(p10).
-export([encode/1]).

encode(L)->
    encode(L,[],1).
encode([H,H|T],L1,N)->
    encode([H|T],L1,N+1);
encode([H|[]],L1,N)->
    p05:reverse([{N,H}|L1]);
encode([H|T],L1,N)->
    encode(T,[{N,H}|L1],1).