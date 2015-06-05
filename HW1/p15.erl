-module(p15).
-export([replicate/2]).

replicate(L,N)->
    replicate(L,N,[]).

replicate([H|T],N,L)->
    replicate(T,N,replicate(H,N,L));
replicate([],_,L)->
    p05:reverse(L);
replicate(H,1,L)->
    [H|L];
replicate(H,N,L)->
    replicate(H,N-1,[H|L]).