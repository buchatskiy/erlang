-module(p13).
-export([decode/1]).

decode(L)->
    decode(L,[]).

decode([{N,H}|T],L)->
    decode(T,decode(N,H,L));
decode([],L)->
    p05:reverse(L).

decode(1,H,L)->
    [H|L];
decode(N,H,L)->
    decode(N-1,H,[H|L]).
