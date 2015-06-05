-module(p12).
-export([decode_modified/1]).

decode_modified(L)->
    decode_modified(L,[]).

decode_modified([{N,H}|T],L)->
    decode_modified(T,decode_modified(N,H,L));
decode_modified([],L)->
    p05:reverse(L);
decode_modified([H|T],L)->
    decode_modified(T,[H|L]).

decode_modified(1,H,L)->
    [H|L];
decode_modified(N,H,L)->
    decode_modified(N-1,H,[H|L]).
