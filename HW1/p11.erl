-module(p11).
-export([encode_modified/1]).

encode_modified(L)->
    encode_modified(L,[],1).
encode_modified([H,H|T],L1,N)->
    encode_modified([H|T],L1,N+1);
encode_modified([H|[]],L1,1)->
    p05:reverse([H|L1]);
encode_modified([H|[]],L1,N)->
    p05:reverse([{N,H}|L1]);
encode_modified([H|T],L1,1)->
    encode_modified(T,[H|L1],1);
encode_modified([H|T],L1,N)->
    encode_modified(T,[{N,H}|L1],1).