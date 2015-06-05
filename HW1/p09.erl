-module(p09).
-export([pack/1]).

pack(L)->
    pack(L,[],[]).
pack([H,H|T],L1,L2)->
    pack([H|T],L1,[H|L2]);
pack([H|[]],L1,L2)->
    p05:reverse([[H|L2]|L1]);
pack([H|T],L1,L2)->
    pack(T,[[H|L2]|L1],[]).