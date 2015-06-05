-module(p14).
-export([duplicate/1]).

duplicate(L)->
    duplicate(L,[]).

duplicate([H|T],L)->
    duplicate(T,[H,H|L]);
duplicate([],L)->
    p05:reverse(L).
