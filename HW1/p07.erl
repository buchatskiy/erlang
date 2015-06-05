-module(p07).
-export([flatten/1]).

flatten(L)->
    flatten(L,[]).
flatten([H|[]],L)->
    flatten(H,L);
flatten([H|T],L)->
    flatten(H,T,L);
flatten(H,L)->
    p05:reverse([H|L]).
flatten([H|T],T2,L)->
    flatten(H,[T|T2],L);
flatten([],T2,L)->
    flatten(T2,L);
flatten(H,T2,L)->
    flatten(T2,[H|L]).

