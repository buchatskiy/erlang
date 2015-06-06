-module(p07_new).
-export([flatten/1]).

flatten(L)->
    p05:reverse(flatten(L,[])).
flatten([[_|_]=H|T],L)->
    %io:format("H:~p,T:~p,L:~p",[H,T,L]),
    flatten(T,flatten(H,L));
flatten([H|T],L)->
    flatten(T,[H|L]);
flatten([],L)->
    L.
