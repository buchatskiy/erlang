-module(bs02).
-export([words/1]).
words(BinaryData)->
    words(BinaryData,[],<<>>).
words(<<" ",B/binary>>, L, Acc)->
    words(B, [Acc|L], <<>>);
words(<<S, B/binary>>, L, Acc)->
    words(B, L, <<Acc/binary, S>>);
words(<<_/binary>>, L, Acc)->
    reverse([Acc|L]).

reverse(L)->
    reverse(L,[]).
reverse([],L)->
    L;
reverse([H|T],L)->
    reverse(T,[H|L]).
