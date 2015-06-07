-module(bs03).
-export([split/2]).
split(BinaryData, Spl)->
    split(BinaryData, [], <<>>, list_to_binary(Spl)).
split(<<X, B/binary>>, L, Acc, <<X, Spl2/binary>>)->
    check(B, L, Acc, Spl2, <<X, Spl2/binary>>, <<Acc/binary, X>>);
split(<<S, B/binary>>, L, Acc, Spl)->
    split(B, L, <<Acc/binary, S>>, Spl);
split(<<_/binary>>, L, Acc, _)->
    reverse([Acc|L]).

check(B, L, Acc, <<>>, Spl0, _)->
    split(B, [Acc|L], <<>>, Spl0);
check(<<X, B/binary>>, L, Acc, <<X, Spl/binary>>, Spl0, Acc2)->
    check(B, L, Acc, Spl, Spl0, <<Acc2/binary, X>>);
check(B, L, _, _, Spl0, Acc2)->
    split(B, L, Acc2, Spl0).

reverse(L)->
    reverse(L,[]).
reverse([],L)->
    L;
reverse([H|T],L)->
    reverse(T,[H|L]).
