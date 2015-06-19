-module(bs03).
-export([split/2]).
split(BinaryData, Spl)->
    split(BinaryData, [], <<>>, Spl).
split(<<X, B/binary>>, L, Acc, <<X, Spl/binary>>)->
    Size=byte_size(Spl),
    Spl0 = <<X, Spl/binary>>,
    case B of
        <<Spl:Size/binary, T/binary>>->
            split(T, [Acc|L], <<>>, Spl0);
        <<S, T/binary>>->
            split(T, L, <<Acc/binary, X, S>>, Spl0)
    end;
split(<<S, B/binary>>, L, Acc, Spl)->
    split(B, L, <<Acc/binary, S>>, Spl);
split(<<_/binary>>, L, Acc, _)->
    reverse([Acc|L]).

reverse(L)->
    reverse(L,[]).
reverse([],L)->
    L;
reverse([H|T],L)->
    reverse(T,[H|L]).
