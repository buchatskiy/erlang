-module(bs01).
-export([first_word/1]).
first_word(BinaryData)->
    first_word(BinaryData,<<>>).
first_word(<<" ",_/binary>>, Acc)->
    Acc;
first_word(<<S,B/binary>>, Acc)->
    first_word(B,<<Acc/binary, S>>).
