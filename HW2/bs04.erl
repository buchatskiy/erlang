-module(bs04).
-export([decode_xml/1]).

decode_xml(<<>>)->
    " ";
decode_xml(<<"<", B/binary>>)->
    decode_xml(B, <<>>);
decode_xml(<<" ", B/binary>>)->
    decode_xml(B);
decode_xml(<<"\t", B/binary>>)->
    decode_xml(B);
decode_xml(<<"\n", B/binary>>)->
    decode_xml(B);
decode_xml(<<S, B/binary>>)->
    [<<S, B/binary>>].
decode_xml(<<">", B/binary>>, Tag)->
    get_end_tag(B, <<Tag/binary, ">">>, <<>>, Tag);
decode_xml(<<S, B/binary>>, Tag)->
    decode_xml(B, <<Tag/binary, S>>).

get_end_tag(<<"</", B/binary>>, Tag, Acc, _)->
    check(B, Tag, Acc, Tag, <<Acc/binary, "</">>);
get_end_tag(<<S, B/binary>>, Tag, Acc, Tag_text)->
    get_end_tag(B, Tag, <<Acc/binary, S>>, Tag_text).

check(<<>>, <<>>, Acc, Tag_text, _)->
    {Tag_text, [], decode_xml(Acc)};
check(B, <<>>, Acc, Tag_text, _)->
    case decode_xml(B)=:=" " of
        true->{Tag_text, [], decode_xml(Acc)};
        _->[{Tag_text, [], decode_xml(Acc)}, decode_xml(B)] end;
check(<<X, B/binary>>, <<X, Tag/binary>>, Acc, Tag_text, Acc2)->
    check(B, Tag, Acc, Tag_text, <<Acc2/binary, X>>);
check(B, _, _, Tag_text, Acc2)->
    get_end_tag(B, Tag_text, Acc2, Tag_text).
