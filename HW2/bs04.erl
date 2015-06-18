-module(bs04).
-export([decode_xml/1]).

decode_xml(<<>>)->
    " ";
decode_xml(<<"<", B/binary>>)->
    decode_xml(B, <<>>);
decode_xml(<<S, B/binary>>)->
    case <<S>> of
        <<" ">> -> decode_xml(B);
        <<"\t">> -> decode_xml(B);
        <<"\n">> -> decode_xml(B);
        _->[<<S, B/binary>>]
    end.

decode_xml(<<">", B/binary>>, Tag)->
    get_end_tag(B, <<Tag/binary, ">">>, <<>>, Tag);
decode_xml(<<S, B/binary>>, Tag)->
    decode_xml(B, <<Tag/binary, S>>).

get_end_tag(<<"</", B/binary>>, Tag, Acc, Tag_text)->
    Size = byte_size(Tag),
    io:format("B: ~p, Tag: ~p\n", [B, <<Tag:Size/binary>>]),
    case B of
        <<Tag:Size/binary, T/binary>>->
            case decode_xml(T)=:=" " of
                true->{Tag_text, [], decode_xml(Acc)};
                _->[{Tag_text, [], decode_xml(Acc)}, decode_xml(T)] end;
        _ ->
            get_end_tag(B, Tag, <<Acc/binary, "</">>, Tag_text)
    end;
get_end_tag(<<S, B/binary>>, Tag, Acc, Tag_text)->
    get_end_tag(B, Tag, <<Acc/binary, S>>, Tag_text).