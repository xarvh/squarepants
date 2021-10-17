#
# TODO I don't know yet how I want to implement sequential text access in a way that allows pattern matching
# so for now I'll use this module to hide the fact that it's very inefficient
#
alias Buffer =
    {
    , pos is Int
    , tailList is [Text]
    , tailText is Text
    , fullSize is Int
    # fullText is used only by slice. Should it actually live here?
    , fullText is Text
    }


init s =
    is Text -> Buffer

    assert ((List.length << Text.split "" s) == Text.length s) "BLAH"

    {
    , pos = 0
    , tailList = Text.split "" s
    , tailText = s
    , fullSize = Text.length s
    , fullText = s
    }


pos b =
    is Buffer -> Int

    b.pos


skipAheadBy length b =
    is Int -> Buffer -> Buffer

    d = min length (b.fullSize - b.pos)

    { b with
    , tailList = List.drop d .tailList
    , tailText = Text.slice 0 d .tailText
    , pos = .pos + d
    }


skipAheadTo endPos b =
    is Int -> Buffer -> Buffer

    skipAheadBy (max 0 (endPos - b.pos)) b


next b =
    is Buffer -> Text

    try b.tailList as
        []: ""
        h :: _: h




startsWith sub b =
    is Text -> Buffer -> Maybe Buffer

    if Text.startsWith sub b.tailText:
        Just << skipAheadBy (Text.length sub) b
    else
        Nothing


regexMatch regex b =
    is Text -> Buffer -> Maybe (Text & Buffer)

    # TODO use try..as once it is fixed
    match = Text.startsWithRegex regex b.tailText
    if match == "":
        Nothing
    else
        Just << match & skipAheadBy (Text.length match) b

atEnd b =
    is Buffer -> Bool

    b.tailText == ""


slice start end b =
    is Int -> Int -> Buffer -> Text

    Text.slice start end b.fullText
