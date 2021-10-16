#
# TODO I don't know yet how I want to implement sequential text access in a way that allows pattern matching
# so for now I'll use this module to hide the fact that it's very inefficient
#
alias Buffer =
    {
    , tailList is [Text]
    , tailText is Text
    # fullText is used only by slice. Should it actually live here?
    , fullText is Text
    , pos is Int
    }


init s =
    is Text -> Buffer
    {
    , tailList = Text.split "" s
    , tailText = s
    , fullText = s
    , pos = 0
    }


pos b =
    is Buffer -> Int

    b.pos


skipAheadBy length b =
    is Int -> Buffer -> Buffer
    { b with
    , tailList = List.drop length b.tailList
    , tailText = Text.slice 0 length b.tailText
    , pos = b.pos + length
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
