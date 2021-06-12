#
# Buffer
#
# I don't know yet how I want to implement text parsing, so I'll keep it abstract for now
#
alias Buffer =
    {
    , tail as Text
    , pos as Number
    }


init s =
    as Text -> Buffer
    {
    , tail = s
    , pos = 0
    }


pos b =
    as Buffer -> Number

    b.pos


consume l b =
    as Number -> Buffer -> Buffer

    {
    , tail = Text.slice 0 l b.tail
    , pos = b.pos + l
    }


startsWith sub b =
    as Text -> Buffer -> Maybe Buffer

    if Text.startsWith sub b.tail then
        Just << consume (Text.length sub) b
    else
        Nothing


regexMatch regex b =
    as Text -> Buffer -> Maybe (Text & Buffer)

    match = Text.startsWithRegex regex b.tail
    if match == "" then
        Nothing
    else
        Just << match & consume (Text.length match) b


#
# Lexer
#
lexer moduleName moduleCode =
    as Text -> Text -> Res [ Token ]

    Ok []


