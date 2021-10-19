
unindent multilineText =
    is Text -> Text

    lines =
        Text.lines multilineText

    countLeadingSpaces line =
        is Text -> Int
        try Text.uncons line as
            Nothing:
                0

            Just ( char & xs ):
                try char as
                    " ":
                        1 + countLeadingSpaces xs

                    _:
                        0

    minLead =
        lines
            >> List.filter (Text.any ((/=) " "))
            >> List.map countLeadingSpaces
            >> List.minimum
            >> Maybe.withDefault 0

    lines
        >> List.map (Text.dropLeft minLead)
        >> Text.join "\n"


errorToText eenv e =
    is Error.Env -> Error -> Text

    Error.flatten e []
        >> List.map (Error.toText eenv)
        >> Text.join "\n\n"


resErrorToText code result =
    is Text -> Res a -> Result Text a

    eenv =
        { #metaFile = { sourceDirs = [], libraries = [] }
        , moduleByName = Dict.singleton "Test" { fsPath = "<TestPath>", content = unindent code }
        }

    Result.mapError (errorToText eenv)

