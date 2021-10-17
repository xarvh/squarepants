
unindent multilineText =
    is Text -> Text

    lines =
        Text.split "\n" multilineText

    countLeadingSpaces line =
        is Text -> Int

        line
          >> Text.startsWithRegex "[ ]*"
          >> Text.length

    minLead =
        lines
            >> List.filter (fn s: Text.trimLeft s /= "") #Text.any ((/=) " "))
            >> List.map countLeadingSpaces
            >> List.minimum
            >> Maybe.withDefault 0

    lines
        >> List.map (Text.dropLeft minLead)
        >> Text.join "\n"


errorToText eenv e =
    is Error.Env -> Error -> Text

    Error.flatten e []
        >> List.map (fn (pos & descr): Error.toText eenv pos descr)
        >> Text.join "\n\n"


resErrorToText code =
    is Text -> Res a -> Result Text a

    eenv =
        { #metaFile = { sourceDirs = [], libraries = [] }
        , moduleByName = Dict.singleton "Test" { fsPath = "<TestPath>", content = unindent code }
        }

    Result.mapError (errorToText eenv)
