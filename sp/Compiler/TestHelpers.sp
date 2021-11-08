
formattedToStrippedText formatted =
    as [Error.FormattedText]: Text

    strip fmt =
        as Error.FormattedText: Text
        try fmt as
            Error.FormattedText_Default t: t
            Error.FormattedText_Emphasys t: t
            Error.FormattedText_Warning t: t
            Error.FormattedText_Decoration t: t

    formatted
        >> List.map strip
        >> Text.join ""


dummyErrorEnv code =
    as Text: Error.Env
    {
    #, metaFile = { sourceDirs = [], libraries = [] }
    , moduleByName = Dict.singleton "Test" { fsPath = "<TestPath>", content = code }
    }


resErrorToStrippedText code =
    as Text: Res a: Result Text a

    Result.mapError fn e:
        e
            >> Error.toFormattedText (dummyErrorEnv code)
            >> formattedToStrippedText


textToFormattableModule code =
    as Text: Res [FA.Statement]

    tokensResult =
        as Res [Token]
        Compiler/Lexer.lexer "Test" code

    tokensToStatsResult tokens =
        as [Token]: Res [FA.Statement]
        Compiler/Parser.parse "Test" tokens

    Result.andThen tokensToStatsResult tokensResult

