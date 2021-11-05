
errorToText eenv e =
    as Error.Env: Error: Text

    Error.flatten e []
        >> List.map (fn (pos & descr): Error.toText eenv pos descr)
        >> Text.join "\n\n"


resErrorToText code =
    as Text: Res a: Result Text a

    eenv =
        { #metaFile = { sourceDirs = [], libraries = [] }
        , moduleByName = Dict.singleton "Test" { fsPath = "<TestPath>", content = code }
        }

    Result.mapError (errorToText eenv)


textToFormattableModule code =
    as Text: Res [FA.Statement]

    tokensResult =
        as Res [Token]
        Compiler/Lexer.lexer "Test" code

    tokensToStatsResult tokens =
        as [Token]: Res [FA.Statement]
        Compiler/Parser.parse "Test" tokens

    Result.andThen tokensToStatsResult tokensResult

