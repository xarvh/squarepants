

stripLocations =
        True


defaultMeta =
    as Meta

    ModulesFile.toMeta DefaultModules.asRecord
#    x =
#        Result.mapError fn e:
#            e
#                >> Error.toFormattedText { moduleByName = Dict.singleton "DefaultModules" { fsPath = "DefaultModules.sp", content =  DefaultModules.asText } }
#                >> formattedToStrippedText
#
#    try ModulesFile.stringToMetaFile "DefaultModules" DefaultModules.asText >> x as
#        Err e:
#            Debug.todo  << "Error in DefaultModules.sp: " .. e
#        Ok m:


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
        Compiler/Parser.parse stripLocations "Test" tokens

    Result.andThen tokensToStatsResult tokensResult


textToCanonicalModule code =
    as Text: Res CA.Module

    code
        >> textToFormattableModule
        >> Result.andThen (Compiler/MakeCanonical.translateModule { meta = defaultMeta })


#
# Same as core types, but have Pos.T rather than Pos.N
#
boolType =
    as CA.Type
    CA.TypeConstant Pos.T ("Bool" >> Meta.USR Meta.Core [] >> CA.Foreign) []


numberType =
    as CA.Type
    CA.TypeConstant Pos.T ("Number" >> Meta.USR Meta.Core [] >> CA.Foreign) []
