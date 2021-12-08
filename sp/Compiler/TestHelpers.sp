

stripLocations =
    True


#
# Modules and refs
#
moduleName =
    "(test)"


moduleUmr =
    Meta.UMR Meta.SourcePlaceholder moduleName


localType name =
    as Name: Meta.UniqueSymbolReference
    Meta.USR TH.moduleUmr name


rootLocal name =
    as Name: CA.Ref
    CA.RefRoot << Meta.USR TH.moduleUmr name


#
# Meta
#
defaultMeta =
    as Meta

    eenv =
        as Error.Env
        {
        , moduleByName =
            Dict.singleton "DefaultModules" {
                , fsPath = "DefaultModules.sp"
                , content =  DefaultModules.asText
                }
        }

    metaResult =
        DefaultModules.asText
            >> ModulesFile.textToMeta "DefaultModules"
            >> Result.mapError (fn e: e >> Error.toFormattedText eenv >> formattedToStrippedText)

    try metaResult as
        Err e:
            log ("Error in DefaultModules.sp: " .. e) None
#            Meta.init
            Debug.todo "error loading DefaultModules.sp"
        Ok m:
            m


#
# Errors
#
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
    , moduleByName = Dict.singleton moduleName { fsPath = "<TestPath>", content = code }
    }


resErrorToStrippedText code =
    as Text: Res a: Result Text a

    Result.mapError fn e:
        e
            >> Error.toFormattedText (dummyErrorEnv code)
            >> formattedToStrippedText


#
# Pipelines
#
textToFormattableModule code =
    as Text: Res [FA.Statement]

    tokensResult =
        as Res [Token]
        Compiler/Lexer.lexer moduleName code

#    _ =
#        tokensResult >> Result.map fn tokens:
#            List.each tokens fn t:
#                log "*" t

    tokensToStatsResult tokens =
        as [Token]: Res [FA.Statement]
        Compiler/Parser.parse stripLocations moduleName tokens

    onOk tokensToStatsResult tokensResult


textToCanonicalModule code =
    as Text: Res CA.Module

    env =
        as Compiler/MakeCanonical.ReadOnly
        {
        , currentModule = moduleUmr
        , meta = defaultMeta
        }

    code
        >> textToFormattableModule
        >> onOk (Compiler/MakeCanonical.translateModule env moduleName)


#
# Same as core types, but have Pos.T rather than Pos.N
#
boolType =
    as CA.Type
    CA.TypeConstant Pos.T ("Bool" >> Meta.spCoreUSR) []


numberType =
    as CA.Type
    CA.TypeConstant Pos.T ("Number" >> Meta.spCoreUSR) []


noneType =
    as CA.Type
    CA.TypeConstant Pos.T ("None" >> Meta.spCoreUSR) []


listType itemType =
    as CA.Type: CA.Type
    CA.TypeConstant Pos.T ("List" >> Meta.spCoreUSR) [ itemType ]

