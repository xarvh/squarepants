

stripLocations as Bool =
    True


#
# Modules and refs
#
moduleName as Text =
    "(test)"


moduleUmr as Meta.UniqueModuleReference =
    Meta.UMR (Meta.SourceDir "<Test>") moduleName


localType as Name: Meta.UniqueSymbolReference =
    name:
    Meta.USR moduleUmr name


rootLocal as Name: CA.Ref =
    name:
    CA.RefRoot << Meta.USR moduleUmr name


#
# Meta
#
defaultMeta as Meta =

    Meta.init

#    eenv as Error.Env = {
#        , moduleByName =
#            Dict.singleton "DefaultModules" {
#                , fsPath = "DefaultModules.sp"
#                , content = DefaultModules.asText
#                }
#        }
#
#    metaResult =
#        DefaultModules.asText
#            >> ModulesFile.textToMeta "DefaultModules"
#            >> Result.mapError (e: e >> Error.toFormattedText eenv >> formattedToStrippedText)
#
#    try metaResult as
#        Err e:
#            log ("Error in DefaultModules.sp: " .. e) None
#            Debug.todo "error loading DefaultModules.sp"
#        Ok m:
#            m


#
# Errors
#
formattedToStrippedText as [Error.FormattedText]: Text =
    formatted:

    strip as Error.FormattedText: Text =
        fmt:
        try fmt as
            Error.FormattedText_Default t: t
            Error.FormattedText_Emphasys t: t
            Error.FormattedText_Warning t: t
            Error.FormattedText_Decoration t: t

    formatted
        >> List.map strip
        >> Text.join ""


dummyErrorEnv as Text: Error.Env =
    code:
    {
    #, metaFile = { sourceDirs = [], libraries = [] }
    , moduleByName = Dict.singleton moduleName { fsPath = "<TestPath>", content = code }
    }


resErrorToStrippedText as Text: Res a: Result Text a =
    code:
    Result.mapError e:
        e
            >> Error.toFormattedText (dummyErrorEnv code)
            >> formattedToStrippedText


#
# Pipelines
#
#textToFormattableModule as Text: Res [FA.Statement] =
#    code:
#    tokensResult =
#        as Res [Token]
#        Compiler/Lexer.lexer moduleName code
#
#    tokensToStatsResult as [Token]: Res [FA.Statement] =
#        tokens:
#        Compiler/Parser.parse stripLocations moduleName tokens
#
#    onOk tokensToStatsResult tokensResult


#textToCanonicalModule as Text: Res CA.Module =
#    code:
#
#    env =
#        as Compiler/MakeCanonical.ReadOnly
#        {
#        , currentModule = moduleUmr
#        , meta = defaultMeta
#        }
#
#    code
#        >> textToFormattableModule
#        >> onOk (Compiler/MakeCanonical.translateModule env code moduleUmr)


#
# Same as core types, but have Pos.T rather than Pos.N
#
boolType as CA.Type =
    CA.TypeConstant Pos.T ("Bool" >> Meta.spCoreUSR) []


numberType as CA.Type =
    CA.TypeConstant Pos.T ("Number" >> Meta.spCoreUSR) []


noneType as CA.Type =
    CA.TypeConstant Pos.T ("None" >> Meta.spCoreUSR) []


listType as CA.Type: CA.Type =
    itemType:
    CA.TypeConstant Pos.T ("List" >> Meta.spCoreUSR) [ itemType ]

