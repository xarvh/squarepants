

#
# Modules and refs
#
moduleName as Text =
    "(test)"


source as Meta.Source =
    Meta.SourceDir "<Test>"


moduleUmr as UMR =
    UMR source moduleName


localType as Name: USR =
    name:
    USR moduleUmr name


rootLocal as Name: Ref =
    name:
    RefGlobal << USR moduleUmr name


#
# Meta
#
meta as Meta =

    eenv as Error.Env = {
        , moduleByName =
            Dict.singleton "DefaultModules" {
                , fsPath = "<DefaultModules>"
                , content = DefaultModules.asText
                }
        }

    metaResult =
        DefaultModules.asText
            >> ModulesFile.textToMeta "DefaultModules"
            >> Result.mapError (e: e >> Error.toFormattedText eenv >> formattedToStrippedText)

    try metaResult as
        Err e:
            log ("Error in DefaultModules.sp: " .. e) None
            todo "error loading DefaultModules.sp"
        Ok m:
            m


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
# Same as core types, but have Pos.T rather than Pos.N
#
caBool as CA.Type =
    CA.Type Pos.T << CA.TypeNamed (Meta.spCoreUSR "Bool") (CA.UniIsFixed Imm) []


caNumber as CA.Type =
    CA.Type Pos.T << CA.TypeNamed (Meta.spCoreUSR "Number") (CA.UniIsFixed Imm) []


caNone as CA.Type =
    CA.Type Pos.T << CA.TypeNamed (Meta.spCoreUSR "None") (CA.UniIsFixed Imm) []


caList as CA.Type: CA.Type =
    itemType:
    CA.Type Pos.T << CA.TypeNamed (Meta.spCoreUSR "List") (CA.UniIsFixed Imm) [ itemType ]


#
# TA Types
#
taTyvar as Int: TA.Type =
    id:
    TA.TypeVar id


taTyvarImm as Int: TA.Type =
    # TODO same as above
    id:
    TA.TypeVar id


taNumber as TA.Type =
    TA.TypeExact (TA.UniIsFixed TA.ForceImm) ("Number" >> Meta.spCoreUSR) []


taNumberAllowUni as TA.Type =
    TA.TypeExact (TA.UniIsFixed TA.AllowUni) ("Number" >> Meta.spCoreUSR) []


taNone as TA.Type =
    TA.TypeExact (TA.UniIsFixed TA.ForceImm) ("None" >> Meta.spCoreUSR) []


taBool as TA.Type =
    TA.TypeExact (TA.UniIsFixed TA.ForceImm) ("Bool" >> Meta.spCoreUSR) []


taList as TA.Type: TA.Type =
    item:
    TA.TypeExact (TA.UniIsFixed TA.ForceImm) ("List" >> Meta.spCoreUSR) [item]


taFunction as [TA.Type]: TA.Type: TA.Type =
    from: to:
    TA.TypeFn (List.map (t: Spend & t) from) to

