

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
caBool as CA.RawType =
    CA.TypeNamed Pos.T (Meta.spCoreUSR "Bool") []


caNumber as CA.RawType =
    CA.TypeNamed Pos.T (Meta.spCoreUSR "Number") []


caNone as CA.RawType =
    CA.TypeNamed Pos.T (Meta.spCoreUSR "None") []


caList as CA.RawType: CA.RawType =
    itemType:
    CA.TypeNamed Pos.T (Meta.spCoreUSR "List") [ itemType ]


#
# TA Types
#
taTyvar as Int: TA.RawType =
    id:
    TA.TypeVar id


taTyvarImm as Int: TA.RawType =
    id:
    TA.TypeVar id


taNumber as TA.RawType =
    TA.TypeExact ("Number" >> Meta.spCoreUSR) []


taNone as TA.RawType =
    TA.TypeExact ("None" >> Meta.spCoreUSR) []


taBool as TA.RawType =
    TA.TypeExact ("Bool" >> Meta.spCoreUSR) []


taList as TA.RawType: TA.RawType =
    item:
    TA.TypeExact ("List" >> Meta.spCoreUSR) [item]


taFunction as [TA.RawType]: TA.RawType: TA.RawType =
    from: to:
    TA.TypeFn (List.map (t: TA.ParSp (toImm t)) from) (toImm to)

