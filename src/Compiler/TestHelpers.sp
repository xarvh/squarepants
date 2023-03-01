

#
# Modules and refs
#
moduleName as Text =
    "(test)"


source as Meta.Source =
    Meta.SourceDir "<Test>"


moduleUmr as UMR =
    UMR source moduleName


localType as fn Name: USR =
    USR moduleUmr __


rootLocal as fn Name: Ref =
    fn name:
    RefGlobal << USR moduleUmr name


#
# Errors
#
formattedToStrippedText as fn [Error.FormattedText]: Text =
    fn formatted:

    strip as fn Error.FormattedText: Text =
        fn fmt:
        try fmt as
            , Error.FormattedText_Default t: t
            , Error.FormattedText_Emphasys t: t
            , Error.FormattedText_Warning t: t
            , Error.FormattedText_Decoration t: t

    formatted
    >> List.map strip __
    >> Text.join "" __


dummyErrorEnv as fn Text: Error.Env =
    fn code:
    {
    #, metaFile = { sourceDirs = [], libraries = [] }
    , moduleByName = Dict.ofOne moduleName { fsPath = "<TestPath>", content = code }
    }


resErrorToStrippedText as fn Text, Res a: Result Text a =
    fn code, res:

    errorToText =
        fn e:
        e
        >> Error.toFormattedText (dummyErrorEnv code) __
        >> formattedToStrippedText

    Result.mapError errorToText res


#
# Meta
#
meta as Meta =

    eenv as Error.Env =
        {
        , moduleByName =
            Dict.ofOne "DefaultModules"
                {
                , fsPath = "<DefaultModules>"
                , content = DefaultModules.asText
                }
        }

    metaResult =
        DefaultModules.asText
        >> ModulesFile.textToMeta "DefaultModules" __
        >> Result.mapError (fn e: e >> Error.toFormattedText eenv __ >> formattedToStrippedText) __

    try metaResult as
        , Err e:
            log ("Error in DefaultModules.sp: " .. e) None
            todo "error loading DefaultModules.sp"
        , Ok m:
            m


#
# Same as core types, but have Pos.T rather than Pos.N
#
caBool as CA.RawType =
    CA.TypeNamed Pos.T (Meta.spCoreUSR "Bool") []


caNumber as CA.RawType =
    CA.TypeNamed Pos.T (Meta.spCoreUSR "Number") []


caNone as CA.RawType =
    CA.TypeNamed Pos.T (Meta.spCoreUSR "None") []


caList as fn CA.RawType: CA.RawType =
    fn itemType:
    CA.TypeNamed Pos.T (Meta.spCoreUSR "List") [ itemType ]


#
# TA Types
#
taTyvar as fn Int: TA.RawType =
    TA.TypeVar __


taTyvarImm as fn Int: TA.RawType =
    TA.TypeVar __


taNumber as TA.RawType =
    TA.TypeExact ("Number" >> Meta.spCoreUSR) []


taNone as TA.RawType =
    TA.TypeExact ("None" >> Meta.spCoreUSR) []


taBool as TA.RawType =
    TA.TypeExact ("Bool" >> Meta.spCoreUSR) []


taList as fn TA.RawType: TA.RawType =
    fn item:
    TA.TypeExact ("List" >> Meta.spCoreUSR) [item]


taFunction as fn [TA.RawType], TA.RawType: TA.RawType =
    fn from, to:
    TA.TypeFn (List.map (fn t: TA.ParSp (toImm t)) from) (toImm to)

