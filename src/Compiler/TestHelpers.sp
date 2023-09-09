#
# Modules and refs
#
moduleName as Text =
    "(test)"


errorModule as fn Text: Error.Module =
    fn content:
    { content, fsPath = "<Test>" }


source as Meta.Source =
    Meta.'sourceDirId "<Test>"


moduleUmr as UMR =
    'UMR source moduleName


localType as fn Name: USR =
    'USR moduleUmr __


rootLocal as fn Name: Ref =
    fn name:
    'refGlobal << 'USR moduleUmr name


#
# Errors
#
formattedToStrippedText as fn [ Error.FormattedText ]: Text =
    fn formatted:
    strip as fn Error.FormattedText: Text =
        fn fmt:
        try fmt as
            Error.'formattedText_Default t: t
            Error.'formattedText_Emphasys t: t
            Error.'formattedText_Warning t: t
            Error.'formattedText_Decoration t: t

    formatted
    >> List.map strip __
    >> Text.join "" __


errorToStrippedText as fn Error: Text =
    __
    >> Error.toFormattedText
    >> formattedToStrippedText


resErrorToStrippedText as fn Res a: Result Text a =
    Result.mapError errorToStrippedText __


#
# Meta
#
meta as Meta =
    metaResult =
        DefaultModules.asText
        >> ModulesFile.textToMeta "DefaultModules" __
        >> Result.mapError (fn e: e >> Error.toFormattedText >> formattedToStrippedText) __

    try metaResult as

        'err e:
            log ("Error in DefaultModules.sp: " .. e) 'none

            todo "error loading DefaultModules.sp"

        'ok m:
            m


#
# CA Types
#
# These are necessary because some tests will expect Pos T instead of N
#
caBool as CA.RawType =
    CA.'typeNamed Pos.'t (Meta.spCoreUSR "Bool") []


caNumber as CA.RawType =
    CA.'typeNamed Pos.'t (Meta.spCoreUSR "Number") []


caNone as CA.RawType =
    CA.'typeNamed Pos.'t (Meta.spCoreUSR "None") []


caList as fn CA.RawType: CA.RawType =
    fn itemType:
    CA.'typeNamed Pos.'t (Meta.spCoreUSR "List") [ itemType ]


caFunction as fn [ CA.RawType ], CA.RawType: CA.RawType =
    fn from, to:
    CA.'typeFn Pos.'t (List.map (fn t: CA.'parSp (toImm t)) from) (toImm to)


#
# TA Types
#
taTyvar as fn Int: TA.RawType =
    TA.'typeVar __


taTyvarImm as fn Int: TA.RawType =
    TA.'typeVar __


taNumber as TA.RawType =
    TA.'typeExact ("Number" >> Meta.spCoreUSR) []


taNone as TA.RawType =
    TA.'typeExact ("None" >> Meta.spCoreUSR) []


taBool as TA.RawType =
    TA.'typeExact ("Bool" >> Meta.spCoreUSR) []


taList as fn TA.RawType: TA.RawType =
    fn item:
    TA.'typeExact ("List" >> Meta.spCoreUSR) [ item ]


taFunction as fn [ TA.RawType ], TA.RawType: TA.RawType =
    fn from, to:
    TA.'typeFn (List.map (fn t: TA.'parSp (toImm t)) from) (toImm to)
