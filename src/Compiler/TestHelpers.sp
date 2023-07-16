

#
# Modules and refs
#
moduleName as Text =
    "(test)"

errorModule as fn Text: Error.Module =
    fn content:
    { fsPath = "<Test>", content }

source as Meta.Source =
    Meta.SourceDirId "<Test>"


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


resErrorToStrippedText as fn Res a: Result Text a =
    fn res:

    errorToText =
        fn e:
        e
        >> Error.toFormattedText
        >> formattedToStrippedText

    Result.mapError errorToText res


#
# Meta
#
meta as Meta =

    metaResult =
        DefaultModules.asText
        >> ModulesFile.textToMeta "DefaultModules" __
        >> Result.mapError (fn e: e >> Error.toFormattedText >> formattedToStrippedText) __

    try metaResult as
        , Err e:
            log ("Error in DefaultModules.sp: " .. e) None
            todo "error loading DefaultModules.sp"
        , Ok m:
            m


#
# TA Types
#
taTyvar as fn Int: TA.RawType =
    TA.TypeVar __


taTyvarImm as fn Int: TA.RawType =
    TA.TypeVar __


taNumber as TA.RawType =
    TA.TypeOpaque CoreTypes.numberUsr []


taNone as TA.RawType =
    TA.TypeUnion Nothing CoreTypes.noneCons


taBool as TA.RawType =
    TA.TypeUnion Nothing CoreTypes.boolCons


taList as fn TA.RawType: TA.RawType =
    fn item:

    Dict.empty
    >> Dict.insert CoreTypes.nil [] __
    >> Dict.insert CoreTypes.cons [ item, TA.TypeRecursive CoreTypes.listUsr [ item ] ] __
    >> TA.TypeUnion Nothing __


taFunction as fn [TA.RawType], TA.RawType: TA.RawType =
    fn from, to:
    TA.TypeFn (List.map (fn t: TA.ParSp (toImm t)) from) (toImm to)

