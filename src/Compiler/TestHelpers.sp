#
# Modules and refs
#
moduleName as Text =
    "(test)"


errorModule as fn Text: Error.Module =
    fn content:
    { content, fsPath = "<Test>" }


importsPath as Meta.ImportsPath =
    Meta.'importsPath Meta.'user "<testImportsPath/>"


moduleUmr as UMR =
    'UMR importsPath "<testSourceDir/>" "<TestModulePath>"


moduleUsr as fn Name: USR =
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
imports as Imports =
    pars as ImportsFile.ToImportsPars =
        {
        , importsPath
        , joinPath = Path.join
        }

    try ImportsFile.toImports pars DefaultImports.defaultImportsFile as

        'err e:
            log "Error in DefaultImports.sp: " e

            todo "error loading DefaultImports.sp"

        'ok m:
            m


#
# Resolve Pars
#
resolvePars as Meta.ResolvePars Error =
    {
    , currentImports = imports
    , currentModule = moduleUmr
    , loadExports = fn ip: 'err << Error.'raw [ "TestHelpers: trying to loadExports?" ]
    , makeError = Error.'raw __
    }


#
# CA Types
#
# These are necessary because some tests will expect Pos T instead of N
#
caBool as CA.RawType =
    CA.'typeNamed Pos.'t (CoreDefs.usr "Bool") []


caNumber as CA.RawType =
    CA.'typeNamed Pos.'t (CoreDefs.usr "Number") []


caNone as CA.RawType =
    CA.'typeNamed Pos.'t (CoreDefs.usr "None") []


caList as fn CA.RawType: CA.RawType =
    fn itemType:
    CA.'typeNamed Pos.'t (CoreDefs.usr "List") [ itemType ]


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
    TA.'typeExact (CoreDefs.usr "Number") []


taNone as TA.RawType =
    TA.'typeExact (CoreDefs.usr "None") []


taBool as TA.RawType =
    TA.'typeExact (CoreDefs.usr "Bool") []


taList as fn TA.RawType: TA.RawType =
    fn item:
    TA.'typeExact (CoreDefs.usr "List") [ item ]


taFunction as fn [ TA.RawType ], TA.RawType: TA.RawType =
    fn from, to:
    TA.'typeFn (List.map (fn t: TA.'parSp (toImm t)) from) (toImm to)
