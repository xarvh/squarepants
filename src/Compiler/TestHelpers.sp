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
    'UMR Meta.'user 0 "<TestModulePath>"


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
        , getSourceDirId = fn importsDir, sourceDir: 1
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
exports as Meta.Exports =
    isOpen =
        'false

    [
    , "None" & { isOpen = 'true, usr = CoreDefs.noneTypeUsr }
    , "'none" & { isOpen, usr = CoreDefs.noneConsUsr }
    #
    , "Bool"
    & { isOpen = 'true, usr = CoreDefs.boolUsr }
    , "'true" & { isOpen, usr = CoreDefs.trueUsr }
    , "'false" & { isOpen, usr = CoreDefs.falseUsr }
    #
    , "List"
    & { isOpen = 'true, usr = CoreDefs.listUsr }
    , "'nil" & { isOpen, usr = CoreDefs.nilUsr }
    , "'cons" & { isOpen, usr = CoreDefs.consUsr }
    #
    , "Text"
    & { isOpen, usr = CoreDefs.textUsr }
    , "Number" & { isOpen, usr = CoreDefs.numberUsr }
    ]
    >> Dict.fromList
    >> Dict.ofOne "Core" __


loadExports as fn Meta.ImportsPath: Res Meta.Exports =
    fn ip:
    try ip as

        Meta.'importsPath Meta.'core "":
            'ok exports

        _:
            [
            , "TestHelpers: trying to load an unknown export:"
            , ""
            , Debug.toHuman ip
            , ""
            ]
            >> Error.'raw
            >> 'err


resolvePars as Meta.ResolvePars Error =
    {
    , currentImports = imports
    , currentModule = moduleUmr
    , loadExports
    , makeError = Error.'raw __
    }


resolveToUsr as fn Pos, Maybe Name, Name: Res USR =
    fn pos, maybeModule, name:
    Meta.resolve resolvePars maybeModule name


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
    TA.'typeVar Pos.'t __


taTyvarImm as fn Int: TA.RawType =
    TA.'typeVar Pos.'t __


taNumber as TA.RawType =
    TA.'typeExact Pos.'t (CoreDefs.usr "Number") []


taNone as TA.RawType =
    TA.'typeExact Pos.'t (CoreDefs.usr "None") []


taBool as TA.RawType =
    TA.'typeExact Pos.'t (CoreDefs.usr "Bool") []


taList as fn TA.RawType: TA.RawType =
    fn item:
    TA.'typeExact Pos.'t (CoreDefs.usr "List") [ item ]


taFunction as fn Int, [ TA.RawType ], TA.RawType: TA.RawType =
    fn id, from, to:
    TA.'typeFn Pos.'t id (List.map (fn t: TA.'parSp (toImm t)) from) (toImm to)
