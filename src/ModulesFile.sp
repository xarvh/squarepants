ModulesFile =
    {
    , libraries as [ Library ]
    , sourceDirs as [ SourceDir ]
    }


#, platform as ....?
initModulesFile as ModulesFile =
    {
    , libraries = []
    , sourceDirs = []
    }


SourceDir =
    {
    , modules as [ Module ]
    , path as Text
    }


Library =
    {
    , modules as [ Module ]
    , source as Text
    }


Module =
    {
    , globalTypes as [ Text ]
    , globalValues as [ Text ]
    , path as Text
    # TODO: renameTo?
    , visibleAs as Text
    }


textToMeta as fn Text, Text: Res Meta =
    fn sponName, sponContent:
    sponContent
    >> textToModulesFile sponName __
    >> Result.map toMeta __


toMeta as fn ModulesFile: Meta =
    fn mf:
    Meta.init
    >> List.for __ mf.libraries insertLibrary
    >> List.for __ mf.sourceDirs insertModules


insertLibrary as fn Library, Meta: Meta =
    fn lib, meta:
    umr =
        try lib.source as
            "core:prelude": Meta.'core
            "core:browser": Meta.'browser
            "core:posix": Meta.'posix
            _: todo << "Library source `" .. lib.source .. "` is not supported."

    List.for meta lib.modules (insertModule umr __ __)


insertModules as fn SourceDir, Meta: Meta =
    fn sd, m:
    n =
        m.sourceDirIdCounter + 1

    id =
        "sd" .. Text.fromNumber n

    { m with
    , sourceDirIdCounter = n
    , sourceDirIdToPath = Dict.insert id sd.path .sourceDirIdToPath
    }
    >> List.for __ sd.modules (insertModule (Meta.'sourceDirId id) __ __)


insertModule as fn Meta.Source, Module, Meta: Meta =
    fn source, mod, meta:
    visibleAs =
        # TODO fail if visibleAs is used already
        # TODO test that is well-formed
        mod.visibleAs

    umr =
        'UMR source mod.path

    insertGlobal =
        fn varName, d:
        # TODO fail if varName is used already
        varName
        # TODO should probably split on module load instead
        >> 'USR umr __
        >> Dict.insert varName __ d

    { meta with
    , globalTypes = List.for meta.globalTypes mod.globalTypes insertGlobal
    , globalValues = List.for meta.globalValues mod.globalValues insertGlobal
    , moduleVisibleAsToUmr = Dict.insert visibleAs umr meta.moduleVisibleAsToUmr
    , umrToModuleVisibleAs = Dict.insert umr visibleAs meta.umrToModuleVisibleAs
    }


#
# Reader
#

var RootEntry =
    , 'lib Library
    , 'dir SourceDir


globalValue as SPON.Reader Text =
    SPON.oneOf [ SPON.lowerName, SPON.constructor ]


moduleReader as SPON.Reader Module =
    SPON.field "path" SPON.upperName
    >> SPON.onAcc fn path:
    SPON.maybe (SPON.field "importAs" SPON.upperName)
    >> SPON.onAcc fn visibleAs:
    SPON.maybe (SPON.field "globalTypes" (SPON.many SPON.upperName))
    >> SPON.onAcc fn globalTypes:
    SPON.maybe (SPON.field "globalValues" (SPON.many globalValue))
    >> SPON.onAcc fn globalValues:
    SPON.return
        {
        , globalTypes = Maybe.withDefault [] globalTypes
        , globalValues = Maybe.withDefault [] globalValues
        , path = path
        , visibleAs = Maybe.withDefault path visibleAs
        }


libraryReader as SPON.Reader Library =
    SPON.field "source" SPON.text
    >> SPON.onAcc fn source:
    SPON.many (SPON.field "module" moduleReader)
    >> SPON.onAcc fn modules:
    SPON.return
        {
        , modules = modules
        , source = source
        }


sourceDirectoryReader as SPON.Reader SourceDir =
    SPON.field "path" SPON.text
    >> SPON.onAcc fn path:
    SPON.many (SPON.field "module" moduleReader)
    >> SPON.onAcc fn modules:
    SPON.return
        {
        , modules = modules
        , path = path
        }


modulesFileReader as SPON.Reader [ RootEntry ] =
    [
    , SPON.field "library" libraryReader >> SPON.onAcc (fn lib: SPON.return << 'lib lib)
    , SPON.field "sourceDir" sourceDirectoryReader >> SPON.onAcc (fn dir: SPON.return << 'dir dir)
    ]
    >> SPON.oneOf
    >> SPON.many


textToModulesFile as fn Text, Text: Res ModulesFile =
    fn sponName, sponContent:
    insert as fn RootEntry, ModulesFile: ModulesFile =
        fn rootEntry, mf:
        try rootEntry as
            'lib lib: { mf with libraries = lib :: mf.libraries }
            'dir dir: { mf with sourceDirs = dir :: mf.sourceDirs }

    sponContent
    >> SPON.read modulesFileReader sponName __
    >> Result.map (fn rootEntries: List.for initModulesFile rootEntries insert) __
