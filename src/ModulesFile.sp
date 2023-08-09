

alias ModulesFile = {
    , sourceDirs as [SourceDir]
    , libraries as [Library]
    #, platform as ....?
    }


initModulesFile as ModulesFile = {
    , sourceDirs = []
    , libraries = []
    }


alias SourceDir = {
    , path as Text
    , modules as [Module]
    }


alias Library = {
    , source as Text
    , modules as [Module]
    }


alias Module = {
    , path as Text
    # TODO: renameTo?
    , visibleAs as Text
    , globalValues as [Text]
    , globalTypes as [Text]
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
            , "core:prelude": Meta.Core
            , "core:browser": Meta.Browser
            , "core:posix": Meta.Posix
            , _: todo << "Library source `" .. lib.source .. "` is not supported."

    List.for meta lib.modules (insertModule umr __ __)


insertModules as fn SourceDir, Meta: Meta =
    fn sd, m:

    n = m.sourceDirIdCounter + 1
    id = "sd" .. Text.fromNumber n

    { m with
    , sourceDirIdCounter = n
    , sourceDirIdToPath = Dict.insert id sd.path .sourceDirIdToPath
    }
    >> List.for __ sd.modules (insertModule (Meta.SourceDirId id) __ __)


insertModule as fn Meta.Source, Module, Meta: Meta =
    fn source, mod, meta:

    visibleAs =
        # TODO fail if visibleAs is used already
        # TODO test that is well-formed
        mod.visibleAs

    umr =
        UMR source mod.path

    insertGlobal =
       fn varName, d:
       # TODO fail if varName is used already
       varName
       # TODO should probably split on module load instead
       >> USR umr __
       >> Dict.insert varName __ d

    { meta with
    , globalValues = List.for meta.globalValues mod.globalValues insertGlobal
    , globalTypes = List.for meta.globalTypes mod.globalTypes insertGlobal
    , moduleVisibleAsToUmr = Dict.insert visibleAs umr meta.moduleVisibleAsToUmr
    , umrToModuleVisibleAs = Dict.insert umr visibleAs meta.umrToModuleVisibleAs
    }



#
# Reader
#


union RootEntry =
    , Lib Library
    , Dir SourceDir


globalValue as SPON.Reader Text =
    SPON.oneOf [ SPON.lowerName, SPON.constructor ]



moduleReader as SPON.Reader Module =
    SPON.field "path" SPON.upperName >> SPON.onAcc fn path:
    SPON.maybe (SPON.field "importAs" SPON.upperName) >> SPON.onAcc fn visibleAs:
    SPON.maybe (SPON.field "globalTypes" (SPON.many SPON.upperName)) >> SPON.onAcc fn globalTypes:
    SPON.maybe (SPON.field "globalValues" (SPON.many globalValue)) >> SPON.onAcc fn globalValues:
    SPON.return
        { path = path
        , visibleAs = Maybe.withDefault path visibleAs
        , globalTypes = Maybe.withDefault [] globalTypes
        , globalValues = Maybe.withDefault [] globalValues
        }


libraryReader as SPON.Reader Library =
    SPON.field "source" SPON.text >> SPON.onAcc fn source:
    SPON.many (SPON.field "module" moduleReader) >> SPON.onAcc fn modules:
    SPON.return
        { source = source
        , modules = modules
        }


sourceDirectoryReader as SPON.Reader SourceDir =
    SPON.field "path" SPON.text >> SPON.onAcc fn path:
    SPON.many (SPON.field "module" moduleReader) >> SPON.onAcc fn modules:
    SPON.return
        { path = path
        , modules = modules
        }


modulesFileReader as SPON.Reader [RootEntry] =
    [ (SPON.field "library" libraryReader) >> SPON.onAcc (fn lib: SPON.return << Lib lib)
    , (SPON.field "sourceDir" sourceDirectoryReader) >> (SPON.onAcc fn dir: SPON.return << Dir dir)
    ]
    >> SPON.oneOf
    >> SPON.many


textToModulesFile as fn Text, Text: Res ModulesFile =
    fn sponName, sponContent:
    insert as fn RootEntry, ModulesFile: ModulesFile =
        fn rootEntry, mf:
        try rootEntry as
            , Lib lib:
                { mf with libraries = lib :: mf.libraries }

            , Dir dir:
                { mf with sourceDirs = dir :: mf.sourceDirs }

    sponContent
        >> SPON.read modulesFileReader sponName __
        >> Result.map (fn rootEntries: List.for initModulesFile rootEntries insert) __

