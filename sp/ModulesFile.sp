

alias ModulesFile = {
    , sourceDirs as [SourceDir]
    , libraries as [Library]
    }


initModulesFile =
    as ModulesFile
    {
    , sourceDirs = []
    , libraries = []
    }


alias SourceDir = {
    , path as Text
    , moduleExceptions as [Module]
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


textToMeta sponName sponContent =
    as Text: Text: Res Meta
    sponContent >> textToModulesFile sponName >> Result.map toMeta


toMeta mf =
    as ModulesFile: Meta

    Meta.init
        >> List.foldl insertLibrary mf.libraries
        >> List.foldl (insertModule Meta.SourcePlaceholder) (List.concatMap (fn x: x.moduleExceptions) mf.sourceDirs)


insertLibrary lib meta =
    as Library: Meta: Meta

    if lib.source /= "spcore":
        Debug.todo << "Library source `" .. lib.source .. "` is not supported."
    else
        List.foldl (insertModule Meta.Core) lib.modules meta



insertModule source mod meta =
    as Meta.Source: Module: Meta: Meta

#    path =
#        if source == Meta.Core:
#            Meta.spCorePath
#        else
#            mod.path

    visibleAs =
        # TODO fail if visibleAs is used already
        # TODO test that is well-formed
        mod.visibleAs

    umr =
        Meta.UMR source mod.path

    insertGlobal varName =
       # TODO fail if varName is used already
       varName
           # TODO should probably split on module load instead
           >> Meta.USR umr
           >> Dict.insert varName

    {
    , globalValues = List.foldl insertGlobal mod.globalValues meta.globalValues
    , globalTypes = List.foldl insertGlobal mod.globalTypes meta.globalTypes
    , moduleVisibleAsToUmr = Dict.insert visibleAs umr meta.moduleVisibleAsToUmr
    , umrToModuleVisibleAs = Dict.insert umr visibleAs meta.umrToModuleVisibleAs
    }



#
# Reader
#


union RootEntry =
    , Lib Library
    , Dir SourceDir


moduleReader =
    as SPON.Reader Module
    SPON.field "path" SPON.upperName >> SPON.onAcc fn path:
    SPON.maybe (SPON.field "importAs" SPON.upperName) >> SPON.onAcc fn visibleAs:
    SPON.maybe (SPON.field "globalTypes" (SPON.many SPON.upperName)) >> SPON.onAcc fn globalTypes:
    SPON.maybe (SPON.field "globalValues" (SPON.many SPON.lowerOrUpperName)) >> SPON.onAcc fn globalValues:
    SPON.return
        { path = path
        , visibleAs = Maybe.withDefault path visibleAs
        , globalTypes = Maybe.withDefault [] globalTypes
        , globalValues = Maybe.withDefault [] globalValues
        }


libraryReader =
    as SPON.Reader Library
    SPON.field "source" SPON.text >> SPON.onAcc fn source:
    SPON.many (SPON.field "module" moduleReader) >> SPON.onAcc fn modules:
    SPON.return
        { source = source
        , modules = modules
        }


sourceDirectoryReader =
    as SPON.Reader SourceDir
    SPON.field "path" SPON.text >> SPON.onAcc fn path:
    SPON.many (SPON.field "module" moduleReader) >> SPON.onAcc fn modules:
    SPON.return
        { path = path
        , moduleExceptions = modules
        }


modulesFileReader =
    as SPON.Reader [RootEntry]
    [ (SPON.field "library" libraryReader) >> SPON.onAcc fn lib: SPON.return << Lib lib
    , (SPON.field "sourceDir" sourceDirectoryReader) >> SPON.onAcc fn dir: SPON.return << Dir dir
    ]
        >> SPON.oneOf
        >> SPON.many


textToModulesFile sponName sponContent =
    as Text: Text: Res ModulesFile

    insert rootEntry mf =
        as RootEntry: ModulesFile: ModulesFile
        try rootEntry as
            Lib lib:
                { mf with libraries = lib :: mf.libraries }

            Dir dir:
                { mf with sourceDirs = dir :: mf.sourceDirs }

    sponContent
        >> SPON.read modulesFileReader sponName
        >> Result.map (fn rootEntries: List.foldl insert rootEntries initModulesFile)
