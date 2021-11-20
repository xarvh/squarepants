
# TODO rename to modulesFile?
alias ModulesFile = {
    , sourceDirs as [SourceDir]
    , libraries as [Library]
    }


initMetaFile =
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


stringToMeta s =
    as Text: Res Meta
    s >> stringToMetaFile "modules.sp" >> Result.map toMeta


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

    path =
        if source == Meta.Core:
            []
        else
            Text.split "/" mod.path

    visibleAs =
        # TODO fail if visibleAs is used already
        # TODO test that is well-formed
        mod.visibleAs

    umr =
        Meta.UMR source path

    insertGlobal varName =
       # TODO fail if varName is used already
       varName
           # TODO should probably split on module load instead
           >> Meta.USR source path
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
    (SPON.field "path" SPON.varName) >> SPON.onAcc fn path:
    (SPON.maybe << SPON.field "importAs" SPON.varName) >> SPON.onAcc fn visibleAs:
    (SPON.maybe << SPON.field "globalTypes" (SPON.many SPON.varName)) >> SPON.onAcc fn globalTypes:
    (SPON.maybe << SPON.field "globalValues" (SPON.many SPON.varName)) >> SPON.onAcc fn globalValues:
    SPON.return
        { path = path
        , visibleAs = Maybe.withDefault path visibleAs
        , globalTypes = Maybe.withDefault [] globalTypes
        , globalValues = Maybe.withDefault [] globalValues
        }


libraryReader =
    as SPON.Reader Library
    (SPON.field "source" SPON.string) >> SPON.onAcc fn source:
    (SPON.many << SPON.field "module" moduleReader) >> SPON.onAcc fn modules:
    SPON.return
        { source = source
        , modules = modules
        }


sourceDirectoryReader =
    as SPON.Reader SourceDir
    SPON.field "path" SPON.string >> SPON.onAcc fn path:
    SPON.many (SPON.field "module" moduleReader) >> SPON.onAcc fn modules:
    SPON.return
        { path = path
        , moduleExceptions = modules
        }


metaFileReader =
    as SPON.Reader [RootEntry]
    [ (SPON.field "library" libraryReader) >> SPON.onAcc fn lib: SPON.return << Lib lib
    , (SPON.field "sourceDir" sourceDirectoryReader) >> SPON.onAcc fn dir: SPON.return << Dir dir
    ]
        >> SPON.oneOf
        >> SPON.many


stringToMetaFile sponName sponContent =
    as Text: Text: Res ModulesFile

    insert rootEntry mf =
        as RootEntry: ModulesFile: ModulesFile
        try rootEntry as
            Lib lib:
                { mf with libraries = lib :: mf.libraries }

            Dir dir:
                { mf with sourceDirs = dir :: mf.sourceDirs }

    statementsToResMetaFile statements =
        as [FA.Statement]: Res ModulesFile
        try SPON.run metaFileReader statements as
            Err textMessage:
                Error.res Pos.E (fn _: [ textMessage ])

            Ok rootEntries:
                Ok << List.foldl insert rootEntries initMetaFile

    sponContent
        #  TODO These passes should be part of SPON
        >> Compiler/Lexer.lexer sponName
        >> Result.andThen (Compiler/Parser.parse False sponName)
        >> Result.andThen statementsToResMetaFile
