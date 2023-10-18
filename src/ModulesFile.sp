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
    >> onOk toMeta


insertModule as fn @Array Text, Meta.SourceId, Module, Meta: Meta =
    fn @errors, sourceId, mod, meta:
    visibleAs =
        # TODO fail if visibleAs is used already
        # TODO test that is well-formed
        mod.visibleAs

    umr =
        'UMR sourceId mod.path

    insertGlobal =
        fn varName, d:
        # TODO fail if varName is used already
        varName
        # TODO should probably split on module load instead
        >> 'USR umr __
        >> Dict.insert varName __ d

    { meta with
    , globalTypes = List.for .globalTypes mod.globalTypes insertGlobal
    , globalValues = List.for .globalValues mod.globalValues insertGlobal
    , moduleVisibleAsToUmr = Dict.insert visibleAs umr .moduleVisibleAsToUmr
    , umrToModuleVisibleAs = Dict.insert umr visibleAs .umrToModuleVisibleAs
    }


parseLibrarySource as fn Text: Result Text Meta.Source =
    fn sourceAsText:
    try Text.split ":" sourceAsText as

        [ "core" ]:
            'ok Meta.'core

        [ "platform" ]:
            'ok Meta.'platform

        [ "local", path ]:
            'ok << Meta.'localLibrary path

        # TODO support non-local libraries
        _:
            'err << "invalid library source: " .. sourceAsText


insertSource as fn @Array Text, [ Module ], Meta.Source, Meta: Meta =
    fn @errors, modules, source, meta:
    id =
        meta.nextSourceId

    { meta with
    , nextSourceId = id + 1
    , sourceIdToSource = Dict.insert id source .sourceIdToSource
    }
    >> List.for __ modules (insertModule @errors id __ __)


toMeta as fn ModulesFile: Res Meta =
    fn mf:

    # TODO this should be an Array Error, but we don't have Pos annotation on ModulesFile/SPON!
    !errors as Array Text =
        Array.fromList []

    insertLibrary as fn Library, Meta: Meta =
        fn lib, meta:
        try parseLibrarySource lib.source as

            'err msg:
                Array.push @errors msg

                meta

            'ok source:
                insertSource @errors lib.modules source meta

    insertSourceDir as fn SourceDir, Meta: Meta =
        fn sourceDir, meta:
        insertSource @errors sourceDir.modules (Meta.'sourceDirectory sourceDir.path) meta

    meta =
        Meta.init
        >> List.for __ mf.libraries insertLibrary
        >> List.for __ mf.sourceDirs insertSourceDir

    errs =
        Array.toList @errors
        >> List.map (fn msg: Error.'raw [ msg ]) __

    if errs == [] then
        'ok meta
    else
        'err (Error.'nested errs)


#
# Reader
#

# TODO we need Pos otherwise we can't give proper errors!

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
