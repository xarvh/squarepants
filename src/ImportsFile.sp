ImportsFile =
    {
    , libraries as [ Library ]
    , sourceDirs as [ SourceDir ]
    }


init as ImportsFile =
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
    , globals as [ Text ]
    , path as Text
    # TODO: renameTo?
    , visibleAs as Text
    }


var LibrarySource =
    , 'core
    , 'local Text
    , 'installed { address as Text, protocol as Text }


parseLibrarySource as fn Text: Result Text LibrarySource =
    fn sourceAsText:
    try Text.split ":" sourceAsText as
        [ "core" ]: 'ok 'core
        [ "local", path ]: 'ok << 'local path
        [ protocol, address, more... ]: 'ok << 'installed { address = Text.join ":" [address, more...], protocol }
        _: 'err << "invalid library source: " .. sourceAsText


#
# ImportsFile to Imports
#

ToImportsPars =
    {
    , importsPath as Meta.ImportsPath
    , joinPath as fn [ Text ]: Text
    , getSourceDirId as fn Text, Text: Int
    }


insertModules as fn fn Text: Meta.Location, @Array Text, [ Module ], Imports: Imports =
    fn getModulePathToLocation, @errors, modules, imports:
        List.for imports modules fn module, imp:
            location =
                getModulePathToLocation module.path

            # Insert module alias
            # TODO check that there is no duplication!
            moduleAliasToLocation =
                Dict.insert module.visibleAs location imp.moduleAliasToLocation

            modulePathToLocation =
                Dict.insert module.path location imp.modulePathToLocation

            # Insert globals
            globalNameToLocation =
                List.for imp.globalNameToLocation module.globals fn globalName, dict:
                    # TODO check that there is no duplication
                    Dict.insert globalName location dict

            { globalNameToLocation, moduleAliasToLocation, modulePathToLocation }


insertSourceDir as fn ToImportsPars, @Array Text, SourceDir, Imports: Imports =
    fn pars, @errors, sourceDir, imports:

    Meta.'importsPath rootDirectory importsDir =
        pars.importsPath

    sourceDirId =
        pars.getSourceDirId importsDir sourceDir.path

    modulePathToLocation as fn Text: Meta.Location =
        __
        >> 'UMR rootDirectory sourceDirId __
        >> Meta.'locationSourceDir

    insertModules modulePathToLocation @errors sourceDir.modules imports


insertLibrary as fn ToImportsPars, @Array Text, Library, Imports: Imports =
    fn pars, @errors, library, imports:
    try parseLibrarySource library.source as

        'err msg:
            Array.push @errors msg

            imports

        'ok librarySource:
            try librarySource as

                'core:
                    Meta.'locationLibrary CoreDefs.importsPath __

                'local libraryDir:
                    Meta.'importsPath rootDirectory currentImportsDir =
                        pars.importsPath

                    importsDir =
                        pars.joinPath [ currentImportsDir, libraryDir ]

                    Meta.'locationLibrary (Meta.'importsPath rootDirectory importsDir) __

                'installed { address, protocol }:
                    # TODO ensure that `address` is file-system friendly
                    importsDir =
                        pars.joinPath [ protocol, address ]

                    Meta.'locationLibrary (Meta.'importsPath Meta.'installed importsDir) __
            >> insertModules __ @errors library.modules imports


toImports as fn ToImportsPars, ImportsFile: Res Imports =
    fn pars, importsFile:
    # TODO this should be an Array Error, but we don't have Pos annotation on ModulesFile/SPON!
    !errors as Array Text =
        Array.fromList []

    meta =
        Meta.initImports
        >> List.for __ importsFile.libraries (insertLibrary pars @errors __ __)
        >> List.for __ importsFile.sourceDirs (insertSourceDir pars @errors __ __)

    errs =
        Array.toList @errors >> List.map (fn msg: Error.'raw [ msg ]) __

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
    SPON.maybe (SPON.field "globals" (SPON.many SPON.anyName))
    >> SPON.onAcc fn globals:
    SPON.return
        {
        , globals = Maybe.withDefault [] globals
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
        , modules
        , path
        }


modulesFileReader as SPON.Reader [ RootEntry ] =
    [
    , SPON.field "library" libraryReader >> SPON.onAcc (fn lib: SPON.return << 'lib lib)
    , SPON.field "sourceDir" sourceDirectoryReader >> SPON.onAcc (fn dir: SPON.return << 'dir dir)
    ]
    >> SPON.oneOf
    >> SPON.many


fromText as fn Text, Text: Res ImportsFile =
    fn sponName, sponContent:
    insert as fn RootEntry, ImportsFile: ImportsFile =
        fn rootEntry, mf:
        try rootEntry as
            'lib lib: { mf with libraries = lib :: mf.libraries }
            'dir dir: { mf with sourceDirs = dir :: mf.sourceDirs }

    sponContent
    >> SPON.read modulesFileReader sponName __
    >> Result.map (List.for init __ insert) __
