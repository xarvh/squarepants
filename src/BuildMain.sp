#TODO rename to MainMake

importsFileName as Text =
    "imports.sp"


libDirectoryName as Text =
    "lib"


coreDirName as Text =
    "core"


GetModuleMetaAndPathPars =
    {
    , coreLibraryPath as Text
#    , platformSource as Source
    , projectImports as Imports
    , loadImports as fn Text: Res Imports
    }


getModuleMetaAndPath as fn GetModuleMetaAndPathPars, UMR: Res { meta as Imports, path as Text } =
    fn pars, umr:

    'UMR source name =
        umr

    try source as

        Meta.'core:
            {
            , meta = pars.coreLibraryMeta
            , path = Path.resolve [ pars.coreLibraryPath, coreDirName, Text.split "/" (name .. ".sp")... ]
            }
            >> 'ok

        Meta.'platform:
            getModuleMetaAndPath pars ('UMR pars.platformSource name)

        Meta.'userSourceDir path:
            {
            , meta = pars.projectMeta
            , path = Path.resolve [ path, name .. ".sp" ]
            }
            >> 'ok

        Meta.'userLibrary path:
            [ path, "modules.sp" ]
            >> Path.resolve
            >> pars.loadModuleEnv
            >> onOk fn libraryMeta:
            #TODO check that library actually exposes the module

            {
            , meta = libraryMeta
            , path = Path.resolve [ path, name .. "sp" ]
            }
            >> 'ok

        Meta.'installedLibrary path:
            todo "'installedLibrary not yet implemented =("


getEntryUsr as fn Imports, Text: IO.Re USR =
    fn projectMeta, entryModulePath:
    todo "getEntryUsr"
#    isEntryUmr =
#        fn 'UMR source name:
#        try source as
#
#            Meta.'sourceDirId id:
#                try Dict.get id projectMeta.sourceDirIdToPath as
#
#                    'just path:
#                        # TODO this should match the expression in getModuleMetaAndPath
#                        Path.resolve [ path, name .. ".sp" ] == entryModulePath
#
#                    'nothing:
#                        'false
#
#            _:
#                'false
#
#    allUmrs =
#        Dict.values projectMeta.moduleVisibleAsToUmr
#
#    try List.find isEntryUmr allUmrs as
#        'just umr: 'ok umr
#        'nothing: "Error: I can't find the module " .. entryModulePath .. " anywhere." >> 'err


loadModule as fn @IO, Imports, UMR, Text: IO.Re CA.Module =
    fn @io, imports, umr, fileName:
    if umr == CoreDefs.umr then
        'ok CoreDefs.coreModule
    else
        IO.readFile @io fileName
        >> onOk fn moduleAsText:
            params as Compiler/MakeCanonical.ReadOnly =
                {
                , errorModule = { content = moduleAsText, fsPath = fileName }
                , resolvePars = { currentImports = imports, currentModule = umr, loadExports = todo "loadExports" }
                , umr
                }

            Compiler/MakeCanonical.textToCanonicalModule 'false params >> resToIo


#
# IO
#
formattedToConsoleColoredText as fn Error.FormattedText: Text =
    fn formattedText:
    try formattedText as
        Error.'formattedText_Default t: t
        Error.'formattedText_Emphasys t: Term.yellow t
        Error.'formattedText_Warning t: Term.red t
        Error.'formattedText_Decoration t: Term.blue t


errorToText as fn Error: Text =
    __
    >> Error.toFormattedText
    >> List.map formattedToConsoleColoredText __
    >> Text.join "" __


resToIo as fn Res a: IO.Re a =
    fn res:
    try res as

        'ok a:
            'ok a

        'err e:
            e
            >> errorToText
            >> 'err


#
# Module loading
#

asModule as fn Bool & Text: Maybe Text =
    fn tuple:
    isDirectory & name =
        tuple

    if isDirectory or (Text.startsWithRegex "[A-Z][a-zA-Z0-9_]*[.]sp$") name /= name then
        'nothing
    else
        name
        >> Text.replace ".sp" "" __
        >> 'just


asModuleDirectory as fn Bool & Text: Maybe Text =
    fn tuple:
    isDirectory & name =
        tuple

    if isDirectory and (Text.startsWithRegex "^[A-Z][a-zA-Z0-9_]*$") name == name then
        'just name
    else
        'nothing


listSourceDir as fn @IO, Text, Text: IO.Re [ Text ] =
    fn @io, sourceDirRoot, modulePathWithTrailingSlash:
    path =
        sourceDirRoot .. "/" .. modulePathWithTrailingSlash

    IO.readDir @io path
    >> onOk fn dirContents:
    directChildren =
        dirContents
        >> List.filterMap asModule __
        >> List.map (fn fileName: modulePathWithTrailingSlash .. fileName) __

    dirContents
    >> List.filterMap asModuleDirectory __
    >> List.mapRes (fn subDir: listSourceDir @io sourceDirRoot (modulePathWithTrailingSlash .. subDir .. "/")) __
    >> onOk fn descendants:
    [ directChildren, List.concat descendants ]
    >> List.concat
    >> 'ok


ModuleAndPath =
    {
    , filePath as Text
    , moduleName as Text
    }


updateSourceDir as fn [ Text ], ImportsFile.SourceDir: ImportsFile.SourceDir =
    fn fileNames, orig:
    insertModuleName as fn Text, ImportsFile.SourceDir: ImportsFile.SourceDir =
        fn name, sd:
        try List.find (fn m: m.path == name) sd.modules as
            'just _: sd
            'nothing: { sd with modules = { globalTypes = [], globalValues = [], path = name, visibleAs = name } :: .modules }

    List.for orig fileNames insertModuleName


scanSourceDirs as fn @IO, ImportsFile: IO.Re Imports =
    fn @io, importsFile:

    # sourceDirs does not contain all modules available in the dir, but only the exceptions;
    # before building Imports we need to add those that are not mentioned.
    importsFile.sourceDirs
    >> List.mapRes (fn sd: listSourceDir @io sd.path "") __
    >> onOk fn allSourceDirLists:
    updatedSourceDirs as [ ImportsFile.SourceDir ] =
        List.map2 updateSourceDir allSourceDirLists importsFile.sourceDirs

    { importsFile with sourceDirs = updatedSourceDirs }
    >> ImportsFile.toImports
    >> resToIo


loadImports as fn @IO, Text: (IO.Re Imports) =
    fn @io, rootPath:

    filePath =
        Path.resolve [ rootPath, importsFileName ]

    IO.readFile @io filePath
    >> onOk fn fileContent:

    ImportsFile.textToModulesFile filePath fileContent
    >> resToIo
    >> onOk fn importsFile:

    scanSourceDirs @io importsFile


searchAncestorDirectories as fn @IO, fn Bool & Text: Bool, Text: Maybe Text =
    fn @io, isWantedFile, searchDir:
    try IO.readDir @io searchDir as

        'err _:
            'nothing

        'ok dirContents:
            if List.any isWantedFile dirContents then
                searchDir >> 'just
            else
                parent =
                    Path.resolve [ searchDir, ".." ]

                if parent == searchDir then
                    'nothing
                else
                    searchAncestorDirectories @io isWantedFile parent


#
# Compile
#

CompileMainPars =
    {
    , entryPoint as Text
    , maybeOutputPath as Maybe Text
    , platform as Platform.Platform
    , selfPath as Text
    }


compileMain as fn @IO, CompileMainPars: IO.Re None =
    fn @io, pars:
    #
    # Figure out project root
    #
    projectRoot =
        # Either use the first ancestor that contains an imports file...
        searchAncestorDirectories @io (fn isDirectory & fileName: not isDirectory and fileName == importsFileName) "."
        # ...either use the current dir
        >> Maybe.withDefault "." __

    IO.writeStdout @io __ << "Project root is " .. Path.resolve [ projectRoot ]

    #
    # Load meta and figure out entry point's USR
    #
    try loadImports @io projectRoot as
        'err msg:
            # TODO This is not portable, need a better way to get IO errors
            if Text.contains "ENOENT" msg then
                IO.writeStdout @io __ << "No " .. importsFileName .. " found, using default."
                scanSourceDirs @io pars.platform.defaultImportsFile
            else
                'err msg

        'ok i:
            'ok i

    >> onOk fn projectImports:
    getEntryUsr projectImports pars.entryPoint
    >> onOk fn entryUsr:

    #
    # Figure out corelib's root
    #
    [ pars.selfPath ]
    >> Path.resolve
    >> Path.dirname
    >> searchAncestorDirectories @io (fn isDirectory & fileName: isDirectory and fileName == libDirectoryName) __
    >> try __ as
        'nothing: "Error: I need a " .. libDirectoryName .. " directory next to the squarepants executable " .. pars.selfPath .. " but I can't find it." >> 'err
        'just p: Path.resolve [ p, libDirectoryName ] >> 'ok
    >> onOk fn coreLibraryPath:

    #
    # Compile!
    #
    getModuleMetaAndPathPars as GetModuleMetaAndPathPars =
        {
        , coreLibraryPath
        , projectImports
#        , platformSource = todo "platformSource"
        , loadImports = todo "loadImports"
        }

    loadCaModule as fn UMR: Res CA.Module =
        fn umr:
        getModuleMetaAndPath getModuleMetaAndPathPars umr
        >> onOk fn out:
        loadModule @io out.meta umr out.path >> Result.mapError (fn err: Error.'raw [ err ]) __

    {
    , loadCaModule
    , requiredUsrs = [ entryUsr ]
    }
    >> Compiler/LazyBuild.build
    >> resToIo
    >> onOk fn { constructors, rootValues }:
    outputFile =
        Maybe.withDefault pars.platform.defaultOutputName pars.maybeOutputPath

    # Should be the last
    type =
        try List.find (fn rv: rv.usr == entryUsr) rootValues as
            'just rv: rv.type
            'nothing: todo "no type!?"

    {
    , constructors
    , defs = rootValues
    , entryUsr
    , type
    }
    >> pars.platform.makeExecutable
    >> IO.writeFile @io outputFile __
    >> onOk fn _:
    IO.writeStdout @io __ << "---> " .. outputFile .. " written. =)"
