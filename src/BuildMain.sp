#TODO rename to MainMake

importsFileName as Text =
    "imports.sp"


corelibDir as Text =
    "corelib"


installedDir as Text =
    "installedLibraries"



ioToRes as fn IO.Re a: Res a =
   Result.mapError (fn err: Error.'raw [ err ]) __


getEntryUsr as fn Imports, Text: Res USR =
    fn projectImports, entryModule:
    Meta.resolve
        {
        , currentImports = projectImports
        # TODO: currentModule should be necessary only if we're not specifying the module in the second argument
        , currentModule = CoreDefs.umr
        , loadExports = fn importsPath: 'err [ "Entry point can't be in an installed library!" ]
        }
        ('just entryModule)
        "main"
    >> Result.mapError Error.'raw __


LoadCaModulePars =
    {
    , loadImports as fn Meta.ImportsPath, Text: Res Imports
    , readFile as fn Text: IO.Re Text
    , rootPaths as Meta.RootPaths
    }


loadCaModule as fn LoadCaModulePars, UMR: Res CA.Module =
    fn pars, umr:
    if umr == CoreDefs.umr then
        'ok CoreDefs.coreModule
    else
        'UMR importsPath sourceDir modulePath =
            umr

        Meta.'importsPath rootDirectory importsDir =
            importsPath

        rootPath =
            Meta.rootDirectoryToPath pars.rootPaths rootDirectory

        [
        , rootPath
        , importsDir
        ]
        >> Path.join
        >> pars.loadImports importsPath __
        >> onOk fn imports:
        fileName =
            [
            , rootPath
            , importsDir
            , sourceDir
            , modulePath
            ]
            >> Path.join
            >> __ .. ".sp"

        fileName
        >> pars.readFile
        >> ioToRes
        >> onOk fn moduleAsText:
        params as Compiler/MakeCanonical.ReadOnly =
            {
            , errorModule = { content = moduleAsText, fsPath = fileName }
            , resolvePars = { currentImports = imports, currentModule = umr, loadExports = todo "loadExports" }
            , umr
            }

        Compiler/MakeCanonical.textToCanonicalModule 'false params


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
            'nothing: { sd with modules = { globals = [], path = name, visibleAs = name } :: .modules }

    List.for orig fileNames insertModuleName


scanSourceDirs as fn @IO, Meta.ImportsPath, ImportsFile: Res Imports =
    fn @io, importsPath, importsFile:
    # sourceDirs does not contain all modules available in the dir, but only the exceptions;
    # before building Imports we need to add those that are not mentioned.
    importsFile.sourceDirs
    >> List.mapRes (fn sd: listSourceDir @io sd.path "") __
    >> ioToRes
    >> onOk fn allSourceDirLists:
    updatedSourceDirs as [ ImportsFile.SourceDir ] =
        List.map2 updateSourceDir allSourceDirLists importsFile.sourceDirs

    ImportsFile.toImports
        {
        , importsPath
        , joinPath = Path.join
        }
        { importsFile with sourceDirs = updatedSourceDirs }


loadImports as fn @IO, Meta.ImportsPath, Text: Res Imports =
    fn @io, metaImportsPath, fsImportsPath:
    filePath =
        Path.resolve [ fsImportsPath, importsFileName ]

    IO.readFile @io filePath
    >> ioToRes
    >> onOk fn fileContent:
    ImportsFile.textToModulesFile filePath fileContent
    >> onOk fn importsFile:
    scanSourceDirs @io metaImportsPath importsFile


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


compileMain as fn @IO, CompileMainPars: Res None =
    fn @io, pars:
    #
    # Figure out project root
    #
    projectRoot =
        # Either use the first ancestor that contains an imports file...
        searchAncestorDirectories @io (fn isDirectory & fileName: not isDirectory and fileName == importsFileName) "."
        # ...either use the current dir
        >> Maybe.withDefault "." __

    IO.writeStdout @io __ << "Project root is " .. Path.resolve [ projectRoot ] .. "\n"

    importsPath as Meta.ImportsPath =
        Meta.'importsPath Meta.'user ""

    #
    # Load meta and figure out entry point's USR
    #
#    try loadImports @io importsPath projectRoot as
#
#        'err msg:
#            # TODO This is not portable, need a better way to get IO errors
#            if Text.contains "ENOENT" msg then
#                IO.writeStdout @io __ << "No " .. importsFileName .. " found, using default.\n"
#
#                scanSourceDirs @io importsPath pars.platform.defaultImportsFile
#            else
#                'err msg
#
#        'ok i:
#            'ok i
    loadImports @io importsPath projectRoot
    >> onOk fn projectImports:
    getEntryUsr projectImports pars.entryPoint
    >> onOk fn entryUsr:
    #
    # Figure out corelib's root
    #
    executablePath =
        [ pars.selfPath ]
        >> Path.resolve
        >> Path.dirname

    #
    # Compile!
    #
    rootPaths as Meta.RootPaths =
        {
        , core = Path.join [ executablePath, corelibDir ]
        , installed = Path.join [ projectRoot, installedDir ]
        , project = projectRoot
        }

    loadCaModulePars as LoadCaModulePars =
        {
        , loadImports = loadImports @io __ __
        , readFile = IO.readFile @io __
        , rootPaths
        }

    {
    , loadCaModule = loadCaModule loadCaModulePars __
    , requiredUsrs = [ entryUsr ]
    }
    >> Compiler/LazyBuild.build
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
    >> ioToRes
    >> onOk fn _:

    IO.writeStdout @io ("---> " .. outputFile .. " written. =)")
    >> ioToRes
