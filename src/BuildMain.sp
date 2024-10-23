#TODO rename to MainMake

importsFileName as Text =
    "imports.sp"


exportsFileName as Text =
    "exports.sp"


defaultCorelibDir as Text =
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
        , currentModule =
            CoreDefs.umr
        , loadExports = fn importsPath: 'err (Error.'raw [ "Entry point can't be in an installed library!" ])
        , makeError = Error.'raw
        }
        ('just entryModule)
        "main"


buildInfoModule as fn Platform: CA.Module =
    fn platform:
#    compile as CA.ValueDef =
#        {
#        , directDeps = Dict.ofOne platform.compileUsr Meta.'valueDependency
#        , maybeBody = 'just << CA.'variable Pos.'n ('refGlobal platform.compileUsr)
#        , name = "compile"
#        , maybeAnnotation = 'nothing
#        , namePos = Pos.'n
#        }

    {
    , aliasDefs = Dict.empty
    , asText = "<buildInfo module>"
    , constructorDefs = Dict.empty
    , fsPath = "<buildInfo module>"
    , umr = CoreDefs.makeUmr "BuildInfo"
    , valueDefs = Dict.empty
    #Dict.ofOne "compile" compile
    , variantTypeDefs =
        Dict.empty
    }


LoadCaModulePars =
    {
#    , buildInfoModule as CA.Module
    , idToDirs as fn Int: { importsDir as Text, sourceDir as Text }
    , loadExports as fn Meta.ImportsPath: Res Exports
    , loadImports as fn Meta.ImportsPath: Res Imports
    , readFile as fn Text: IO.Re Text
    , rootPaths as Meta.RootPaths
    }


loadCaModule as fn LoadCaModulePars, USR: Res CA.Module =
    fn pars, 'USR umr name:
    if umr == CoreDefs.umr then
        'ok CoreDefs.coreModule
#    else if umr == pars.buildInfoModule.umr then
#        'ok pars.buildInfoModule
    else
        'UMR rootDirectory id modulePath =
            umr

        { importsDir, sourceDir } =
            pars.idToDirs id

        Meta.'importsPath rootDirectory importsDir
        >> pars.loadImports
        >> onOk fn imports:
        rootPath =
            Meta.rootDirectoryToPath pars.rootPaths rootDirectory

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
        errorModule =
            { content = moduleAsText, fsPath = fileName }

        resolvePars =
            fn pos:
            {
            , currentImports = imports
            , currentModule = umr
            , loadExports = pars.loadExports
            , makeError = Error.'simple errorModule pos __
            }

        !globals as Hash USR Name =
            Hash.fromList []

        !aliases as Hash UMR Name =
            Hash.fromList []

        # Here we're hijacking the resolution to build the reverse imports hashes
        # TODO we're re-adding the same entries every time we resolve them: there's probably a cleaner and more efficient way to do this
        resolveToUsr =
            fn pos, maybeModuleName, name_:
            try Meta.resolve (resolvePars pos) maybeModuleName name_ as

                'err e:
                    'err e

                'ok usr:
                    'USR umr_ _ =
                        usr

                    __xxx__ =
                        try maybeModuleName as
                            'just moduleName: Hash.insert @aliases umr_ moduleName
                            'nothing: Hash.insert @globals usr name_

                    'ok usr

        params as Compiler/MakeCanonical.ReadOnly =
            {
            , errorModule
            , imports
            , resolveToUsr
            , umr
            }

        Compiler/MakeCanonical.textToCanonicalModule 'false params
        >> onOk fn mod:
        { mod with
        , umrToAlias = Hash.toList @aliases >> Dict.fromList
        , usrToGlobal = Hash.toList @globals >> Dict.fromList
        }
        >> 'ok


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
        >> List.filterMap __ asModule
        >> List.map __ (fn fileName: modulePathWithTrailingSlash .. fileName)

    dirContents
    >> List.filterMap __ asModuleDirectory
    >> List.mapRes __ (fn subDir: listSourceDir @io sourceDirRoot (modulePathWithTrailingSlash .. subDir .. "/"))
    >> onOk fn descendants:
    x =
        [ directChildren, List.concat descendants ] >> List.concat

    x >> 'ok


ModuleAndPath =
    {
    , filePath as Text
    , moduleName as Text
    }


updateSourceDir as fn [ Text ], ImportsFile.SourceDir: ImportsFile.SourceDir =
    fn fileNames, orig:
    insertModuleName as fn Text, ImportsFile.SourceDir: ImportsFile.SourceDir =
        fn name, sd:
        try List.find sd.modules (fn m: m.path == name) as
            'just _: sd
            'nothing: { sd with modules = { globals = [], path = name, visibleAs = name } :: .modules }

    List.for orig fileNames insertModuleName


scanSourceDirs as fn @IO, fn Text, Text: Int, Meta.RootPaths, Meta.ImportsPath, ImportsFile: Res Imports =
    fn @io, getSourceDirId, rootPaths, importsPath, importsFile:
    Meta.'importsPath root importsDir =
        importsPath

    rootPath =
        Meta.rootDirectoryToPath rootPaths root

    # sourceDirs does not contain all modules available in the dir, but only the exceptions;
    # before building Imports we need to add those that are not mentioned.
    importsFile.sourceDirs
    >> List.mapRes __ (fn sd: listSourceDir @io (Path.join [ rootPath, importsDir, sd.path ]) "")
    >> ioToRes
    >> onOk fn allSourceDirLists:
    updatedSourceDirs as [ ImportsFile.SourceDir ] =
        List.map2 allSourceDirLists importsFile.sourceDirs updateSourceDir

    ImportsFile.toImports
        {
        , getSourceDirId
        , importsPath
        , joinPath = Path.join
        }
        { importsFile with sourceDirs = updatedSourceDirs }


#
#
#
State =
    {
    , idToSourcePath as Hash Int { importsDir as Text, sourceDir as Text }
    , loadedExports as Hash Meta.ImportsPath Exports
    , loadedImports as Hash Meta.ImportsPath Imports
    , nextId as Int
    , sourcePathToId as Hash { importsDir as Text, sourceDir as Text } Int
    }


idToDirs as fn @State, Int: { importsDir as Text, sourceDir as Text } =
    fn @state, id:
    try Hash.get @state.idToSourcePath id as
        'nothing: todo "compiler error: no idToSourcePath"
        'just sourcePath: sourcePath


loadExports as fn @IO, @State, Meta.RootPaths, Meta.ImportsPath: Res Exports =
    fn @io, @state, rootPaths, importsPath:
    try Hash.get @state.loadedExports importsPath as

        'just exports:
            'ok exports

        'nothing:
            loadImports @io @state rootPaths importsPath
            >> onOk fn imports:
            Meta.'importsPath rootDirectory importsDir =
                importsPath

            filePath =
                Path.resolve [ Meta.rootDirectoryToPath rootPaths rootDirectory, importsDir, exportsFileName ]

            IO.readFile @io filePath
            >> ioToRes
            >> onOk fn fileContent:
            ExportsFile.fromText filePath fileContent
            >> onOk fn exportsFile:
            ExportsFile.toExports imports exportsFile
            >> onOk fn exports:
            Hash.insert @state.loadedExports importsPath exports

            'ok exports


loadImports as fn @IO, @State, Meta.RootPaths, Meta.ImportsPath: Res Imports =
    fn @io, @state, rootPaths, importsPath:
    getSourceDirId as fn Text, Text: Int =
        fn importsDir, sourceDir:
        try Hash.get @state.sourcePathToId { importsDir, sourceDir } as

            'nothing:
                id =
                    cloneUni @state.nextId

                @state.nextId += 1

                Hash.insert @state.sourcePathToId { importsDir, sourceDir } id

                Hash.insert @state.idToSourcePath id { importsDir, sourceDir }

                id

            'just id:
                id

    try Hash.get @state.loadedImports importsPath as

        'just imports:
            'ok imports

        'nothing:
            Meta.'importsPath rootDirectory importsDir =
                importsPath

            filePath =
                Path.resolve [ Meta.rootDirectoryToPath rootPaths rootDirectory, importsDir, importsFileName ]

            IO.readFile @io filePath
            >> ioToRes
            >> onOk fn fileContent:
            ImportsFile.fromText filePath fileContent
            >> onOk fn importsFile:
            scanSourceDirs @io getSourceDirId rootPaths importsPath importsFile
            >> onOk fn imports:
            Hash.insert @state.loadedImports importsPath imports

            'ok imports


searchAncestorDirectories as fn @IO, fn Bool & Text: Bool, Text: Maybe Text =
    fn @io, isWantedFile, searchDir:
    try IO.readDir @io searchDir as

        'err _:
            'nothing

        'ok dirContents:
            if List.any dirContents isWantedFile then
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
    , corelib as Maybe Text
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
    # Figure out corelib's root
    #
    corelibPath =
        try pars.corelib as

            'just corelib:
                corelib

            'nothing:
                executablePath =
                    [ pars.selfPath ]
                    >> Path.resolve
                    >> Path.dirname

                Path.join [ executablePath, defaultCorelibDir ]

    rootPaths as Meta.RootPaths =
        {
        , core = corelibPath
        , installed = Path.join [ projectRoot, installedDir ]
        , project = projectRoot
        }

    #
    # Load meta and figure out entry point's USR
    #

    # TODO make the compiler work even when executed from a sub-directory of the project root
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

    !state as State =
        {
        , idToSourcePath = Hash.fromList [ CoreDefs.pathId & { importsDir = CoreDefs.importsDir, sourceDir = CoreDefs.sourceDir } ]
        , loadedExports = Hash.fromList []
        , loadedImports = Hash.fromList []
        , nextId = CoreDefs.pathId + 1
        , sourcePathToId = Hash.fromList [ { importsDir = CoreDefs.importsDir, sourceDir = CoreDefs.sourceDir } & CoreDefs.pathId ]
        }

    loadImports @io @state rootPaths importsPath
    >> onOk fn projectImports:
    getEntryUsr projectImports pars.entryPoint
    >> onOk fn entryUsr:
    #
    # Figure out the platform library UMR
    #
    Dict.get pars.platform.name projectImports.platforms
    >> Maybe.toResult (Error.'raw [ "project imports.sp does not specify a '" .. pars.platform.name .. "' platform." ]) __
    >> onOk fn platformModuleLocations:
    # TODO properly collect or return errors instead of crashing
    makePlatformUmr as fn Name: UMR =
        fn modulePath:
        try Dict.get modulePath platformModuleLocations as

            'nothing:
                todo << "no " .. modulePath .. "in loaded platform."

            'just location:
                try location as

                    Meta.'locationSourceDir umr:
                        umr

                    Meta.'locationLibrary libraryImportsPath modulePath2:
                        try loadImports @io @state rootPaths libraryImportsPath as

                            'err err:
                                todo (toHuman err)

                            'ok libraryImports:
                                try Dict.get modulePath libraryImports.modulePathToLocation as
                                    'nothing: todo << "Platform bug: no module " .. modulePath .. " the library imports for platform " .. pars.platform.name
                                    'just (Meta.'locationLibrary _ _): todo << "Platform bug: platform wants the UMR of a library module: " .. modulePath
                                    'just (Meta.'locationSourceDir umr): umr

    #
    # Compile!
    #
    loadCaModulePars as LoadCaModulePars =
        {
#        , buildInfoModule = buildInfoModule pars.platform
        , idToDirs = idToDirs @state __
        , loadExports = loadExports @io @state rootPaths __
        , loadImports = loadImports @io @state rootPaths __
        , readFile = IO.readFile @io __
        , rootPaths
        }

    {
    , loadCaModule = loadCaModule loadCaModulePars __
    , projectImports
    , requiredUsrs = [ entryUsr, pars.platform.extraRequiredUsrs makePlatformUmr... ]
    }
    >> Compiler/LazyBuild.build
    >> onOk fn { constructors, natives, rootValues }:
    # TODO ensure all natives are implemented?

    outputFile =
        Maybe.withDefault pars.platform.defaultOutputName pars.maybeOutputPath

    # Should be the last
    _entryUsr as EA.TranslatedUsr =
        EA.translateUsr entryUsr

    type =
        try List.find rootValues (fn rv: rv.usr == _entryUsr) as
            'just rv: rv.type
            'nothing: todo "no type!?"

    {
    , constructors
    , defs = rootValues
    , entryUsr = _entryUsr
    , type
    }
    >> pars.platform.makeExecutable makePlatformUmr
    >> IO.writeFile @io outputFile __
    >> ioToRes
    >> onOk fn _:
    IO.writeStdout @io ("---> " .. outputFile .. " written. =)\n") >> ioToRes
