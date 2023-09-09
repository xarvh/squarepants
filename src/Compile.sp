modulesFileName as Text =
    "modules.sp"


libDirectoryName as Text =
    "lib"


coreDirName as Text =
    "core"


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


resToIo as fn Res a: IO a =
    fn res:
    try res as

        'ok a:
            IO.succeed a

        'err e:
            e
            >> Error.toFormattedText
            >> List.map formattedToConsoleColoredText __
            >> Text.join "" __
            >> IO.fail


onResSuccess as fn fn a: IO b: fn Res a: IO b =
    fn f:
    fn res:
    res
    >> resToIo
    >> (IO.onSuccess f) __


#
# Load modules.sp
#
loadModulesFile as fn Types/Platform.Platform, Text: IO ModulesFile.ModulesFile =
    fn platform, projectRoot:
    path =
        [ projectRoot, modulesFileName ] >> Path.resolve

    log "Metafile: " path

    path
    >> IO.readFile
    >> IO.onResult fn result:
    modulesAsText =
        try result as

            'ok f:
                f

            'err _:
                log "Using default modules.sp" ""

                platform.defaultModules

    resToIo << ModulesFile.textToModulesFile modulesFileName modulesAsText


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


listSourceDir as fn Text, Text: IO [ Text ] =
    fn sourceDirRoot, modulePathWithTrailingSlash:
    path =
        sourceDirRoot .. "/" .. modulePathWithTrailingSlash

    IO.readDir path
    >> IO.onSuccess fn dirContents:
    directChildren =
        dirContents
        >> List.filterMap asModule __
        >> List.map (fn fileName: modulePathWithTrailingSlash .. fileName) __

    getDescendants as IO [ [ Text ] ] =
        dirContents
        >> List.filterMap asModuleDirectory __
        >> List.map (fn subDir: listSourceDir sourceDirRoot (modulePathWithTrailingSlash .. subDir .. "/")) __
        >> IO.parallel

    getDescendants
    >> IO.onSuccess fn descendants:
    [ directChildren, List.concat descendants ]
    >> List.concat
    >> IO.succeed


# TODO move this to Meta?
umrToFileName as fn Meta, Text, UMR: Text =
    fn meta, corePath, umr:
    'UMR source name =
        umr

    try source as

        Meta.'core:
            Path.resolve (corePath :: "core" :: (Text.split "/" __ << name .. ".sp"))

        Meta.'posix:
            Path.resolve (corePath :: "posix" :: (Text.split "/" __ << name .. ".sp"))

        Meta.'browser:
            Path.resolve (corePath :: "browser" :: (Text.split "/" __ << name .. ".sp"))

        Meta.'sourceDirId id:
            try Dict.get id meta.sourceDirIdToPath as
                'nothing: todo << "invalid sourceDirId " .. id
                'just path: Path.resolve [ path, name .. ".sp" ]


loadModule as fn Meta, UMR, Text: IO CA.Module =
    fn meta, umr, fileName:
    IO.readFile fileName
    >> IO.onSuccess fn moduleAsText:
    params as Compiler/MakeCanonical.ReadOnly =
        {
        , errorModule = { content = moduleAsText, fsPath = fileName }
        , meta
        , umr
        }

    resToIo << Compiler/MakeCanonical.textToCanonicalModule 'false params


ModuleAndPath =
    {
    , filePath as Text
    , moduleName as Text
    }


#sdItemToUMR as fn Meta.Source, Text: UMR =
#    fn source, fileName:
#    fileName
#    >> Text.replace ".sp" "" __
#    >> UMR source __

updateSourceDir as fn [ Text ], ModulesFile.SourceDir: ModulesFile.SourceDir =
    fn fileNames, orig:
    insertModuleName as fn Text, ModulesFile.SourceDir: ModulesFile.SourceDir =
        fn name, sd:
        try List.find (fn m: m.path == name) sd.modules as
            'just _: sd
            'nothing: { sd with modules = { globalTypes = [], globalValues = [], path = name, visibleAs = name } :: .modules }

    List.for orig fileNames insertModuleName


loadMeta as fn IO.Env, Types/Platform.Platform, Text, Text: IO Meta =
    fn env, platform, entryModuleDir, projectRoot:
    loadModulesFile platform projectRoot
    >> IO.onSuccess fn modulesFileRaw:
    resolvedDirs =
        modulesFileRaw.sourceDirs >> List.map (fn sd: { sd with path = Path.resolve [ projectRoot, .path ] }) __

    allDirs =
        if List.any (fn sd: sd.path == entryModuleDir) resolvedDirs then
            resolvedDirs
        else
            { modules = [], path = entryModuleDir } :: resolvedDirs

    modulesFile =
        { modulesFileRaw with sourceDirs = allDirs }

    # sourceDirs does not contain all modules available in the dir, but only the exceptions;
    # before building Meta we need to add those that are not mentioned.
    getAllSourceDirLists as IO [ [ Text ] ] =
        modulesFile.sourceDirs
        >> List.map (fn sd: listSourceDir sd.path "") __
        >> IO.parallel

    getAllSourceDirLists
    >> IO.onSuccess fn allSourceDirLists:
    updatedSourceDirs as [ ModulesFile.SourceDir ] =
        List.map2 updateSourceDir allSourceDirLists modulesFile.sourceDirs

    { modulesFile with sourceDirs = updatedSourceDirs }
    >> ModulesFile.toMeta
    >> IO.succeed


#
# Compile
#
searchAncestorDirectories as fn fn Bool & Text: Bool, Text: IO (Maybe Text) =
    fn isWantedFile, searchDir:
    IO.readDir searchDir
    >> IO.onResult fn result:
    try result as

        'err _:
            IO.succeed 'nothing

        'ok dirContents:
            if List.any isWantedFile dirContents then
                searchDir
                >> 'just
                >> IO.succeed
            else
                parent =
                    Path.resolve [ searchDir, ".." ]

                if parent == searchDir then
                    IO.succeed 'nothing
                else
                    parent >> searchAncestorDirectories isWantedFile __


CompileMainPars =
    {
    , entryModulePath as Text
    , env as IO.Env
    , maybeOutputPath as Maybe Text
    , platform as Types/Platform.Platform
    , selfPath as Text
    }


compileMain as fn CompileMainPars: IO Int =
    fn pars:
    #
    # Figure out project root
    #
    entryModulePath =
        Path.resolve [ pars.entryModulePath ]

    entryModuleDir =
        Path.dirname entryModulePath

    entryModuleDir
    >> searchAncestorDirectories (fn isDirectory & fileName: not isDirectory and fileName == modulesFileName) __
    >> IO.onSuccess fn maybeProjectRoot:
    projectRoot =
        Maybe.withDefault entryModuleDir maybeProjectRoot

    #
    # Load meta and figure out entry point's USR
    #
    loadMeta pars.env pars.platform entryModuleDir projectRoot
    >> IO.onSuccess fn meta:
    # This will be replaced once we get lazy loading
    maybeEntryUmr =
        meta.moduleVisibleAsToUmr
        >> Dict.values
        # TODO split umrToFileName so we can call it without `""`
        >> List.find (fn umr: umrToFileName meta "" umr == entryModulePath) __

    entryUmr =
        try maybeEntryUmr as
            'nothing: todo << "Error: you are asking me to compile module " .. entryModulePath .. " but I can't find it anywhere."
            'just umr: umr

    #
    # Figure out corelib's root
    #
    [ pars.selfPath ]
    >> Path.resolve
    >> Path.dirname
    >> searchAncestorDirectories (fn isDirectory & fileName: isDirectory and fileName == libDirectoryName) __
    >> IO.onSuccess fn maybeCorelibParent:
    corePath =
        try maybeCorelibParent as
            'nothing: todo << "Error: I expect to find the " .. libDirectoryName .. " directory next to the spcc executable " .. pars.selfPath .. " but I can't find it."
            'just p: Path.resolve [ p, libDirectoryName ]

    outputFile =
        Maybe.withDefault pars.platform.defaultOutputPath pars.maybeOutputPath

    loadFile as fn UMR: IO (UMR & Text) =
        fn umr:
        IO.readFile (umrToFileName meta corePath umr)
        >> IO.onSuccess fn content:
        IO.succeed (umr & content)

    meta.moduleVisibleAsToUmr
    >> Dict.values
    >> List.map loadFile __
    >> IO.parallel
    >> IO.onSuccess fn modulesAsText:
    {
    , entryModule = entryUmr
    , exposedValues = []
    , meta
    , modules = modulesAsText
    , umrToFsPath = umrToFileName meta corePath __
    }
    >> Compiler/Compiler.compileModules
    >> onResSuccess fn compileModulesOut:
    pars.platform.makeExecutable compileModulesOut
    >> IO.writeFile outputFile __
    >> IO.onSuccess fn _:
    IO.writeStdout __ << "---> " .. outputFile .. " written. =)"
