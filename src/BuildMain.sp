#TODO rename to BuildMain

modulesFileName as Text =
    "modules.sp"


libDirectoryName as Text =
    "lib"


coreDirName as Text =
    "core"


GetModuleMetaAndPathPars =
    {
    , coreLibraryMeta as Meta
    , coreLibraryPath as Text
    , projectMeta as Meta
    }


getModuleMetaAndPath as fn @IO, GetModuleMetaAndPathPars, UMR: Res { meta as Meta, path as Text } =
    fn @io, pars, umr:
    'UMR source name =
        umr

    try source as

        Meta.'core:
            {
            , meta = pars.coreLibraryMeta
            , path = Path.resolve [ pars.coreLibraryPath, coreDirName, Text.split "/" (name .. ".sp")... ]
            }
            >> 'ok

        Meta.'sourceDirId id:
            try Dict.get id pars.projectMeta.sourceDirIdToPath as
                'nothing:
                      [ "invalid sourceDirId " .. id .. " (this is a compiler bug!)" ]
                      >> Error.'raw
                      >> 'err

                'just path:
                      [ path, name .. ".sp" ]
                      >> Path.resolve
                      >> 'ok

            >> onOk fn path:
            {
            , meta = pars.projectMeta
            , path
            }
            >> 'ok

        [#
        Meta.'library libSource id:
            env.loadMetafile libSource
            >> onOk fn libraryMeta:
            #TODO check that library exposes module

            {
            , meta = libraryMeta
            , path =
                try Dict.get id libraryMeta.sourceDirIdToPath as
                    'nothing: todo << "invalid sourceDirId " .. id .. " in library " .. libSource
                    'just path: Path.resolve [ path, name .. ".sp" ]
            }
            >> 'ok
        #]

        # TODO posix and browser should be just replaced by a "platform" source
        Meta.'posix:
            {
            , meta = pars.coreLibraryMeta
            , path = Path.resolve [ pars.coreLibraryPath, "posix", Text.split "/" (name .. ".sp")... ]
            }
            >> 'ok

        Meta.'browser:
            {
            , meta = pars.coreLibraryMeta
            , path = Path.resolve [ pars.coreLibraryPath, "browser", Text.split "/" (name .. ".sp")... ]
            }
            >> 'ok


getEntryUmr as fn Meta, Text: IO.Re UMR =
    fn projectMeta, entryModulePath:
    isEntryUmr =
        fn 'UMR source name:
        try source as

            Meta.'sourceDirId id:
                try Dict.get id projectMeta.sourceDirIdToPath as

                    'just path:
                        # TODO this should match the expression in getModuleMetaAndPath
                        Path.resolve [ path, name .. ".sp" ] == entryModulePath

                    'nothing:
                        'false

            _:
                'false

    allUmrs =
        Dict.values projectMeta.moduleVisibleAsToUmr

    try List.find isEntryUmr allUmrs as
        'just umr: 'ok umr
        'nothing: "Error: I can't find the module " .. entryModulePath .. " anywhere." >> 'err


loadModule as fn @IO, Meta, UMR, Text: IO.Re CA.Module =
    fn @io, meta, umr, fileName:
    try umr as

        'UMR Meta.'core "Core":
            'ok CoreDefs.coreModule

        _:
            IO.readFile @io fileName
            >> onOk fn moduleAsText:
                params as Compiler/MakeCanonical.ReadOnly =
                    {
                    , errorModule = { content = moduleAsText, fsPath = fileName }
                    , meta
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
# Load modules.sp
#
loadModulesFile as fn @IO, Platform.Platform, Text: IO.Re ModulesFile.ModulesFile =
    fn @io, platform, projectRoot:
    path =
        [ projectRoot, modulesFileName ] >> Path.resolve

    log "Metafile: " path

    try IO.readFile @io path as

        'ok f:
            f

        'err _:
            log "Using default modules.sp" ""

            platform.defaultModules
    >> ModulesFile.textToModulesFile modulesFileName __
    >> resToIo


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


updateSourceDir as fn [ Text ], ModulesFile.SourceDir: ModulesFile.SourceDir =
    fn fileNames, orig:
    insertModuleName as fn Text, ModulesFile.SourceDir: ModulesFile.SourceDir =
        fn name, sd:
        try List.find (fn m: m.path == name) sd.modules as
            'just _: sd
            'nothing: { sd with modules = { globalTypes = [], globalValues = [], path = name, visibleAs = name } :: .modules }

    List.for orig fileNames insertModuleName


loadMeta as fn @IO, Platform.Platform, Text, Text: IO.Re Meta =
    fn @io, platform, entryModuleDir, projectRoot:
    loadModulesFile @io platform projectRoot
    >> onOk fn modulesFileRaw:
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
    modulesFile.sourceDirs
    >> List.mapRes (fn sd: listSourceDir @io sd.path "") __
    >> onOk fn allSourceDirLists:
    updatedSourceDirs as [ ModulesFile.SourceDir ] =
        List.map2 updateSourceDir allSourceDirLists modulesFile.sourceDirs

    { modulesFile with sourceDirs = updatedSourceDirs }
    >> ModulesFile.toMeta
    >> 'ok


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
    , entryModulePath as Text
    , maybeOutputPath as Maybe Text
    , platform as Platform.Platform
    , selfPath as Text
    }


compileMain as fn @IO, CompileMainPars: IO.Re None =
    fn @io, pars:
    #
    # Figure out project root
    #
    entryModulePath =
        Path.resolve [ pars.entryModulePath ]

    entryModuleDir =
        Path.dirname entryModulePath

    projectRoot =
        entryModuleDir
        >> searchAncestorDirectories @io (fn isDirectory & fileName: not isDirectory and fileName == modulesFileName) __
        >> Maybe.withDefault entryModuleDir __

    #
    # Load meta and figure out entry point's USR
    #
    loadMeta @io pars.platform entryModuleDir projectRoot
    >> onOk fn projectMeta:
    getEntryUmr projectMeta entryModulePath
    >> onOk fn entryUmr:
    entryUsr =
        'USR entryUmr "main"

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
        , coreLibraryMeta = TH.meta
        , coreLibraryPath
        , projectMeta
        }

    loadCaModule as fn UMR: Res CA.Module =
        fn umr:
        getModuleMetaAndPath @io getModuleMetaAndPathPars umr
        >> onOk fn out:
        loadModule @io out.meta umr out.path
        >> Result.mapError (fn err: Error.'raw [ err ]) __

    {
    , requiredUsrs = [ entryUsr ]
    , loadCaModule
    }
    >> Compiler/LazyBuild.build
    >> Result.mapError errorToText __
    >> onOk fn { constructors, rootValues }:

    outputFile =
        Maybe.withDefault pars.platform.defaultOutputPath pars.maybeOutputPath

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
