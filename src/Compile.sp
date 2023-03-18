

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
        , Error.FormattedText_Default t: t
        , Error.FormattedText_Emphasys t: Term.yellow t
        , Error.FormattedText_Warning t: Term.red t
        , Error.FormattedText_Decoration t: Term.blue t


resToIo as fn Res a: IO a =
    fn res:
    try res as
        , Ok a: IO.succeed a
        , Err e:
            e
            >> Error.toFormattedText
            >> List.map formattedToConsoleColoredText __
            >> Text.join "" __
            >> IO.fail


onResSuccess as fn (fn a: IO b): fn Res a: IO b =
    fn f: fn res:
    res
    >> resToIo
    >> (IO.onSuccess f) __


#
# Load modules.sp
#
loadModulesFile as fn Types/Platform.Platform, Text: IO ModulesFile.ModulesFile =
    fn platform, projectRoot:

    path =
        [ projectRoot, modulesFileName ]
        >> Path.resolve

    log "Metafile: " path

    path
    >> IO.readFile
    >> IO.onResult fn result:

    modulesAsText =
        try result as
            , Ok f:
                f

            , Err _:
                log "Using default modules.sp" ""
                platform.defaultModules

    resToIo << ModulesFile.textToModulesFile modulesFileName modulesAsText



#
# Module loading
#


asModule as fn (Bool & Text): Maybe Text =
    fn tuple:
    isDirectory & name = tuple

    if isDirectory or (Text.startsWithRegex "[A-Z][a-zA-Z0-9_]*[.]sp$") name /= name then
        Nothing
    else
        name
            >> Text.replace ".sp" "" __
            >> Just


asModuleDirectory as fn (Bool & Text): Maybe Text =
    fn tuple:
    isDirectory & name = tuple

    if isDirectory and (Text.startsWithRegex "^[A-Z][a-zA-Z0-9_]*$") name == name then
        Just name
    else
        Nothing


listSourceDir as fn Text, Text: IO [Text] =
    fn sourceDirRoot, modulePathWithTrailingSlash:

    path =
        sourceDirRoot .. "/" .. modulePathWithTrailingSlash

    IO.readDir path
    >> IO.onSuccess fn dirContents:

    directChildren =
        dirContents
        >> List.filterMap asModule __
        >> List.map (fn fileName: modulePathWithTrailingSlash .. fileName) __

    getDescendants as IO [[Text]] =
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
umrToFileName as fn Text, UMR: Text =
    fn corePath, umr:

    UMR source name =
        umr

    try source as
        , Meta.SourceDir d:
            Path.resolve [ d, name .. ".sp" ]

        , Meta.Core:
            Path.resolve (corePath :: "core" :: (Text.split "/" __ << name .. ".sp"))

        , Meta.Posix:
            Path.resolve (corePath :: "posix" :: (Text.split "/" __ << name .. ".sp"))

        , Meta.Browser:
            Path.resolve (corePath :: "browser" :: (Text.split "/" __ << name .. ".sp"))


loadModule as fn Meta, UMR, Text: IO CA.Module =
    fn meta, umr, fileName:

    IO.readFile fileName
    >> IO.onSuccess fn moduleAsText:

    params as Compiler/MakeCanonical.ReadOnly =
        {
        , meta
        , umr
        , errorModule = { fsPath = fileName, content = moduleAsText }
        }

    resToIo << Compiler/MakeCanonical.textToCanonicalModule False params


alias ModuleAndPath = {
    , moduleName as Text
    , filePath as Text
    }


#sdItemToUMR as fn Meta.Source, Text: UMR =
#    fn source, fileName:
#    fileName
#    >> Text.replace ".sp" "" __
#    >> UMR source __


updateSourceDir as fn [Text], ModulesFile.SourceDir: ModulesFile.SourceDir =
    fn fileNames, orig:

    insertModuleName as fn Text, ModulesFile.SourceDir: ModulesFile.SourceDir =
        fn name, sd:
        try List.find (fn m: m.path == name) sd.modules as
            , Just _: sd
            , Nothing: { sd with modules = { path = name, visibleAs = name, globalValues = [], globalTypes = [] } :: .modules }

    List.for orig fileNames insertModuleName


loadMeta as fn IO.Env, Types/Platform.Platform, Text, Text: IO Meta =
    fn env, platform, entryModuleDir, projectRoot:

    loadModulesFile platform projectRoot
    >> IO.onSuccess fn modulesFileRaw:

    resolvedDirs =
        modulesFileRaw.sourceDirs
        >> List.map (fn sd: { sd with path = Path.resolve [ projectRoot, .path ] }) __

    allDirs =
        if List.any (fn sd: sd.path == entryModuleDir) resolvedDirs then
            resolvedDirs
        else
            { path = entryModuleDir, modules = [] } :: resolvedDirs

    modulesFile =
        { modulesFileRaw with sourceDirs = allDirs }

    # sourceDirs does not contain all modules available in the dir, but only the exceptions;
    # before building Meta we need to add those that are not mentioned.
    getAllSourceDirLists as IO [[Text]] =
        modulesFile.sourceDirs
        >> List.map (fn sd: listSourceDir sd.path "") __
        >> IO.parallel

    getAllSourceDirLists
    >> IO.onSuccess fn allSourceDirLists:

    updatedSourceDirs as [ModulesFile.SourceDir] =
        List.map2 updateSourceDir allSourceDirLists modulesFile.sourceDirs

    { modulesFile with sourceDirs = updatedSourceDirs }
    >> ModulesFile.toMeta
    >> IO.succeed


#
# Compile
#
searchAncestorDirectories as fn (fn Bool & Text: Bool), Text: IO (Maybe Text) =
    fn isWantedFile, searchDir:

    IO.readDir searchDir
    >> IO.onResult fn result:

    try result as
        , Err _:
            IO.succeed Nothing

        , Ok dirContents:
            if List.any isWantedFile dirContents then
                searchDir
                >> Just
                >> IO.succeed

            else
                parent = Path.resolve [ searchDir, ".." ]
                if parent == searchDir then
                    IO.succeed Nothing
                else
                    parent >> searchAncestorDirectories isWantedFile __

#mergeWithCore as fn CA.Module, CA.Module: CA.Module =
#    fn coreModule, userModule:
#
#    need to strip positions before merging >_<
#
#    { userModule with
#    , aliasDefs = Dict.join coreModule.aliasDefs .aliasDefs
#    , unionDefs = Dict.join coreModule.unionDefs .unionDefs
#    , valueDefs = Dict.join coreModule.valueDefs .valueDefs
#    }



getMainName as fn Dict Name TA.RawType: Res Text =
    fn targetsByName:

    # TODO: actually check that type is IO Int
    #    returnType as TA.Type =
    #        TA.TypeExact ()

    try Dict.toList targetsByName as
        , [ name & type, ..._ ]:
            Ok name

        , z:
            log "-->" z
            todo "main type should be IO Int"


alias CompileMainPars = {
    , env as IO.Env
    , selfPath as Text
    , entryModulePath as Text
    , maybeOutputPath as Maybe Text
    , platform as Types/Platform.Platform
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
    >> searchAncestorDirectories (fn (isDirectory & fileName): not isDirectory and fileName == modulesFileName) __
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
        >> List.find (fn umr: umrToFileName "" umr == entryModulePath) __

    entryUsr =
        try maybeEntryUmr as
            , Nothing:
                todo << "Error: you are asking me to compile module " .. entryModulePath .. " but I can't find it anywhere."

            , Just umr:
                USR umr "main"

    #
    # Figure out corelib's root
    #
    [ pars.selfPath ]
    >> Path.resolve
    >> Path.dirname
    >> searchAncestorDirectories (fn (isDirectory & fileName): isDirectory and fileName == libDirectoryName) __
    >> IO.onSuccess fn maybeCorelibParent:

    corePath =
        try maybeCorelibParent as
            , Nothing:
                todo << "Error: I expect to find the " .. libDirectoryName .. " directory next to the spcc executable " .. pars.selfPath .. " but I can't find it."

            , Just p:
                Path.resolve [ p, libDirectoryName ]

    outputFile =
        Maybe.withDefault pars.platform.defaultOutputPath pars.maybeOutputPath

    loadFile as fn UMR: IO (UMR & Text) =
        fn umr:
        IO.readFile (umrToFileName corePath umr)
        >> IO.onSuccess fn content:
        IO.succeed (umr & content)

    meta.moduleVisibleAsToUmr
    >> Dict.values
    >> List.map loadFile __
    >> IO.parallel
    >> IO.onSuccess fn modulesAsText:

    Compiler/Compiler.compileModules
        {
        , platform = pars.platform
        , meta
        , umrToFsPath = umrToFileName corePath __
        , nativeValues = Prelude.coreNativeValues
        }
        modulesAsText
        [entryUsr]
    >> onResSuccess fn (targetsByName & compiledStatements):

    getMainName targetsByName
    >> onResSuccess fn mainName:

    pars.platform.makeExecutable mainName compiledStatements
    >> IO.writeFile outputFile __
    >> IO.onSuccess fn _:

    IO.writeStdout __ << "---> " .. outputFile .. " written. =)"

