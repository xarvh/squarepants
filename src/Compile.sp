
modulesFileName as Text =
    "modules.sp"


libDirectoryName as Text =
    "lib"


coreDirName as Text =
    "core"


#
# IO
#
formattedToConsoleColoredText as Error.FormattedText: Text =
    formattedText:
    try formattedText as
        Error.FormattedText_Default t: t
        Error.FormattedText_Emphasys t: Term.yellow t
        Error.FormattedText_Warning t: Term.red t
        Error.FormattedText_Decoration t: Term.blue t


resToIo as Error.Env: Res a: IO a =
    errorEnv: res:
    try res as
        Ok a: IO.succeed a
        Err e:
            e
            >> Error.toFormattedText errorEnv
            >> List.map formattedToConsoleColoredText
            >> Text.join ""
            >> IO.fail


onResSuccess as Error.Env: (a: IO b): Res a: IO b =
    errorEnv: f: res:
    res
    >> resToIo errorEnv
    >> IO.onSuccess f


#
# Load modules.sp
#
loadModulesFile as Text: IO ModulesFile.ModulesFile =
    projectRoot:

    path =
        [ projectRoot, modulesFileName ]
        >> Path.resolve

    log "Metafile: " path

    path
    >> IO.readFile
    >> IO.onResult result:

    modulesAsText =
        try result as
            Ok f:
                f

            Err _:
                log "Using default modules.sp"
                Platforms/Posix.platform.defaultModules

    eenv as Error.Env =
        {
        , moduleByName = Dict.singleton modulesFileName {
            , fsPath = modulesFileName
            , content = modulesAsText
            }
        }

    resToIo eenv << ModulesFile.textToModulesFile modulesFileName modulesAsText



#
# Module loading
#


asModule as (Bool & Text): Maybe Text =
    tuple:
    isDirectory & name = tuple

    if isDirectory or Text.startsWithRegex "[A-Z][a-zA-Z0-9_]*[.]sp$" name /= name then
        Nothing
    else
        name
            >> Text.replace ".sp" ""
            >> Just


asModuleDirectory as (Bool & Text): Maybe Text =
    tuple:
    isDirectory & name = tuple

    if isDirectory and Text.startsWithRegex "^[A-Z][a-zA-Z0-9_]*$" name == name then
        Just name
    else
        Nothing


listSourceDir as Text: Text: IO [Text] =
    sourceDirRoot: modulePathWithTrailingSlash:

    path =
        sourceDirRoot .. "/" .. modulePathWithTrailingSlash

    IO.readDir path >> IO.onSuccess dirContents:

    directChildren =
        dirContents
        >> List.filterMap asModule
        >> List.map (fileName: modulePathWithTrailingSlash .. fileName)

    getDescendants as IO [[Text]] =
        dirContents
        >> List.filterMap asModuleDirectory
        >> List.map (subDir: listSourceDir sourceDirRoot (modulePathWithTrailingSlash .. subDir .. "/"))
        >> IO.parallel

    getDescendants >> IO.onSuccess descendants:

    [ directChildren, List.concat descendants ]
    >> List.concat
    >> IO.succeed


# TODO move this to Meta?
umrToFileName as Text: Meta.UniqueModuleReference: Text =
    corePath: umr:

    Meta.UMR source name =
        umr

    try source as
        Meta.SourceDir d:
            Path.resolve [ d, name .. ".sp" ]

        Meta.Core:
            Path.resolve (corePath :: "core" :: (Text.split "/" << name .. ".sp"))

        Meta.Posix:
            Path.resolve (corePath :: "posix" :: (Text.split "/" << name .. ".sp"))


loadModule as Meta: Meta.UniqueModuleReference: Text: IO CA.Module =
    meta: umr: fileName:

    # TODO get rid of eenv so this is not needed
    Meta.UMR source moduleName =
        umr

    IO.readFile fileName  >> IO.onSuccess moduleAsText:

    params as Compiler/MakeCanonical.Params = {
        , meta
        , stripLocations = False
        , source
        , name = moduleName
        }

    eenv as Error.Env = {
        , moduleByName = Dict.singleton moduleName {
            , fsPath = fileName
            , content = moduleAsText
            }
        }

    resToIo eenv << Compiler/MakeCanonical.textToCanonicalModule params moduleAsText


alias ModuleAndPath = {
    , moduleName as Text
    , filePath as Text
    }


sdItemToUMR as Meta.Source: Text: Meta.UniqueModuleReference =
    source: fileName:
    fileName
    >> Text.replace ".sp" ""
    >> Meta.UMR source


updateSourceDir as [Text]: ModulesFile.SourceDir: ModulesFile.SourceDir =
    fileNames:

    insertModuleName as Text: ModulesFile.SourceDir: ModulesFile.SourceDir =
        name: sd:
        try List.find (m: m.path == name) sd.modules as
            Just _: sd
            Nothing: { sd with modules = { path = name, visibleAs = name, globalValues = [], globalTypes = [] } :: .modules }

    List.for fileNames insertModuleName


loadMeta as IO.Env: Text: Text: IO Meta =
    env: entryModuleDir: projectRoot:

    loadModulesFile projectRoot
    >> IO.onSuccess modulesFileRaw:

    resolvedDirs =
        modulesFileRaw.sourceDirs
        >> List.map sd: { sd with path = Path.resolve [ projectRoot, .path ] }

    allDirs =
        if List.any (sd: sd.path == entryModuleDir) resolvedDirs then
            resolvedDirs
        else
            { path = entryModuleDir, modules = [] } :: resolvedDirs

    modulesFile =
        { modulesFileRaw with sourceDirs = allDirs }

    # sourceDirs does not contain all modules available in the dir, but only the exceptions;
    # before building Meta we need to add those that are not mentioned.
    getAllSourceDirLists as IO [[Text]] =
        modulesFile.sourceDirs
        >> List.map (sd: listSourceDir sd.path "")
        >> IO.parallel

    getAllSourceDirLists
    >> IO.onSuccess allSourceDirLists:

    updatedSourceDirs as [ModulesFile.SourceDir] =
        List.map2 updateSourceDir allSourceDirLists modulesFile.sourceDirs

    { modulesFile with sourceDirs = updatedSourceDirs }
    >> ModulesFile.toMeta
    >> IO.succeed


#
# Compile
#

typeCheckModule as Meta: CA.Globals: CA.Module: Res Compiler/TypeCheck.Env =
    meta: globals: module:

    env as Compiler/TypeCheck.Env = {
        , currentModule = module.umr
        , meta
        , instanceVariables = Dict.mapKeys CA.RefRoot globals.instanceVariables
        , constructors = globals.constructors
        , types = globals.types
        , nonFreeTyvars = Dict.empty
        , nonAnnotatedRecursives = Dict.empty
        }

    Compiler/TypeCheck.fromModule env module


searchAncestorDirectories as (Bool & Text: Bool): Text: IO (Maybe Text) =
    isWantedFile: searchDir:

    IO.readDir searchDir
    >> IO.onResult result:

    try result as
        Err _:
            IO.succeed Nothing

        Ok dirContents:
            if List.any isWantedFile dirContents then
                searchDir
                >> Just
                >> IO.succeed

            else
                parent = Path.resolve [ searchDir, ".." ]
                if parent == searchDir then
                    IO.succeed Nothing
                else
                    parent >> searchAncestorDirectories isWantedFile

mergeWithCore as CA.Module: CA.Module: CA.Module =
    coreModule: userModule:

#    need to strip positions before merging >_<

#    xxx = Dict.join coreModule.valueDefs userModule.valueDefs
#
#    if userModule.umr == Meta.UMR Meta.Core "Tuple" then
#      List.each (Dict.keys xxx) k:
#        log "*" k
#      None
#    else
#      None

    { userModule with
    , aliasDefs = Dict.join coreModule.aliasDefs .aliasDefs
    , unionDefs = Dict.join coreModule.unionDefs .unionDefs
    , valueDefs = Dict.join coreModule.valueDefs .valueDefs
    }




compileMain as IO.Env: Text: Text: Maybe Text: IO None =
    env: selfPath: entryModulePathUnresolved: maybeOutputFile:

    #
    # Figure out project root
    #
    entryModulePath =
        Path.resolve [ entryModulePathUnresolved ]

    entryModuleDir =
        Path.dirname entryModulePath

    entryModuleDir
    >> searchAncestorDirectories ((isDirectory & fileName): not isDirectory and fileName == modulesFileName)
    >> IO.onSuccess maybeProjectRoot:

    projectRoot =
        Maybe.withDefault entryModuleDir maybeProjectRoot

    #
    # Load meta and figure out entry point's USR
    #
    loadMeta env entryModuleDir projectRoot
    >> IO.onSuccess meta:



    # This will be replaced once we get lazy loading
    maybeEntryUmr =
        meta.moduleVisibleAsToUmr
        >> Dict.values
        # TODO split umrToFileName so we can call it without `""`
        >> List.find umr: umrToFileName "" umr == entryModulePath

    entryUsr =
        try maybeEntryUmr as
            Nothing:
                todo << "Error: you are asking me to compile module " .. entryModulePath .. " but I can't find it anywhere."

            Just umr:
                Meta.USR umr "main"

    #
    # Figure out corelib's root
    #
    [ selfPath ]
    >> Path.resolve
    >> Path.dirname
    >> searchAncestorDirectories ((isDirectory & fileName): isDirectory and fileName == libDirectoryName)
    >> IO.onSuccess maybeCorelibParent:


    corePath =
        try maybeCorelibParent as
            Nothing:
                todo << "Error: I expect to find the " .. libDirectoryName .. " directory next to the spcc executable " .. selfPath .. " but I can't find it."

            Just p:
                Path.resolve [ p, libDirectoryName ]


    #
    #
    #
    outputFile =
        # TODO: the platform should provide the name
        Maybe.withDefault "out.js" maybeOutputFile


    log "Loading modules..." ""
    loadAllModules as IO [CA.Module] =
        meta.moduleVisibleAsToUmr
        >> Dict.values
        >> List.map (umr: loadModule meta umr (umrToFileName corePath umr))
        >> IO.parallel

    loadAllModules >> IO.onSuccess userModules:

    modules as Dict Meta.UniqueModuleReference CA.Module =
        Prelude.coreModulesByUmr >> List.for userModules module:
            Dict.update module.umr maybeCore:
                try maybeCore as
                    Nothing: Just module
                    Just core: Just (mergeWithCore core module)

    # TODO eenv should be eliminated completely, each module should have all the info necessary to produce errors
    eenv as Error.Env =
        getName = n:
            Meta.UMR source name = n.umr
            name

        { moduleByName =
            List.for (Dict.values modules) (m: Dict.insert (getName m) { fsPath = umrToFileName corePath m.umr, content = m.asText }) Dict.empty
        }


    log "Solving globals..." ""
    x as Res CA.Globals =
        Compiler/Pipeline.globalExpandedTypes modules

    x >> onResSuccess eenv globals:

    log "Type checking..." ""

    typeCheckModules =
        (Dict.values modules)
        >> List.map (m: typeCheckModule meta globals m >> resToIo eenv)
        >> IO.parallel

    typeCheckModules >> IO.onSuccess typeCheckEnvs:

    log "Emittable AST..." ""
    Compiler/MakeEmittable.translateAll (Dict.values modules)
    >> Result.mapError (e: todo "MakeEmittable.translateAll returned Err")
    >> onResSuccess eenv emittableStatements:

    log "= Platform specific stuff ="
    js =
        Platforms/Posix.compile {
            , errorEnv = eenv
            , constructors = Dict.toList globals.constructors
            }
            entryUsr
            emittableStatements

    IO.writeFile outputFile js


