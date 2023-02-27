

allowModule as Text: Bool =
    filename:

    not << List.any (n: Text.contains n filename) [
        , "src/Types/TypedAst.sp"
#        , "src/Types/Token.sp"
#        , "src/Types/Pos.sp"
        , "src/Types/Platform.sp"
#        , "src/Types/Op.sp"
#        , "src/Types/Meta.sp"
        , "src/Types/JavascriptAst.sp"
#        , "src/Types/FormattableAst.sp"
        , "src/Types/EmittableAst.sp"
#        , "src/Types/CanonicalAst.sp"
#        , "src/Types/Ast.sp"
#        , "/src/Test.sp"
#        , "src/Term.sp"
        , "src/Targets/Javascript/Runtime.sp"
        , "src/Targets/Javascript/JsToText.sp"
        , "src/Targets/Javascript/EmittableToJs.sp"
        , "src/Targets/Javascript/CanonicalToJs_Test.sp"
        , "src/StateMonad.sp"
        , "src/SPON.sp"
#        , "src/SPLib/Parser.sp"
#        , "src/SPLib/Buffer.sp"
        , "src/RefHierarchy_Test.sp"
        , "src/RefHierarchy.sp"
#        , "src/Prelude.sp"
        , "src/Platforms/RawJavaScript.sp"
        , "src/Platforms/Posix.sp"
        , "src/Platforms/Browser.sp"
        , "src/ModulesFile.sp"
        , "src/Main.sp"
        , "src/Human/CanonicalAst.sp"
        , "src/Human/Type.sp"
        , "src/DefaultModules.sp"
        , "src/Compiler/UniquenessCheck.sp"
        , "src/Compiler/TypeCheck_Test.sp"
        , "src/Compiler/TypeCheck.sp"
#        , "src/Compiler/TestHelpers.sp"
#        , "src/Compiler/Parser_Test.sp"
#        , "src/Compiler/Parser.sp"
        , "src/Compiler/MakeEmittable.sp"
        , "src/Compiler/MakeCanonical_Test.sp"
        , "src/Compiler/MakeCanonical.sp"
#        , "src/Compiler/Lexer_Test.sp"
#        , "src/Compiler/Lexer.sp"
#        , "src/Compiler/Error.sp"
#        , "src/Compiler/CoreTypes.sp"
        , "src/Compile.sp"
        , "specs/Uniqueness.sp"
        , "lib/posix/Path.sp"
#        , "lib/posix/IO.sp"
#        , "lib/core/Tuple.sp"
#        , "lib/core/Text.sp"
#        , "lib/core/Set.sp"
#        , "lib/core/Result.sp"
#        , "lib/core/Maybe.sp"
#        , "lib/core/List_Test.sp"
#        , "lib/core/List.sp"
#        , "lib/core/Hash_Test.sp"
#        , "lib/core/Hash.sp"
#        , "lib/core/Dict_Test.sp"
#        , "lib/core/Dict.sp"
#        , "lib/core/Debug.sp"
#        , "lib/core/Core.sp"
#        , "lib/core/Basics.sp"
#        , "lib/core/Array_Test.sp"
#        , "lib/core/Array.sp"
      ]



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
loadModulesFile as Types/Platform.Platform: Text: IO ModulesFile.ModulesFile =
    platform: projectRoot:

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
                platform.defaultModules

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
umrToFileName as Text: UMR: Text =
    corePath: umr:

    UMR source name =
        umr

    try source as
        Meta.SourceDir d:
            Path.resolve [ d, name .. ".sp" ]

        Meta.Core:
            Path.resolve (corePath :: "core" :: (Text.split "/" << name .. ".sp"))

        Meta.Posix:
            Path.resolve (corePath :: "posix" :: (Text.split "/" << name .. ".sp"))

        Meta.Browser:
            Path.resolve (corePath :: "browser" :: (Text.split "/" << name .. ".sp"))


loadModule as Meta: UMR: Text: IO CA.Module =
    meta: umr: fileName:

    # TODO get rid of eenv so this is not needed
    UMR source moduleName =
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


sdItemToUMR as Meta.Source: Text: UMR =
    source: fileName:
    fileName
    >> Text.replace ".sp" ""
    >> UMR source


updateSourceDir as [Text]: ModulesFile.SourceDir: ModulesFile.SourceDir =
    fileNames:

    insertModuleName as Text: ModulesFile.SourceDir: ModulesFile.SourceDir =
        name: sd:
        try List.find (m: m.path == name) sd.modules as
            Just _: sd
            Nothing: { sd with modules = { path = name, visibleAs = name, globalValues = [], globalTypes = [] } :: .modules }

    List.for fileNames insertModuleName


loadMeta as IO.Env: Types/Platform.Platform: Text: Text: IO Meta =
    env: platform: entryModuleDir: projectRoot:

    loadModulesFile platform projectRoot
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
#    if userModule.umr == UMR Meta.Core "Tuple" then
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



alias CompileMainPars = {
    , env as IO.Env
    , selfPath as Text
    , entryModulePath as Text
    , maybeOutputPath as Maybe Text
    , platform as Types/Platform.Platform
    }


compileMain as CompileMainPars: IO Int =
    pars:

    #
    # Figure out project root
    #
    entryModulePath =
        Path.resolve [ pars.entryModulePath ]



#    IO.readFile entryModulePath
#    >> IO.onSuccess moduleAsText:
#
#    moduleAsText
#    >> Compiler/Parser.textToFormattableModule { moduleName = entryModulePath, stripLocations = False }
#    >> resToIo { moduleByName = Dict.singleton entryModulePath { fsPath = entryModulePath, content = moduleAsText } }
#    >> IO.onSuccess out:
#
#    log "==" out
#
#    IO.writeStdout "done"


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
    loadMeta pars.env pars.platform entryModuleDir projectRoot
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
                USR umr "main"

    #
    # Figure out corelib's root
    #
    [ pars.selfPath ]
    >> Path.resolve
    >> Path.dirname
    >> searchAncestorDirectories ((isDirectory & fileName): isDirectory and fileName == libDirectoryName)
    >> IO.onSuccess maybeCorelibParent:


    corePath =
        try maybeCorelibParent as
            Nothing:
                todo << "Error: I expect to find the " .. libDirectoryName .. " directory next to the spcc executable " .. pars.selfPath .. " but I can't find it."

            Just p:
                Path.resolve [ p, libDirectoryName ]


    #
    #
    #
    outputFile =
        Maybe.withDefault pars.platform.defaultOutputPath pars.maybeOutputPath


    log "Loading modules..." ""
    loadAllModules as IO [CA.Module] =
        meta.moduleVisibleAsToUmr
        >> Dict.values
        >> List.filter (umr: allowModule (umrToFileName corePath umr))
        >> List.map (umr: loadModule meta umr (umrToFileName corePath umr))
        >> IO.parallel

    loadAllModules >> IO.onSuccess userModules:

    modules as Dict UMR CA.Module =
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
    modules
    >> Dict.values
    >> Compiler/TypeCheck.initStateAndGlobalEnv
    >> onResSuccess eenv (lastUnificationVarId & typeCheckGlobalEnv):

    log "Type checking..." ""

    modules
    >> Dict.values
    >> List.map (m: Compiler/TypeCheck.doModule lastUnificationVarId typeCheckGlobalEnv m >> resToIo eenv)
    >> IO.parallel
    >> IO.onSuccess typedModules:


    log "Uniqueness check..." ""

    typedModules
    >> List.map (m: Compiler/UniquenessCheck.doModule m >> resToIo eenv)
    >> IO.parallel
    >> IO.onSuccess modulesWithDestruction:


    log "Emittable AST..." ""

    modulesWithDestruction
    >> Compiler/MakeEmittable.translateAll
    >> Result.mapError (e: todo "MakeEmittable.translateAll returned Err")
    >> onResSuccess eenv (meState & emittableStatements):

    emittableState @=
        meState

    log "= Platform specific stuff ="
    js =
        pars.platform.compile
            {
            , errorEnv = eenv
            , constructors = Dict.toList (Dict.map (k: v: v.type) typeCheckGlobalEnv.constructors)
            }
            entryUsr
            @emittableState
            emittableStatements

    IO.writeFile outputFile js
    >> IO.onSuccess _:

    IO.writeStdout << "---> " .. outputFile .. " written. =)"

