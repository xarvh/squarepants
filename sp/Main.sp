
allTests =
    as [ Test ]

    [
#    , Compiler/Lexer_Test.tests
#    , Compiler/Parser_Test.tests
#    , Compiler/MakeCanonical_Test.tests
#    , Compiler/TypeCheck_Test.tests
#    , SPCore/List_Test.tests
#    , SPCore/Dict_Test.tests
    ]




#
# Errors
#


color code text =
    as Text: Text: Text

    code .. text .. "\x1b[0m"


blue =
    color  "\x1b[34m"


green =
    color "\x1b[32m"


yellow =
    color "\x1b[33m"


red =
    color "\x1b[31m"


indent s =
    as Text: Text
    s
      >> Text.split "\n"
      >> List.map (fn l: "  " .. l)
      >> Text.join "\n"


testOutcomeToText name code outcome =
    as Text: Text: Test.TestOutcome: Text

    try outcome as
        Test.Success:
            green << "* PASS: " .. name

        Test.Skipped:
            yellow << "* skip: " .. name

        Test.Error error:
            (red << "FAIL ! " .. name) .. "\n" .. indent code .. "\n" .. indent error


formattedToConsoleColoredText formattedText =
    as Error.FormattedText: Text
    try formattedText as
        Error.FormattedText_Default t: t
        Error.FormattedText_Emphasys t: yellow t
        Error.FormattedText_Warning t: red t
        Error.FormattedText_Decoration t: blue t


order outcome =
    as Test.TestOutcome: Int
    try outcome as
        Test.Success: 0
        Test.Skipped: 1
        Test.Error _: 2


#
# IO
#


resToIo errorEnv res =
    as Error.Env: Res a: IO a

    try res as
        Ok a: IO.succeed a
        Err e:
            e
                >> Error.toFormattedText errorEnv
                >> List.map formattedToConsoleColoredText
                >> Text.join ""
                >> IO.fail


onResSuccess errorEnv f res =
    as Error.Env: (a: IO b): Res a: IO b

    res
        >> resToIo errorEnv
        >> IO.onSuccess f


#
# Module loading
#


loadModulesFile =
    as IO ModulesFile.ModulesFile

    modulesFileName = "modules.sp"

    IO.readFile modulesFileName >> IO.onSuccess fn modulesAsText:

    eenv =
        as Error.Env
        {
        , moduleByName = Dict.singleton modulesFileName {
            , fsPath = modulesFileName
            , content = modulesAsText
            }
        }

    resToIo eenv << ModulesFile.textToModulesFile modulesFileName modulesAsText


asModule (isDirectory & name) =
    as (Bool & Text): Maybe Text

    if isDirectory or Text.startsWithRegex "[A-Z][a-zA-Z0-9_]*[.]sp$" name /= name:
        Nothing
    else
        name
            >> Text.replace ".sp" ""
            >> Just


asModuleDirectory (isDirectory & name) =
    as (Bool & Text): Maybe Text

    if isDirectory and Text.startsWithRegex "^[A-Z][a-zA-Z0-9_]*$" name == name:
        Just name
    else
        Nothing


listSourceDir sourceDirRoot modulePathWithTrailingSlash =
    as Text: Text: IO [Text]

    path =
        sourceDirRoot .. "/" .. modulePathWithTrailingSlash

    IO.readDir path >> IO.onSuccess fn dirContents:

    directChildren =
        dirContents
            >> List.filterMap asModule
            >> List.map (fn fileName: modulePathWithTrailingSlash .. fileName)

    getDescendants =
        as IO [[Text]]
        dirContents
            >> List.filterMap asModuleDirectory
            >> List.map (fn subDir: listSourceDir sourceDirRoot (modulePathWithTrailingSlash .. subDir .. "/"))
            >> IO.parallel

    getDescendants >> IO.onSuccess fn descendants:

    [ directChildren, List.concat descendants ]
        >> List.concat
        >> IO.succeed


# TODO move this to Meta?
umrToFileName (Meta.UMR source name) =
    as Meta.UniqueModuleReference: Maybe Text

    try source as
        Meta.SourceDir d:
            Just << d .. "/" .. name .. ".sp"
        _:
            Nothing


loadModule meta umr fileName =
    as Meta: Meta.UniqueModuleReference: Text: IO CA.Module

    # TODO get rid of eenv so this is not needed
    Meta.UMR source moduleName =
        umr

    IO.readFile fileName  >> IO.onSuccess fn moduleAsText:

    params =
        as Compiler/Pipeline.ModuleParams
        {
        , meta
        , stripLocations = False
        , source
        , name = moduleName
        , code = moduleAsText
        }

    eenv =
        as Error.Env
        {
        , moduleByName = Dict.singleton moduleName {
            , fsPath = fileName
            , content = moduleAsText
            }
        }

    resToIo eenv << Compiler/Pipeline.textToCanonicalModule params



alias ModuleAndPath = {
    , moduleName as Text
    , filePath as Text
    }


sdItemToUMR source fileName =
    as Meta.Source: Text: Meta.UniqueModuleReference

    fileName
        >> Text.replace ".sp" ""
        >> Meta.UMR source


updateSd fileNames =
    as [Text]: ModulesFile.SourceDir: ModulesFile.SourceDir

    insertModuleName name sd =
        as Text: ModulesFile.SourceDir: ModulesFile.SourceDir
        try List.find (fn m: m.path == name) sd.modules as
            Just _: sd
            Nothing: { sd with modules = { path = name, visibleAs = name, globalValues = [], globalTypes = [] } :: .modules }

    List.foldl insertModuleName fileNames


loadMeta env =
    as IO.Env: IO Meta

    loadModulesFile >> IO.onSuccess fn modulesFile:

    # sourceDirs does not contain all modules available in the dir, but only the exceptions;
    # before building Meta we need to add those that are not mentioned.
    getAllSourceDirLists =
        as IO [[Text]]
        modulesFile.sourceDirs
            >> List.map (fn sd: listSourceDir sd.path "")
            >> IO.parallel

    getAllSourceDirLists >> IO.onSuccess fn allSourceDirLists:

    updatedSourceDirs =
        as [ModulesFile.SourceDir]
        List.map2 updateSd allSourceDirLists modulesFile.sourceDirs

    { modulesFile with sourceDirs = updatedSourceDirs }
        >> ModulesFile.toMeta
        >> IO.succeed


#
# Compile
#

typeCheckModule meta globals module =
    as Meta: CA.Globals: CA.Module: Res Compiler/TypeCheck.Env

    env =
        as Compiler/TypeCheck.Env
        {
        , currentModule = module.umr
        , meta
        , instanceVariables = globals.instanceVariables
        , constructors = globals.constructors
        , types = globals.types
        , nonFreeTyvars = Dict.empty
        , nonAnnotatedRecursives = Dict.empty
        }

    Compiler/TypeCheck.fromModule env module


getTargetUsr meta entryModule entryValue globals =
    as Meta: Text: Text: CA.Globals: Res Meta.UniqueSymbolReference

    # TODO translate entryModule?

    asEntry ref =
        as CA.Ref: Maybe Meta.UniqueSymbolReference
        try ref as
            CA.RefBlock _: Nothing
            CA.RefRoot usr:
                Meta.USR (Meta.UMR source moduleName) valueName =
                    usr

                if moduleName == entryModule and valueName == entryValue:
                    Just usr
                else
                    Nothing

    possibleTargets =
        as [Meta.UniqueSymbolReference]
        globals.instanceVariables
            >> Dict.keys
            >> List.filterMap asEntry

    try possibleTargets as
        []:
            Debug.todo << "Can't find build target `" .. entryModule .. "." .. entryValue .. "` anywhere."

        [ usr ]:
            Ok usr

        many:
            x =
                many
                    >> List.map Debug.toHuman
                    >> Text.join ", "
            Debug.todo << "Multiple values match build target `" .. entryModule .. "." .. entryValue .. "`: " .. x



compile env entryModule entryValue outputFile =
    as IO.Env: Text: Text: Text: IO None

    log "Loading meta..." ""
    loadMeta env >> IO.onSuccess fn meta:

    log "Loading modules..." ""
    loadAllModules =
        as IO [CA.Module]
        meta.moduleVisibleAsToUmr
            >> Dict.values
            >> List.filterMap (fn umr: Maybe.map (loadModule meta umr) (umrToFileName umr))
            >> IO.parallel

    loadAllModules >> IO.onSuccess fn modules:

    # TODO eenv should be eliminated completely, each module should have all the info necessary to produce errors
    eenv =
        as Error.Env

        getName { umr = Meta.UMR source name } =
            name

        { moduleByName =
            List.foldl (fn m: Dict.insert (getName m) { fsPath = Maybe.withDefault "CORE" << umrToFileName m.umr, content = m.asText }) modules Dict.empty
        }


    log "Solving globals..." ""
    x =
        as Res CA.Globals
        Compiler/Pipeline.globalExpandedTypes (List.indexBy (fn m: m.umr) modules)

    x >> onResSuccess eenv fn globals:

    getTargetUsr meta entryModule entryValue globals >> onResSuccess eenv fn targetUsr:

    log "Type checking..." ""

    typeCheckModules =
        modules
            >> List.map (fn m: typeCheckModule meta globals m >> resToIo eenv)
            >> IO.parallel

    typeCheckModules >> IO.onSuccess fn typeCheckEnvs:

    log "Creating JS AST..." ""
    jaStatements =
        Compiler/CanonicalToJs.translateAll eenv globals modules

    log "Emitting JS..." ""

    callMain =
        """

        const out = """ .. Compiler/CanonicalToJs.translateUsr targetUsr .. """({})(array_toList(process.argv.slice(1)))[1]('never');
        if (out[1]) console.error(out[1]);
        """

    statements =
        jaStatements
            >> List.map (Compiler/JsToText.emitStatement 0)
            >> Text.join "\n\n"

    js =
        Compiler/CanonicalToJs.nativeDefinitions .. statements .. callMain

    IO.writeFile outputFile js



#
# main
#

main env args =
    as IO.Program

    try args as
        [ self ]:
            allTests
                >> Test.flatten
                >> List.sortBy (fn (name & code & outcome): order outcome & name)
                >> List.map (fn (name & code & outcome): testOutcomeToText name code outcome)
                >> Text.join "\n"
                >> IO.writeStdout

        [ self, testFile ]:
            umr = Meta.UMR (Meta.SourceDir "") testFile

            loadModule Compiler/TestHelpers.defaultMeta umr testFile >> IO.onSuccess fn caModule:
            IO.succeed None

        [ self, entryModule, entryValue, outputFile ]:
            compile env entryModule entryValue outputFile

        _:
            """
            Usage

            spcc EntryModule entryFunction outputFile.js
            """
                >> IO.writeStdout

