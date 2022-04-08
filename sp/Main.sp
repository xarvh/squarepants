
allTests as [ Test ] = [
    , Compiler/Lexer_Test.tests
    , Compiler/Parser_Test.tests
    , Compiler/MakeCanonical_Test.tests
    , Compiler/TypeCheck_Test.tests
    #, Compiler/CanonicalToJs_Test.tests
    , Hash_Test.tests
    , Array_Test.tests
    , List_Test.tests
    , Dict_Test.tests
    , RefHierarchy_Test.tests
    ]




#
# Errors
#


color as Text: Text: Text =
    code: text:

    code .. text .. "\x1b[0m"


blue as Text: Text =
    color  "\x1b[34m"


green as Text: Text =
    color "\x1b[32m"


yellow as Text: Text =
    color "\x1b[33m"


red as Text: Text =
    color "\x1b[31m"


indent as Text: Text =
    s:
    s
      >> Text.split "\n"
      >> List.map (l: "  " .. l)
      >> Text.join "\n"


testOutcomeToText as Text: Text: Test.TestOutcome: Text =
    name: code: outcome:

    try outcome as
        Test.Success:
            green << "* PASS: " .. name

        Test.Skipped:
            yellow << "* skip: " .. name

        Test.Error error:
            (red << "FAIL ! " .. name) .. "\n" .. indent code .. "\n" .. indent error


formattedToConsoleColoredText as Error.FormattedText: Text =
    formattedText:
    try formattedText as
        Error.FormattedText_Default t: t
        Error.FormattedText_Emphasys t: yellow t
        Error.FormattedText_Warning t: red t
        Error.FormattedText_Decoration t: blue t


order as Test.TestOutcome: Int =
    outcome:
    try outcome as
        Test.Success: 0
        Test.Skipped: 1
        Test.Error _: 2


#
# IO
#


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
# Module loading
#


loadModulesFile as IO ModulesFile.ModulesFile =

    modulesFileName = "modules.sp"

    IO.readFile modulesFileName >> IO.onSuccess modulesAsText:

    eenv as Error.Env =
        {
        , moduleByName = Dict.singleton modulesFileName {
            , fsPath = modulesFileName
            , content = modulesAsText
            }
        }

    resToIo eenv << ModulesFile.textToModulesFile modulesFileName modulesAsText


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
umrToFileName as Meta.UniqueModuleReference: Maybe Text =
    x:

    Meta.UMR source name =
        x

    try source as
        Meta.SourceDir d:
            Just << d .. "/" .. name .. ".sp"
        _:
            Nothing


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


updateSd as [Text]: ModulesFile.SourceDir: ModulesFile.SourceDir =
    fileNames:

    insertModuleName as Text: ModulesFile.SourceDir: ModulesFile.SourceDir =
        name: sd:
        try List.find (m: m.path == name) sd.modules as
            Just _: sd
            Nothing: { sd with modules = { path = name, visibleAs = name, globalValues = [], globalTypes = [] } :: .modules }

    List.for fileNames insertModuleName


loadMeta as IO.Env: IO Meta =
    env:

    loadModulesFile >> IO.onSuccess modulesFile:

    # sourceDirs does not contain all modules available in the dir, but only the exceptions;
    # before building Meta we need to add those that are not mentioned.
    getAllSourceDirLists as IO [[Text]] =
        modulesFile.sourceDirs
            >> List.map (sd: listSourceDir sd.path "")
            >> IO.parallel

    getAllSourceDirLists >> IO.onSuccess allSourceDirLists:

    updatedSourceDirs as [ModulesFile.SourceDir] =
        List.map2 updateSd allSourceDirLists modulesFile.sourceDirs

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
        , instanceVariables = globals.instanceVariables
        , constructors = globals.constructors
        , types = globals.types
        , nonFreeTyvars = Dict.empty
        , nonAnnotatedRecursives = Dict.empty
        }

    Compiler/TypeCheck.fromModule env module


getTargetUsr as Meta: Text: Text: CA.Globals: Res Meta.UniqueSymbolReference =
    meta: entryModule: entryValue: globals:

    # TODO translate entryModule?

    asEntry as CA.Ref: Maybe Meta.UniqueSymbolReference =
        ref:
        try ref as
            CA.RefBlock _: Nothing
            CA.RefRoot usr:
                Meta.USR (Meta.UMR source moduleName) valueName =
                    usr

                if moduleName == entryModule and valueName == entryValue then
                    Just usr
                else
                    Nothing

    possibleTargets as [Meta.UniqueSymbolReference] =
        globals.instanceVariables
            >> Dict.keys
            >> List.filterMap asEntry

    try possibleTargets as
        []:
            SPCore.todo << "Can't find build target `" .. entryModule .. "." .. entryValue .. "` anywhere."

        [ usr ]:
            Ok usr

        many:
            x =
                many
                    >> List.map SPCore.toHuman
                    >> Text.join ", "
            SPCore.todo << "Multiple values match build target `" .. entryModule .. "." .. entryValue .. "`: " .. x


compile as IO.Env: Text: Text: Text: IO None =
    env: entryModule: entryValue: outputFile:

    log "Loading meta..." ""
    loadMeta env >> IO.onSuccess meta:

    log "Loading modules..." ""
    loadAllModules as IO [CA.Module] =
        meta.moduleVisibleAsToUmr
            >> Dict.values
            >> List.filterMap (umr: Maybe.map (loadModule meta umr) (umrToFileName umr))
            >> IO.parallel

    loadAllModules >> IO.onSuccess modules:

    # TODO eenv should be eliminated completely, each module should have all the info necessary to produce errors
    eenv as Error.Env =
        getName = n:
            { umr = Meta.UMR source name } = n
            name

        { moduleByName =
            List.for modules (m: Dict.insert (getName m) { fsPath = Maybe.withDefault "CORE" << umrToFileName m.umr, content = m.asText }) Dict.empty
        }


    log "Solving globals..." ""
    x as Res CA.Globals =
        Compiler/Pipeline.globalExpandedTypes (List.indexBy (m: m.umr) modules)

    x >> onResSuccess eenv globals:

    getTargetUsr meta entryModule entryValue globals >> onResSuccess eenv targetUsr:

    log "Type checking..." ""

    typeCheckModules =
        modules
            >> List.map (m: typeCheckModule meta globals m >> resToIo eenv)
            >> IO.parallel

    typeCheckModules >> IO.onSuccess typeCheckEnvs:

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

main as IO.Program =
    env: args:

    try args as
        [ self ]:
            allTests
                >> Test.flatten
                >> List.sortBy (x: order x.outcome & x.name)
                >> List.map (x: testOutcomeToText x.name x.code x.outcome)
                >> Text.join "\n"
                >> IO.writeStdout

        [ self, testFile ]:
            umr = Meta.UMR (Meta.SourceDir "") testFile

            loadModule TH.meta umr testFile >> IO.onSuccess caModule:
            IO.succeed None

        [ self, entryModule, entryValue, outputFile ]:
            compile env entryModule entryValue outputFile

        _:
            """
            Usage

            spcc EntryModule entryFunction outputFile.js
            """
                >> IO.writeStdout

