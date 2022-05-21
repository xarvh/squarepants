

platform as Types/Platform.Platform = {
    , name = "posix"
    , compile
    , defaultModules = DefaultModules.asText .. posixModules
    , quickstart = "TODO"
    }


posixModules as Text =
    """

sourceDir =
    path = "platforms/posix"

    module =
        path = IO
        importAs = IO
        globalTypes = IO
    """


compile as Types/Platform.GetRidOfMe: Meta.UniqueSymbolReference: [EA.GlobalDefinition]: Text =
    getRidOfMe: targetUsr: emittableStatements:

    { errorEnv = eenv, constructors } =
        getRidOfMe

    log "Creating JS AST..." ""
    jaStatements =
        Compiler/EmittableToJs.translateAll eenv constructors emittableStatements

    log "Emitting JS..." ""

    callMain =
        """

        const out = """ .. Compiler/MakeEmittable.translateUsr targetUsr .. """({})(array_toList(process.argv.slice(1)))[1]('never');
        if (out[1]) console.error(out[1]);
        """

    statements =
        jaStatements
            >> List.map (Compiler/JsToText.emitStatement 0)
            >> Text.join "\n\n"

    Compiler/CanonicalToJs.nativeDefinitions .. statements .. callMain
