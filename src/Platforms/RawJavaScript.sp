

platform as Types/Platform.Platform = {
    , name = "rawjs"
    , compile
    , defaultModules = DefaultModules.asText
    , quickstart = "TODO"
    , defaultOutputPath = "squarepants.mjs"
    }


compile as Types/Platform.GetRidOfMe: USR: Compiler/MakeEmittable.State@: [EA.GlobalDefinition]: Text =
    getRidOfMe: targetUsr: emState@: emittableStatements:

    { errorEnv = eenv, constructors } =
        getRidOfMe

    log "Creating JS AST..." ""
    jaStatements =
        Targets/Javascript/EmittableToJs.translateAll @emState {
            , errorEnv = eenv
            , caConstructors = constructors
            , eaDefs = emittableStatements
            , platformOverrides = []
            }

    log "Emitting JS..." ""

    statements =
        jaStatements
            >> List.map (Targets/Javascript/JsToText.emitStatement 0)
            >> Text.join "\n\n"

    main =
        Compiler/MakeEmittable.translateUsr @emState targetUsr

    # TODO don't want to use a server, so `export default` will have to wait
    "(function() {\n" .. Targets/Javascript/Runtime.nativeDefinitions .. statements .. "\n return " .. main .. ";\n })();"

