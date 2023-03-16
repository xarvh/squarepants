

platform as Types/Platform.Platform =
    {
    , name = "rawjs"
    , defaultModules = DefaultModules.asText
    , quickstart = "TODO"
    , defaultOutputPath = "squarepants.mjs"
    , compileStatements = fn _, @z, _: todo "rawjs: compileStatements"
    , makeExecutable = fn _, _: todo "rawjs: makeExecutable"
    }




compile as fn Types/Platform.GetRidOfMe, USR, @Compiler/MakeEmittable.State, [EA.GlobalDefinition]: Text =
    fn getRidOfMe, targetUsr, @emState, emittableStatements:

    { constructors } =
        getRidOfMe

    log "Creating JS AST..." ""
    jaStatements =
        Targets/Javascript/EmittableToJs.translateAll @emState
            {
            , constructors
            , eaDefs = emittableStatements
            , platformOverrides = []
            }

    log "Emitting JS..." ""

    statements =
        jaStatements
        >> List.map (Targets/Javascript/JsToText.emitStatement 0 __) __
        >> Text.join "\n\n" __

    main =
        Compiler/MakeEmittable.translateUsr @emState targetUsr

    # TODO don't want to use a server, so `export default` will have to wait
    "(function() {\n" .. Targets/Javascript/Runtime.nativeDefinitions .. statements .. "\n return " .. main .. ";\n })();"

