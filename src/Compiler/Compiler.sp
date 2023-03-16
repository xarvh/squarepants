

# Issue: is Core.Type compatible with CA.Type or with TA.Raw?

# Issue: We're compiling to a Text and assuming that the runtime can be added to it

# Issue: How do we prevent arbitrary code insertion?


union ExposedValue =
    ExposedValue
    #Never ExposedValue


expose as fn a: ExposedValue =
    fn a:
    todo "native"



alias Config =
    {
    , platform as Types/Platform.Platform
    , meta as Meta
    , umrToFsPath as fn UMR: Text

    # We automaticaly expose all union types referenced by these, and their constructors
    , nativeValues as Dict USR { type as CA.RawType, value as ExposedValue }

    # TODO
    #, nativeAliases as Dict USR { value as Core.Type, args as [Text] }
    }


compileModules as fn Config, [UMR & Text], [USR]: Res (Dict Name TA.Type & Text) =
    fn config, modulesAsText, targets:

    log "Loading modules..." ""

    loadModule as fn (UMR & Text): Res CA.Module =
        fn (umr & content):
        Compiler/MakeCanonical.textToCanonicalModule False
            {
            , meta = config.meta
            , umr
            , errorModule =
                {
                , fsPath = config.umrToFsPath umr
                , content
                }
            }

    modulesAsText
    >> List.mapRes loadModule __
    >> onOk fn userModules:

    log "Type checking..." ""
    Compiler/TypeCheck.initStateAndGlobalEnv [#config.nativeAliases#] config.nativeValues userModules
    >> onOk fn (luv & typeCheckGlobalEnv):

    !lastUnificationVarId =
        cloneImm luv

    userModules
    >> List.mapRes (Compiler/TypeCheck.doModule @lastUnificationVarId typeCheckGlobalEnv __) __
    >> onOk fn typedModules:
    log "Uniqueness check..." ""

    typedModules
    >> List.mapRes (Compiler/UniquenessCheck.doModule __) __
    >> onOk fn modulesWithDestruction:

    mainTypeByName =
        todo "mainTypeByName"

    log "Emittable AST..." ""
    modulesWithDestruction
    >> Compiler/MakeEmittable.translateAll
    >> onOk fn (meState & emittableStatements):

    # This is used to translate USRs?
    !emittableState =
        cloneImm meState

    log "Platform..." ""
    programWithoutRuntime =
        config.platform.compileStatements
            { constructors = Dict.toList (Dict.map (fn k, v: v.type) typeCheckGlobalEnv.constructors) }
            @emittableState
            emittableStatements

    Ok (mainTypeByName & programWithoutRuntime)

