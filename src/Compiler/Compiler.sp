

union ExposedValue =
    ExposedValue
    #Never ExposedValue


expose as fn a: ExposedValue =
    fn a:
    todo "native"


union CompiledValue =
    , FUCK_AROUND_AND_FIND_OUT Text TA.RawType Text


dynamicLoad as fn CompiledValue, (fn specific: general): Result Text general =
    fn compiledValue, variantConstructor:
    todo "native"


#
# ...
#


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


compileModules as fn Config, [UMR & Text], UMR: Res CompiledValue =
    fn config, modulesAsText, mainModule:

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


    mainName =
        USR mainModule "main"
        >> Compiler/MakeEmittable.translateUsr @emittableState __

    mainType as TA.RawType =
        try List.find (fn mod: mod.umr == mainModule) modulesWithDestruction as
            , Nothing:
                todo "error can't find specified mainModule"

            , Just mod:
                getMain =
                    fn def:
                    Dict.get "main" (TA.patternNames def.pattern)

                try mod.valueDefs >> Dict.values >> List.filterMap getMain __ as
                    , [ { with type } ]: type.raw
                    , _: todo "specified mainModule does not have a 'main'"

    Ok << FUCK_AROUND_AND_FIND_OUT mainName mainType programWithoutRuntime

