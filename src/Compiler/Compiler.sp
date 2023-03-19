#
# Compile Modules
#
alias CompileModulesPars =
    {
#    , platform as Types/Platform.Platform
    , meta as Meta
    , umrToFsPath as fn UMR: Text

    # TODO: in theory, we should expose all union types referenced by these, and their constructors
    # In practice, I hope we'll get structural variant types before this becomes necessary
    , nativeValues as Dict USR CA.RawType

    # TODO
    #, nativeAliases as Dict USR { value as Core.Type, args as [Text] }

    , entryModule as UMR
    , modules as [UMR & Text]
    }


alias CompileModulesOut =
    {
    , entryName as Text
    , type as TA.RawType
    , state as Compiler/MakeEmittable.State
    , defs as [EA.GlobalDefinition]
    , constructors as [USR & TA.FullType]
    }


compileModules as fn CompileModulesPars: Res CompileModulesOut =
    fn pars:

    log "Loading modules..." ""

    loadModule as fn (UMR & Text): Res CA.Module =
        fn (umr & content):

        # TODO move textToCanonicalModule here
        Compiler/MakeCanonical.textToCanonicalModule False
            {
            , meta = pars.meta
            , umr
            , errorModule =
                {
                , fsPath = pars.umrToFsPath umr
                , content
                }
            }

    pars.modules
    >> List.mapRes loadModule __
    >> onOk fn userModules:

    log "Type checking..." ""
    Compiler/TypeCheck.initStateAndGlobalEnv [#pars.nativeAliases#] pars.nativeValues userModules
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
    >> Compiler/MakeEmittable.translateAll pars.entryModule __
    >> onOk fn { entryName, state, defs }:

    type as TA.RawType =
        try List.find (fn mod: mod.umr == pars.entryModule) modulesWithDestruction as
            , Nothing:
                todo "error can't find specified entryModule"

            , Just mod:
                getMain =
                    fn def:
                    Dict.get "main" (TA.patternNames def.pattern)

                try mod.valueDefs >> Dict.values >> List.filterMap getMain __ as
                    , [ { with type } ]: type.raw
                    , _: todo "specified entryModule does not have a 'main'"

    constructors =
        Dict.toList (Dict.map (fn k, v: v.type) typeCheckGlobalEnv.constructors)

    Ok { constructors, entryName, type, state, defs }


#
# Dynamic loading
#

#union ExposedValue =
#    ExposedValue
#    #Never ExposedValue
#
#
#expose as fn a: ExposedValue =
#    fn a:
#    todo "native"


dynamicLoad as fn CompileModulesPars: fn (fn specific: general): Result Text general =
    fn pars:
    todo "native"








