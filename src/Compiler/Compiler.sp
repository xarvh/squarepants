#
# Compile Modules
#
alias CompileModulesPars =
    {
    , meta as Meta
    , umrToFsPath as fn UMR: Text

    # TODO: in theory, we should expose all union types referenced by these, and their constructors
    # In practice, I hope we'll get structural variant types before this becomes necessary
    , exposedValues as [ USR & Self.Self ]

    # TODO
    #, nativeAliases as Dict USR { value as Core.Type, args as [Text] }

    , entryModule as UMR
    , modules as [UMR & Text]
    }


exposedValueToUsrAndInstance as fn USR & Self.Self: USR & Compiler/TypeCheck.Instance =
    fn usr & exposed:

    { with raw } =
        exposed

    makeTyvar as fn TA.TyvarId, None: TA.Tyvar =
        fn tyvarId, None:
        {
        , generalizedAt = Pos.N
        , generalizedFor = RefGlobal usr
        , originalName = ""
        , allowFunctions = True # TODO not (List.member tyvarId exposed.nonFn)
        }

    freeTyvars as Dict TA.TyvarId TA.Tyvar =
        raw
        >> TA.typeTyvars
        >> Dict.map makeTyvar __

    usr
    &
    {
    , definedAt = Pos.N
    , type = { raw, uni = Imm }
    , freeTyvars
    , freeUnivars = Dict.empty
    }


compileModules as fn CompileModulesPars: Res Self.LoadPars =
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

    userModules
    >> Compiler/TypeCheck.initStateAndGlobalEnv [#pars.nativeAliases#] (List.map exposedValueToUsrAndInstance pars.exposedValues) __
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
    >> onOk fn { entryName, state = state0, defs }:

    typeResult as Res TA.RawType =
        try List.find (fn mod: mod.umr == pars.entryModule) modulesWithDestruction as
            , Nothing:
                [
                , "The entry module should be:"
                , ""
                , pars.umrToFsPath pars.entryModule
                , ""
                , "But I could not find any module matching that."
                ]
                >> Error.Raw
                >> Err

            , Just mod:
                getMain =
                    fn def:
                    Dict.get "main" (TA.patternNames def.pattern)

                try mod.valueDefs >> Dict.values >> List.filterMap getMain __ as
                    , [ { with type } ]:
                        !hash = Hash.fromList []

                        TA.normalizeType @hash type.raw
                        >> Ok

                    , _:
                        [
                        , "The entry module " .. pars.umrToFsPath pars.entryModule
                        , "does not seem to contain a `main` definition."
                        , ""
                        , "I need this to know where your program starts!"
                        ]
                        >> Error.Raw
                        >> Err

    typeResult
    >> onOk fn type:

    constructors =
        Dict.toList (Dict.map (fn k, v: v.type) typeCheckGlobalEnv.constructors)

    !externalValues =
        Array.fromList []

    # TODO Can I avoid these uniqueness shenanigans?
    !state = cloneImm state0
    List.each pars.exposedValues (fn (usr & exposed): Array.push @externalValues { name = Compiler/MakeEmittable.translateUsr @state usr, exposed })

    Ok { constructors, entryName, type, state, externalValues, defs }

