#
# Compile Modules
#
CompileModulesPars =
    {
    # TODO
    #, nativeAliases as Dict USR { value as Core.Type, args as [Text] }

    , entryModule as UMR
    # TODO: in theory, we should expose all union types referenced by these, and their constructors
    # In practice, I hope we'll get structural variant types before this becomes necessary
    , exposedValues as [ USR & Self.Self ]
    , meta as Meta
    , modules as [ UMR & Text ]
    , umrToFsPath as fn UMR: Text
    }


exposedValueToUsrAndInstance as fn USR & Self.Self: USR & Compiler/TypeCheck.Instance =
    fn usr & exposed:
    { with  raw } =
        exposed

    makeTyvar as fn TA.TyvarId, None: TA.Tyvar =
        fn tyvarId, 'none: { maybeAnnotated = 'nothing }

#        , generalizedAt = Pos.N
#        , generalizedFor = RefGlobal usr
#        , originalName = ""
#        , allowFunctions = True # TODO not (List.member tyvarId exposed.nonFn)
#        }

    freeTyvars as Dict TA.TyvarId TA.Tyvar =
        raw
        >> TA.typeTyvars
        >> Dict.map makeTyvar __

    usr
    & {
    , definedAt = Pos.'n
    , freeTyvars
    , freeUnivars = Dict.empty
    , type = { raw, uni = 'imm }
    }


findMainType as fn UMR, [ TA.Module ]: Res TA.RawType =
    fn umr, modules:
    try List.find (fn mod: mod.umr == umr) modules as

        'nothing:
            [
            , "The entry module should be:"
            , ""
            #TODO , pars.umrToFsPath pars.entryModule
            , ""
            , "But I could not find any module matching that."
            ]
            >> Error.'raw
            >> 'err

        'just mod:
            getMain =
                fn def:
                Dict.get "main" (TA.patternNames def.pattern)

            try mod.valueDefs >> Dict.values >> List.filterMap getMain __ as

                [ { with  type } ]:
                    !hash =
                        Hash.fromList []

                    TA.normalizeType @hash type.raw >> 'ok

                _:
                    [
                    #TODO , "The entry module " .. pars.umrToFsPath pars.entryModule
                    , "does not seem to contain a `main` definition."
                    , ""
                    , "I need this to know where your program starts!"
                    ]
                    >> Error.'raw
                    >> 'err


compileModules as fn CompileModulesPars: Res Self.LoadPars =
    fn pars:
    log "Loading modules..." ""

    loadModule as fn UMR & Text: Res CA.Module =
        fn umr & content:
        # TODO move textToCanonicalModule here
        Compiler/MakeCanonical.textToCanonicalModule
            'false
            {
            , errorModule =
                {
                , content
                , fsPath = pars.umrToFsPath umr
                }
            , meta = pars.meta
            , umr
            }

    pars.modules
    >> List.mapRes loadModule __
    >> onOk fn userModules:
    log "Type checking..." ""

    userModules
    >> Compiler/TypeCheck.initStateAndGlobalEnv
        ([#pars.nativeAliases#]
         List.map exposedValueToUsrAndInstance pars.exposedValues
        )
        __
    >> onOk fn luv & typeCheckGlobalEnv:
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
    >> onOk fn { defs, entryUsr }:
    findMainType pars.entryModule modulesWithDestruction
    >> onOk fn type:
    constructors as [ USR & TA.FullType ] =
        Dict.toList (Dict.map (fn k, v: v.type) typeCheckGlobalEnv.constructors)

    !externalValues as Array { self as Self.Self, usr as USR } =
        Array.fromList []

    List.each pars.exposedValues fn usr & self:
        Array.push @externalValues { self, usr }

    'ok { constructors, defs, entryUsr, externalValues, type }
