var Def =
    , 'valueDef CA.ValueDef
    , 'constructorDef CA.ConstructorDef
    , 'variantTypeDef CA.VariantTypeDef
    , 'aliasDef CA.AliasDef
    , 'missingDef


CollectDependenciesState =
    {
    , done as Hash USR { def as Def, deps as Dict USR DependencyType }
    , loadedModulesByUmr as Hash UMR CA.Module
    , pending as Hash USR DependencyType
    }


initCollectDependenciesState as fn [ USR & DependencyType ]: !CollectDependenciesState =
    fn pendingList:
    !pending =
        Hash.fromList pendingList

    Hash.insert @pending CoreDefs.noneTypeUsr 'typeDependency

    Hash.insert @pending CoreDefs.noneConsUsr 'constructorDependency

    Hash.insert @pending CoreDefs.boolUsr 'typeDependency

    Hash.insert @pending CoreDefs.trueUsr 'constructorDependency

    Hash.insert @pending CoreDefs.falseUsr 'constructorDependency

    Hash.insert @pending CoreDefs.numberUsr 'typeDependency

    Hash.insert @pending CoreDefs.textUsr 'typeDependency

    Hash.insert @pending CoreDefs.listUsr 'typeDependency

    {
    , done = Hash.fromList []
    , loadedModulesByUmr = Hash.fromList [ CoreDefs.umr & CoreDefs.coreModule ]
    , pending
    }


collectUsrDependencies as fn BuildPlan, @CollectDependenciesState, USR, DependencyType: Res None =
    fn env, @state, usr, depType:
#
    'USR umr name =
        usr

    try Hash.get @state.loadedModulesByUmr umr as

        'just caModule:
            'ok caModule

        'nothing:
            env.loadCaModule usr
            >> onOk fn caModule:
            Hash.insert @state.loadedModulesByUmr umr caModule

            'ok caModule
    >> onOk fn caModule:
    (def as Def) & (deps as Dict USR DependencyType) =
        try depType as

            'typeDependency:
                try Dict.get name caModule.aliasDefs as

                    'just d:
                        'aliasDef d & d.directDeps

                    'nothing:
                        try Dict.get name caModule.variantTypeDefs as

                            'just d:
                                # var types don't have dependencies, their costructors do
                                'variantTypeDef d & Dict.empty

                            'nothing:
                                'missingDef & Dict.empty

            'constructorDependency:
                try Dict.get name caModule.constructorDefs as
                    'just d: 'constructorDef d & d.directDeps
                    'nothing: 'missingDef & Dict.empty

            'valueDependency:
                try Dict.get name caModule.valueDefs as
                    'just d: 'valueDef d & d.directDeps
                    'nothing: 'missingDef & Dict.empty

    Hash.insert @state.done usr { def, deps }

    Dict.each deps fn u, c:
        if Hash.get @state.done u == 'nothing then
            Hash.insert @state.pending u c
        else
            'none

    'ok 'none


collectRequiredUsrs as fn BuildPlan, @CollectDependenciesState: Res None =
    fn env, @state:
    try Hash.pop @state.pending as

        'nothing:
            # All done!
            'ok 'none

        'just (usr & depType):
            collectUsrDependencies env @state usr depType
            >> onOk fn 'none:
#
            collectRequiredUsrs env @state


expandAndInsertType as fn @CollectDependenciesState, @Array Error, USR, Compiler/TypeCheck.Env: Compiler/TypeCheck.Env =
    fn @state, @errors, usr, env0:
    try Hash.get @state.done usr as

        'nothing:
            todo ("compiler bug, missing type usr" .. toHuman usr)

        'just { def, deps }:
            try def as

                'variantTypeDef variantTypeDef:
                    { env0 with exactTypes = Dict.insert usr variantTypeDef.pars .exactTypes }

                'aliasDef aliasDef:
                    { env0 with
                    , expandedAliases = Compiler/TypeCheck.expandAndInsertAlias @errors { env0 with currentRootUsr = usr } aliasDef .expandedAliases
                    }

                _:
                    env0


typecheckDefinition as fn @CollectDependenciesState, @Array Error, @Int, USR, Compiler/TypeCheck.Env: Compiler/TypeCheck.Env =
    fn @state, @errors, @lastUnificationVarId, usr, env0:
#    log "typechecking:" (Human/Type.usrToText env0 usr)

    try Hash.get @state.done usr as

        'nothing:
            todo ("compiler bug, missing value usr" .. toHuman usr)

        'just { def, deps }:
            try def as
                'valueDef valueDef: Compiler/TypeCheck.doRootDefinition @lastUnificationVarId @errors usr env0 valueDef
                'constructorDef constructorDef: Compiler/TypeCheck.addConstructorToGlobalEnv @errors constructorDef.name constructorDef env0
                _: env0


evaluateCircularValues as fn @CollectDependenciesState, @Array Error, @Int, [ USR ], Compiler/TypeCheck.Env: Compiler/TypeCheck.Env =
    fn @state, @errors, @lastUnificationVarId, circular, env0:
        # TODO do all aliases first, then all values, otherwise Compiler/TypeCheck.translateRaw will go infinite

        try circular as

            [ u, others... ]:
                if usrToDependencyType u == 'typeDependency then
                    env0
                else
                    # Circular values
                    List.for env0 circular fn usr, envX:
                        try Hash.get @state.done usr as

                            'just { with  def = 'valueDef def }:
                                try def.maybeAnnotation as

                                    'just ann:
                                        # TODO we should do the "initializable" check here rather than when emitting
                                        Compiler/TypeCheck.addRootRecursiveInstance @errors def.namePos usr ann envX

                                    'nothing:
                                        # TODO use Error.'simple
                                        if others == [] then
                                            [ "Value " .. def.name .. "  is recursive, so I need a type annotation for it." ]
                                        else
                                            # TODO print `circular` better
                                            [ "Value " .. def.name .. " is mutually recursive with " .. toHuman circular .. " so I need it to be annotated." ]
                                        >> Error.'raw
                                        >> Array.push @errors __

                                        envX

                            _:
                                # should not happen
                                envX

            _:
                env0


usrToDependencyType as fn USR: DependencyType =
    fn 'USR umr name:
    if Compiler/Lexer.startsWithUpperChar name then
        'typeDependency
    else if Text.startsWith "'" name then
        'constructorDependency
    else
        'valueDependency


BuildPlan =
    {
    , loadCaModule as fn USR: Res CA.Module
    , projectImports as Imports
    , requiredUsrs as [ USR ]
    }


stopOnError as fn BuildPlan, @Array Error: Res None =
    fn pars, @errors:
    try Array.toList @errors as

        []:
            'ok 'none

        errorsAsList:
            errorsAsList
            >> Error.'nested
            >> 'err


BuildOut =
    {
    , constructors as [ USR & TA.RawType ]
    , natives as [ USR ]
    , rootValues as [ EA.GlobalDefinition ]
    }


build as fn BuildPlan: Res BuildOut =
    fn pars:
    !state as CollectDependenciesState =
        pars.requiredUsrs
        >> List.map (fn usr: usr & usrToDependencyType usr) __
        >> initCollectDependenciesState

    collectRequiredUsrs pars @state
    >> onOk fn 'none:
    #
    # Reorder all usrs
    #
    nodeToEdges as fn USR: Dict USR DependencyType =
        fn usr:
        try Hash.get @state.done usr as
            'nothing: Dict.empty
            'just { with  deps }: deps

    nodesById as Dict USR USR =
        Hash.for_ Dict.empty @state.done (fn usr, _, dict: Dict.insert usr usr dict)

    circulars & orderedUsrs =
        RefHierarchy.reorder nodeToEdges nodesById

#    List.each orderedUsrs fn usr:
#        log "*" (Human/Type.usrToText pars.projectImports usr)
#        'none

#    log "CIRC" ""
#    List.each circulars fn circ:
#        List.map (usrToText env_ __) circ
#        >> Text.join ", " __
#        >> log "C" __

    !errors =
        Array.fromList []

    #
    # Error on circular aliases
    #
    List.each circulars fn circular:
        try circular as

            [ usr, others... ]:
                if usrToDependencyType usr == 'typeDependency then
                    # Circular aliases, always a fatal error
                    [ "Circular aliases!", Debug.toHuman circular ]
                    >> Error.'raw
                    >> Array.push @errors __
                else
                    'none

            _:
                'none

    stopOnError pars @errors
    >> onOk fn 'none:
    #
    # init global env
    #
    modulesByUmr as Dict UMR CA.Module =
        @state.loadedModulesByUmr
        >> Hash.toList
        >> Dict.fromList

    #
    # Init
    #
    env0 =
        Compiler/TypeCheck.initEnv pars.projectImports modulesByUmr

    #
    # Insert types
    #
    # We need these before we can deal with circular values
    #
    env1 =
        List.for env0 orderedUsrs (expandAndInsertType @state @errors __ __)

    stopOnError pars @errors
    >> onOk fn 'none:
    #
    # Typecheck and build global env
    #
    !lastUnificationVarId =
        cloneImm 0

    env2 =
        List.for env1 circulars (evaluateCircularValues @state @errors @lastUnificationVarId __ __)

    stopOnError pars @errors
    >> onOk fn 'none:
    envF =
        List.for env2 orderedUsrs (typecheckDefinition @state @errors @lastUnificationVarId __ __)

    #
    # Uniqueness check
    #
    valueDefsWithDestruction as [ USR & TA.RootDef ] =
        envF.reversedRootValueDefs
        >> List.reverse
        >> List.map (Compiler/UniquenessCheck.updateValueDef @errors modulesByUmr __) __

    stopOnError pars @errors
    >> onOk fn 'none:
    #
    # Ensure that entryUsr and platform usrs are available?
    #
    missingDefs =
        Hash.for_ [] @state.done fn usr, { def, deps }, errs:
            if def == 'missingDef then [ usr, errs... ] else errs

    if missingDefs /= [] then
        [
        , "Cannot find definitions for:"
        , List.map (Human/Type.usrToText CoreDefs.coreModule __) missingDefs...
        ]
        >> Error.'raw
        >> 'err
    else
        'ok 'none
    >> onOk fn 'none:
    #
    # Emit
    #

    translateDef as fn USR & TA.RootDef: Maybe EA.GlobalDefinition =
        fn usr & def:
        Maybe.map
            (fn body:
                 {
                 , deps = def.directDeps
                 , expr = Compiler/MakeEmittable.translateExpression (Compiler/MakeEmittable.mkEnv usr modulesByUmr) body
                 , freeTyvars = def.freeTyvars
                 , freeUnivars = def.freeUnivars
                 , type = def.type.raw
                 , usr = EA.translateUsr usr
                 }
            )
            def.body

    rootValues as [ EA.GlobalDefinition ] =
        List.filterMap translateDef valueDefsWithDestruction

    natives as [ USR ] =
        valueDefsWithDestruction
        >> List.filter (fn usr & def: def.body == 'nothing) __
        >> List.map Tuple.first __

    #
    # Constructors
    #
    constructors as [ USR & TA.RawType ] =
        Dict.toList (Dict.map (fn k, v: v.type.raw) envF.constructors)

    #
    # Done!
    #
    'ok { constructors, natives, rootValues }
