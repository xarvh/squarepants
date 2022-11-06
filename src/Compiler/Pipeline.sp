[#

    * load modules.sp

    * parallel, on each module:
        lexer
        parser
        make canonical

    * collect types from all modules
    * resolve all aliases
    * resolve all unions

    * Build type check init env

    * parallel, on each module:
        type check

#]



# ============================================================================
# First Global Pass
#   * Collect all exposed root symbols
#






# TODO break this function in two?
#insertCaUnionType as CA.UnionDef: (ByUsr TA.TypeDef & ByUsr TA.Constructor & Int): (ByUsr TA.TypeDef & ByUsr TA.Constructor & Int) =
#    caDef: (typesDict & consDict & tyvarCounter0):
#
#    nameToId as Dict Name TA.UnificationVariableId =
#        caDef.args
#        >> List.indexedMap (index: arg: (arg & tyvarCounter0 + index))
#        >> Dict.fromList
#
#    nameToTyvarId as Name: TA.UnificationVariableId =
#        name:
#        try Dict.get name nameToId as
#            Just id: id
#            Nothing: todo "insertCaUnionType TSNH"
#
#    taConstructors as ByUsr TA.Constructor =
#        consDict
#        >> Dict.for caDef.constructors name: caCons:
#            Meta.USR umr _ =
#                caDef.usr
#
#            {
#            , pos = caCons.pos
#            , typeUsr = caDef.usr
#            , type = Compiler/TypeCheck.typeCa2Ta_ nameToTyvarId caCons.type
#            }
#            >> Dict.insert (Meta.USR umr name)
#
#    taDef as TA.TypeDef =
#        {
#        , usr = caDef.usr
#        , args = caDef.args
#        #, constructors = taConstructors
#        }
#        >> TA.TypeDefUnion
#
#    Dict.insert caDef.usr taDef typesDict & Dict.join taConstructors consDict & tyvarCounter0 + List.length caDef.args
#
#
#(coreTypes as ByUsr TA.TypeDef) & (coreConstructors as ByUsr TA.Constructor) & (initialTyvarIdCounter as Int) =
#    Dict.empty & Dict.empty & 0
#    >> List.for CoreTypes.allDefs insertCaUnionType




# TODO we are not expanding the types any more
insertModuleAnnotations as CA.Module: ByUsr TA.InstanceVariable: Res (ByUsr TA.InstanceVariable) =
    caModule:

    insertName as CA.ValueDef: Name: { isUnique as Bool, maybeAnnotation as Maybe CA.Type, pos as Pos }: ByUsr TA.InstanceVariable: Res (ByUsr TA.InstanceVariable) =
        def: name: stuff: d:
        { pos, isUnique, maybeAnnotation } = stuff
        try maybeAnnotation as
            Nothing:
                Ok d

            Just type:

                usr =
                    Meta.USR caModule.umr name

                iv as TA.InstanceVariable = {
                    , definedAt = pos
                    , type = todo "type"
                    , tyvars = Dict.empty
                    , isUnique
                    }

                Ok << Dict.insert usr iv d

    Dict.forRes caModule.valueDefs _: caDef:
        Dict.forRes (CA.patternNames caDef.pattern) (insertName caDef)


coreVariables as Int: ByUsr TA.InstanceVariable =
    tyvarIdCounter:

    insertUnop as Op.Unop: ByUsr TA.InstanceVariable: ByUsr TA.InstanceVariable =
        unop:

        usr =
            Meta.spCoreUSR unop.symbol

        iv as TA.InstanceVariable = {
            , definedAt = Pos.N
            , type = unop.type
            , isUnique = False
            , tyvars = Dict.empty
            }

        Dict.insert usr iv

    insertBinop as Text: Op.Binop: ByUsr TA.InstanceVariable: ByUsr TA.InstanceVariable =
        symbol: binop:

        usr =
            Meta.spCoreUSR symbol

        iv as TA.InstanceVariable = {
            , definedAt = Pos.N
            , type = todo "binop.type"
            , isUnique = False
            , tyvars = Dict.empty
            }

        Dict.insert usr iv

    insertCoreFunction as Prelude.Function: ByUsr TA.InstanceVariable: ByUsr TA.InstanceVariable =
        coreFn:

        iv as TA.InstanceVariable = {
            , definedAt = Pos.N
            , type = todo "coreFn.type"
            , isUnique = False
            , tyvars = Dict.empty
            }

        Dict.insert coreFn.usr iv

    Dict.empty
        >> insertUnop Prelude.unaryPlus
        >> insertUnop Prelude.unaryMinus
        >> Dict.for Prelude.binopsBySymbol insertBinop
        >> List.for Prelude.functions insertCoreFunction


globalExpandedTypes as Dict Meta.UniqueModuleReference CA.Module: Res TA.Globals =
    allModules:

    state @= Compiler/TypeCheck.initState

    coreEnv
    >> Dict.for allModules umr: caModule: env:
        env
        >> Dict.for caModule.unionDefs (Compiler/TypeCheck.addUnionTypeAndConstructorsToGlobalEnv @state)
        >> Dict.for caModule.aliasDefs (Compiler/TypeCheck.addAliasToGlobalEnv @state)
    >> Ok



#  Compiler/TypeCheck.initGlobalEnv
#
#
#    (types as ByUsr TA.TypeDef) & (constructors as ByUsr TA.Constructor) & (tyvarIdCounter as Int) =
#        coreTypes & coreConstructors & initialTyvarIdCounter
#            typesAndConsAndCounter
#            >> Dict.for caModule.unionDefs (_: insertCaUnionType)
#            >> Dict.for caModule.aliasDefs (_: insertCaAliasType)
#
#    # populate root variable types
#    coreVariables tyvarIdCounter
#    >> Dict.forRes allModules (_: insertModuleAnnotations)
#    >> onOk instanceVariables:
#
#    Ok env

