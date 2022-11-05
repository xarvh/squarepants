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
#   * Expand and validate types
#   * Collect all exposed root symbols
#


constructorCaToTa as CA.Constructor: TA.Constructor =
  todo ""


# TODO break this function in two?
insertCaUnionType as CA.UnionDef: (ByUsr TA.TypeDef & ByUsr TA.Constructor): (ByUsr TA.TypeDef & ByUsr TA.Constructor) =
    caDef: (typesDict & consDict):

    taConstructors as ByUsr TA.Constructor =
        consDict
        >> Dict.for caDef.constructors name: caCons:
            Meta.USR umr _ =
                caCons.typeUsr

            {
            , pos = caCons.pos
            , typeUsr = todo "caCons.usr"
            , type = todo "caCons.type"
            }
            >> Dict.insert (Meta.USR umr name)

    taDef as TA.TypeDef =
        {
        , usr = caDef.usr
        , args = caDef.args
        #, constructors = taConstructors
        }
        >> TA.TypeDefUnion

    Dict.insert caDef.usr taDef typesDict & Dict.join taConstructors consDict



(coreTypes as ByUsr TA.TypeDef) & (coreConstructors as ByUsr TA.Constructor) =
    Dict.empty & Dict.empty
    >> List.for CoreTypes.allDefs insertCaUnionType




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


coreVariables as ByUsr TA.InstanceVariable =

    insertUnop as Op.Unop: ByUsr TA.InstanceVariable: ByUsr TA.InstanceVariable =
        unop:

        usr =
            Meta.spCoreUSR unop.symbol

        iv as TA.InstanceVariable = {
            , definedAt = Pos.N
            , type = todo "unop.type"
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
#        >> insertUnop Prelude.unaryPlus
#        >> insertUnop Prelude.unaryMinus
        >> Dict.for Prelude.binopsBySymbol insertBinop
        >> List.for Prelude.functions insertCoreFunction




#
# Was "Alias expansion and basic type validation"
# Not sure what's the point now, but the whole Pipeline module will be rewritten so whatever
#
#insertModuleTypes as CA.Module: ByUsr TA.TypeDef: ByUsr TA.TypeDef =
#    module: allTypes:
#    todo "!!!!!!!!"
#    allTypes
#        >> Dict.for module.aliasDefs (name: def: Dict.insert def.usr << CA.TypeDefAlias def)
#        >> Dict.for module.unionDefs (name: def: Dict.insert def.usr << CA.TypeDefUnion def)


globalExpandedTypes as Dict Meta.UniqueModuleReference CA.Module: Res TA.Globals =
    allModules:

    (types as ByUsr TA.TypeDef) & (constructors as ByUsr TA.Constructor) =
        coreTypes & coreConstructors
        >> Dict.for allModules usr: caModule: typesAndCons:
            typesAndCons
            >> Dict.for caModule.unionDefs (_: insertCaUnionType)
            #>> Dict.for module.aliasDefs (name: def: Dict.insert def.usr << CA.TypeDefAlias def)


    # populate constructors dict
    # (constructors in types are already expanded)
#    constructors as ByUsr TA.Constructor =
#        Dict.for types (_: insertUnionConstructors) coreConstructors

    # populate root variable types
    coreVariables
    >> Dict.forRes allModules (_: insertModuleAnnotations)
    >> onOk instanceVariables:

    Ok { types, constructors, instanceVariables }

