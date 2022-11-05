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


insertUnionConstructors as TA.TypeDef: CA.All TA.Constructor: CA.All TA.Constructor =
    typeDef: constructors:
    try typeDef as
        TA.TypeDefAlias _:
            constructors

#        CA.TypeDefUnion def:
#            Meta.USR umr _ =
#                def.usr
#
#            Dict.for def.constructors (name: Dict.insert (Meta.USR umr name)) constructors


coreTypes as CA.All TA.TypeDef =
    List.for CoreTypes.allDefs (def: todo "Dict.insert def.usr << CA.TypeDefUnion def") Dict.empty


coreConstructors as CA.All TA.Constructor =
    List.for CoreTypes.allDefs (u: todo "insertUnionConstructors (CA.TypeDefUnion u)") Dict.empty


# TODO we are not expanding the types any more
insertModuleAnnotations as CA.Module: ByUsr TA.InstanceVariable: Res (ByUsr TA.InstanceVariable) =
    caModule:

    insertName as CA.ValueDef: Name: { isUnique as Bool, maybeAnnotation as Maybe CA.Type, pos as Pos }: CA.All TA.InstanceVariable: Res (CA.All TA.InstanceVariable) =
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
insertModuleTypes as CA.Module: CA.All TA.TypeDef: CA.All TA.TypeDef =
    module: allTypes:
    todo "!!!!!!!!"
#    allTypes
#        >> Dict.for module.aliasDefs (name: def: Dict.insert def.usr << CA.TypeDefAlias def)
#        >> Dict.for module.unionDefs (name: def: Dict.insert def.usr << CA.TypeDefUnion def)


globalExpandedTypes as Dict Meta.UniqueModuleReference CA.Module: Res TA.Globals =
    allModules:

    types as CA.All TA.TypeDef =
        coreTypes
        # Collect types from all modules
        >> Dict.for allModules (_: insertModuleTypes)

    # populate constructors dict
    # (constructors in types are already expanded)
    constructors as CA.All TA.Constructor =
        Dict.for types (_: insertUnionConstructors) coreConstructors

    # populate root variable types
    coreVariables
    >> Dict.forRes allModules (_: insertModuleAnnotations)
    >> onOk instanceVariables:

    Ok { types, constructors, instanceVariables }

