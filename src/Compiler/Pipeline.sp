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


insertUnionConstructors as CA.TypeDef: CA.All CA.Constructor: CA.All CA.Constructor =
    typeDef: constructors:
    try typeDef as
        CA.TypeDefAlias _:
            constructors

        CA.TypeDefUnion def:
            Meta.USR umr _ =
                def.usr

            Dict.for def.constructors (name: Dict.insert (Meta.USR umr name)) constructors


coreTypes as CA.All CA.TypeDef =
    List.for CoreTypes.allDefs (def: Dict.insert def.usr << CA.TypeDefUnion def) Dict.empty


coreConstructors as CA.All CA.Constructor =
    List.for CoreTypes.allDefs (u: insertUnionConstructors (CA.TypeDefUnion u)) Dict.empty


# TODO we are not expanding the types any more
expandAndInsertModuleAnnotations as CA.All CA.TypeDef: CA.Module: ByUsr CA.InstanceVariable: Res (ByUsr CA.InstanceVariable) =
    types: module:

    insertName = #as CA.ValueDef: Name: (Pos & Maybe CA.Type): Dict Meta.UniqueSymbolReference CA.InstanceVariable: Res (Dict Meta.UniqueSymbolReference CA.InstanceVariable) =
        def: name: stuff: d:
        { pos, isUnique, maybeAnnotation } = stuff
        try maybeAnnotation as
            Nothing:
                Ok d

            Just type:

                usr =
                    Meta.USR module.umr name

                iv as CA.InstanceVariable = {
                    , definedAt = pos
                    , type = type
                    , isUnique
                    }

                Ok << Dict.insert usr iv d

    Dict.forRes module.valueDefs _: def:
        Dict.forRes (CA.patternNames def.pattern) (insertName def)


coreVariables as ByUsr CA.InstanceVariable =

    insertUnop as Op.Unop: ByUsr CA.InstanceVariable: ByUsr CA.InstanceVariable =
        unop:

        usr =
            Meta.spCoreUSR unop.symbol

        iv as CA.InstanceVariable = {
            , definedAt = Pos.N
            , type = unop.type
            , isUnique = False
            }

        Dict.insert usr iv

    insertBinop as Text: Op.Binop: ByUsr CA.InstanceVariable: ByUsr CA.InstanceVariable =
        symbol: binop:

        usr =
            Meta.spCoreUSR symbol

        iv as CA.InstanceVariable = {
            , definedAt = Pos.N
            , type = binop.type
            , isUnique = False
            }

        Dict.insert usr iv

    insertCoreFunction as Prelude.Function: ByUsr CA.InstanceVariable: ByUsr CA.InstanceVariable =
        coreFn:

        iv as CA.InstanceVariable = {
            , definedAt = Pos.N
            , type = coreFn.type
            , isUnique = False
            }

        Dict.insert coreFn.usr iv

    Dict.empty
        >> insertUnop Prelude.unaryPlus
        >> insertUnop Prelude.unaryMinus
        >> Dict.for Prelude.binopsBySymbol insertBinop
        >> List.for Prelude.functions insertCoreFunction




#
# Was "Alias expansion and basic type validation"
# Not sure what's the point now, but the whole Pipeline module will be rewritten so whatever
#
insertModuleTypes as CA.Module: CA.All CA.TypeDef: CA.All CA.TypeDef =
    module: allTypes:
    allTypes
        >> Dict.for module.aliasDefs (name: def: Dict.insert def.usr << CA.TypeDefAlias def)
        >> Dict.for module.unionDefs (name: def: Dict.insert def.usr << CA.TypeDefUnion def)


globalExpandedTypes as Dict Meta.UniqueModuleReference CA.Module: Res CA.Globals =
    allModules:

    types as CA.All CA.TypeDef =
        coreTypes
        # Collect types from all modules
        >> Dict.for allModules (_: insertModuleTypes)

    # populate constructors dict
    # (constructors in types are already expanded)
    constructors =
        Dict.for types (_: insertUnionConstructors) coreConstructors

    # populate root variable types
    coreVariables
    >> Dict.forRes allModules (_: [# we're not really expanding anything #] expandAndInsertModuleAnnotations types)
    >> onOk instanceVariables:

    Ok { types, constructors, instanceVariables }

