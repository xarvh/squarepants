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


getFreeTypeVars =
    Compiler/TypeCheck.getFreeTypeVars


# ============================================================================
# First Global Pass
#   * Expand and validate types
#   * Collect all exposed root symbols
#


insertUnionConstructors as CA.TypeDef CA.CanonicalType: CA.All (CA.Constructor CA.CanonicalType): CA.All (CA.Constructor CA.CanonicalType) =
    typeDef: constructors:
    try typeDef as
        CA.TypeDefAlias _:
            constructors

        CA.TypeDefUnion def:
            Meta.USR umr _ =
                def.usr

            Dict.for def.constructors (name: Dict.insert (Meta.USR umr name)) constructors


coreTypes as CA.All (CA.TypeDef CA.CanonicalType) =
    List.for CoreTypes.allDefs (def: Dict.insert def.usr << CA.TypeDefUnion def) Dict.empty


coreConstructors as CA.All (CA.Constructor CA.CanonicalType) =
    List.for CoreTypes.allDefs (u: insertUnionConstructors (CA.TypeDefUnion u)) Dict.empty


expandAndInsertModuleAnnotations as CA.All (CA.TypeDef CA.CanonicalType): (CA.Module CA.CanonicalType): ByUsr (CA.InstanceVariable CA.CanonicalType): Res (ByUsr (CA.InstanceVariable CA.CanonicalType)) =
    types: module:

    insertName as CA.ValueDef: Name: (Pos & Maybe CA.Type): Dict Meta.UniqueSymbolReference CA.InstanceVariable: Res (Dict Meta.UniqueSymbolReference CA.InstanceVariable) =
        def: name: (pos & maybeType): d:
        try maybeType as
            Nothing:
                Ok d

            Just rawType:

                Compiler/ExpandTypes.expandAnnotation types rawType >> onOk type:

                usr =
                    name >> Meta.USR module.umr

                iv as CA.InstanceVariable = {
                    , definedAt = pos
                    , ty = type
                    , freeTypeVariables = getFreeTypeVars Dict.empty type
                    , isMutable = False
                    }

                Ok << Dict.insert usr iv d


    insertValueDef =
        def:
        Dict.forRes (CA.patternNamedTypes def.pattern) (insertName def)

    Dict.forRes module.valueDefs (_: insertValueDef)


coreVariables as ByUsr (CA.InstanceVariable CA.CanonicalType) =

    insertUnop as Op.Unop: ByUsr CA.InstanceVariable: ByUsr CA.InstanceVariable =
        unop:

        usr =
            Meta.spCoreUSR unop.symbol

        iv as CA.InstanceVariable = {
            , definedAt = Pos.N
            , ty = unop.type
            , freeTypeVariables = Dict.empty
            , isMutable = False
            }

        Dict.insert usr iv

    insertBinop as Text: Op.Binop: ByUsr CA.InstanceVariable: ByUsr CA.InstanceVariable =
        symbol: binop:

        usr =
            Meta.spCoreUSR symbol

        iv as CA.InstanceVariable = {
            , definedAt = Pos.N
            , ty = binop.type
            , freeTypeVariables = getFreeTypeVars Dict.empty binop.type
            , isMutable = False
            }

        Dict.insert usr iv

    insertCoreFunction as Prelude.Function: ByUsr CA.InstanceVariable: ByUsr CA.InstanceVariable =
        coreFn:

        iv as CA.InstanceVariable = {
            , definedAt = Pos.N
            , ty = coreFn.type
            , freeTypeVariables = getFreeTypeVars Dict.empty coreFn.type
            , isMutable = False
            }

        Dict.insert coreFn.usr iv

    Dict.empty
        >> insertUnop Prelude.unaryPlus
        >> insertUnop Prelude.unaryMinus
        >> Dict.for Prelude.binopsBySymbol insertBinop
        >> List.for Prelude.functions insertCoreFunction


#
# Alias expansion and basic type validation
#
globalExpandedTypes as Dict Meta.UniqueModuleReference (CA.Module CA.CanonicalType): Res (CA.Globals CA.CanonicalType) =
    allModules:

    coreTypes
        # Collect types from all modules
        >> Dict.for allModules (_: Compiler/ExpandTypes.insertModuleTypes)

        # resolve aliases and apply them to unions
        >> Compiler/ExpandTypes.expandAllTypes
        >> onOk types:

            # populate constructors dict
            # (constructors in types are already expanded)
            constructors =
                Dict.for types (_: insertUnionConstructors) coreConstructors

            # populate root variable types
            Dict.forRes allModules (_: expandAndInsertModuleAnnotations types) coreVariables >> onOk instanceVariables:
            Ok { types, constructors, instanceVariables }

