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


insertUnionConstructors as CA.TypeDef: CA.All CA.Constructor:  CA.All CA.Constructor =
    typeDef: constructors:
    try typeDef as
        CA.TypeDefAlias _:
            constructors

        CA.TypeDefUnion def:
            Meta.USR umr _ =
                def.usr

            Dict.foldl (name: Dict.insert (Meta.USR umr name)) def.constructors constructors


coreTypes as CA.All CA.TypeDef =
    List.foldl (def: Dict.insert def.usr << CA.TypeDefUnion def) CoreTypes.allDefs Dict.empty


coreConstructors as CA.All CA.Constructor =
    List.foldl (u: insertUnionConstructors (CA.TypeDefUnion u)) CoreTypes.allDefs Dict.empty


expandAndInsertModuleAnnotations as CA.All CA.TypeDef: CA.Module: CA.InstanceVariablesByRef: Res CA.InstanceVariablesByRef =
    types: module:

    insertName = def: name: (pos & maybeType): d:
        try maybeType as
            Nothing:
                Ok d

            Just rawType:

                Compiler/ExpandTypes.expandAnnotation types rawType >> onOk type:

                ref as CA.Ref =
                    name
                        >> Meta.USR module.umr
                        >> CA.RefRoot

                iv as CA.InstanceVariable = {
                    , definedAt = pos
                    , ty = type
                    , freeTypeVariables = CA.getFreeTypeVars Dict.empty def.nonFn type
                    , isMutable = False
                    }

                Ok << Dict.insert ref iv d


    insertValueDef = def:
        Dict.foldlRes (insertName def) (CA.patternNamedTypes def.pattern)

    Dict.foldlRes (_: insertValueDef) module.valueDefs


coreVariables as CA.InstanceVariablesByRef =

    insertUnop as Op.Unop: CA.InstanceVariablesByRef: CA.InstanceVariablesByRef =
        unop:

        ref as CA.Ref =
            CA.RefRoot << Meta.spCoreUSR unop.symbol

        iv as CA.InstanceVariable = {
            , definedAt = Pos.N
            , ty = unop.type
            , freeTypeVariables = Dict.empty
            , isMutable = False
            }

        Dict.insert ref iv

    insertBinop as Text: Op.Binop: CA.InstanceVariablesByRef: CA.InstanceVariablesByRef =
        symbol: binop:

        ref as CA.Ref =
            CA.RefRoot << Meta.spCoreUSR symbol

        iv as CA.InstanceVariable = {
            , definedAt = Pos.N
            , ty = binop.type
            , freeTypeVariables = CA.getFreeTypeVars Dict.empty (Set.fromList binop.nonFn) binop.type
            , isMutable = False
            }

        Dict.insert ref iv

    insertCoreFunction as Prelude.Function: CA.InstanceVariablesByRef: CA.InstanceVariablesByRef =
        coreFn:

        ref as CA.Ref =
            CA.RefRoot << coreFn.usr

        iv as CA.InstanceVariable = {
            , definedAt = Pos.N
            , ty = coreFn.type
            , freeTypeVariables = CA.getFreeTypeVars Dict.empty (Set.fromList coreFn.nonFn) coreFn.type
            , isMutable = False
            }

        Dict.insert ref iv

    Dict.empty
        >> insertUnop Prelude.unaryPlus
        >> insertUnop Prelude.unaryMinus
        >> Dict.foldl insertBinop Prelude.binops
        >> List.foldl insertCoreFunction Prelude.functions


#
# Alias expansion and basic type validation
#
globalExpandedTypes as Dict Meta.UniqueModuleReference CA.Module: Res CA.Globals =
    allModules:

    coreTypes
        # Collect types from all modules
        >> Dict.foldl (_: Compiler/ExpandTypes.insertModuleTypes) allModules

        # resolve aliases and apply them to unions
        >> Compiler/ExpandTypes.expandAllTypes
        >> onOk types:

            # populate constructors dict
            # (constructors in types are already expanded)
            constructors =
                Dict.foldl (_: insertUnionConstructors) types coreConstructors

            # populate root variable types
            Dict.foldlRes (_: expandAndInsertModuleAnnotations types) allModules coreVariables >> onOk instanceVariables:
            Ok { types, constructors, instanceVariables }

