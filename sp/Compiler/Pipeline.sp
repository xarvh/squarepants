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


#
# First module-by-module pass: load each module
#

alias ModuleParams = {
    , meta as Meta
    , stripLocations as Bool
    , source as Meta.Source
    , name as Name
    , code as Text
    }

textToFormattableModule pars =
    as ModuleParams: Res [FA.Statement]

    tokensResult =
        as Res [Token]
        Compiler/Lexer.lexer pars.name pars.code

    tokensToStatsResult tokens =
        as [Token]: Res [FA.Statement]
        Compiler/Parser.parse pars.stripLocations pars.name tokens

    onOk tokensToStatsResult tokensResult


# STEP 1
textToCanonicalModule pars =
    as ModuleParams: Res CA.Module

    ro =
        as Compiler/MakeCanonical.ReadOnly
        {
        , currentModule = Meta.UMR pars.source pars.name
        , meta = pars.meta
        }

    pars
        >> textToFormattableModule
        >> onOk (Compiler/MakeCanonical.translateModule ro pars.name)


#
# TODO: this will completely change once we actually build only the dependency tree
#
#loadAllModules meta contentByModuleName =
#    as Meta: Dict Text Text: Res (Dict Meta.UniqueModuleReference CA.Module)
#
#    load moduleName moduleContent acc =
#
#    , meta as Meta
#    , stripLocations as Bool
#    , source as Meta.Source
#    , name as Name
#    , code as Text
#
#    Dict.foldlRes load contentByModuleName Dict.empty
#


# ============================================================================
# First Global Pass
#   * Expand and validate types
#   * Collect all exposed root symbols
#


insertUnionConstructors typeDef constructors =
    as CA.TypeDef: CA.All CA.Constructor:  CA.All CA.Constructor
    try typeDef as
        CA.TypeDefAlias _:
            constructors

        CA.TypeDefUnion def:
            Meta.USR umr _ =
                def.usr

            Dict.foldl (fn name: Dict.insert (Meta.USR umr name)) def.constructors constructors


coreTypes =
    as CA.All CA.TypeDef
    List.foldl (fn def: Dict.insert def.usr << CA.TypeDefUnion def) CoreTypes.allDefs Dict.empty


coreConstructors =
    as CA.All CA.Constructor
    List.foldl (fn u: insertUnionConstructors (CA.TypeDefUnion u)) CoreTypes.allDefs Dict.empty


expandAndInsertModuleAnnotations types module =
    as CA.All CA.TypeDef: CA.Module: CA.InstanceVariablesByRef: Res CA.InstanceVariablesByRef

    insertName def name (pos & maybeType) d =
        try maybeType as
            Nothing:
                Ok d

            Just rawType:

                Compiler/ExpandTypes.expandAnnotation types rawType >> onOk fn type:

                ref =
                    as CA.Ref
                    name
                        >> Meta.USR module.umr
                        >> CA.RefRoot

                iv =
                    as CA.InstanceVariable
                    {
                    , definedAt = pos
                    , ty = type
                    , freeTypeVariables = CA.getFreeTypeVars Dict.empty def.nonFn type
                    , isMutable = False
                    }

                Ok << Dict.insert ref iv d


    insertValueDef def =
        Dict.foldlRes (insertName def) (CA.patternNamedTypes def.pattern)

    Dict.foldlRes (fn _: insertValueDef) module.valueDefs


coreVariables =
    as CA.InstanceVariablesByRef

    insertBinop symbol binop =
        as Text: Op.Binop: CA.InstanceVariablesByRef: CA.InstanceVariablesByRef

        ref =
            as CA.Ref
            CA.RefRoot << Meta.spCoreUSR symbol

        iv =
            as CA.InstanceVariable
            {
            , definedAt = Pos.N
            , ty = binop.type
            , freeTypeVariables = CA.getFreeTypeVars Dict.empty (Set.fromList binop.nonFn) binop.type
            , isMutable = False
            }

        Dict.insert ref iv

    Dict.empty
        # TODO insert unops
        >> Dict.foldl insertBinop Prelude.binops


#
# Alias expansion and basic type validation
#
globalExpandedTypes allModules =
    as Dict Meta.UniqueModuleReference CA.Module: Res { types as CA.All CA.TypeDef, constructors as CA.All CA.Constructor, instanceVariables as CA.InstanceVariablesByRef }

    coreTypes
        # Collect types from all modules
        >> Dict.foldl (fn _: Compiler/ExpandTypes.insertModuleTypes) allModules

        # resolve aliases and apply them to unions
        >> Compiler/ExpandTypes.expandAllTypes
        >> onOk fn types:

        # populate constructors dict
        # (constructors in types are already expanded)
        constructors =
            Dict.foldl (fn _: insertUnionConstructors) types coreConstructors

        # populate root variable types
        Dict.foldlRes (fn _: expandAndInsertModuleAnnotations types) allModules coreVariables >> onOk fn instanceVariables:
        Ok { types, constructors, instanceVariables }

