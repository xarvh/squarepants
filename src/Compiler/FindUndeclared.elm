module Compiler.FindUndeclared exposing (..)

import Compiler.FormattableToCanonicalAst
import Dict exposing (Dict)
import Set exposing (Set)
import Types.CanonicalAst as CA


type alias Path =
    String


type alias Location =
    Int


type alias EnvDeclared =
    { types : Set String
    , values : Set String
    }


type alias Undeclared =
    Dict Path (List Location)


type alias EnvUndeclared =
    { types : Undeclared
    , values : Undeclared
    }



----
--- Module
--


{-| On error, produce a dict of undeclared type variables
--| On ok, produce the stuff that the system should try to load form other modules
-}
moduleUndeclared : CA.Module e -> Result Undeclared EnvUndeclared
moduleUndeclared mod =
    let
        declaredTypes =
            Set.fromList <| Dict.keys mod.aliases ++ Dict.keys mod.unions

        undeclaredTypes =
            Dict.empty
                |> (\a -> Dict.foldl (\k -> addAliasUndeclared declaredTypes) a mod.aliases)
                |> (\a -> Dict.foldl (\k -> addUnionUndeclared declaredTypes) a mod.unions)

        declared : EnvDeclared
        declared =
            { types = declaredTypes
            , values = Set.fromList <| Dict.keys mod.values
            }

        undeclaredInit : EnvUndeclared
        undeclaredInit =
            { types = undeclaredTypes
            , values = Dict.empty
            }

        undeclaredFinal =
            Dict.foldl (\k -> addValueUndeclared declared) undeclaredInit mod.values

        undeclaredTypeVariables =
            undeclaredFinal.types
                |> Dict.filter (\k v -> not <| Compiler.FormattableToCanonicalAst.firstCharIsUpper k)
    in
    if undeclaredTypeVariables /= Dict.empty then
        Err undeclaredTypeVariables

    else
        Ok undeclaredFinal



----
--- Types
--


addAliasUndeclared : Set String -> CA.AliasDef -> Undeclared -> Undeclared
addAliasUndeclared declaredTypes al undeclaredTypes =
    let
        typesEnv =
            List.foldl Set.insert declaredTypes al.args
    in
    addTypeUndeclared False typesEnv al.ty undeclaredTypes


addUnionUndeclared : Set String -> CA.UnionDef -> Undeclared -> Undeclared
addUnionUndeclared declaredTypes union undeclaredTypes =
    let
        typesEnv =
            List.foldl Set.insert declaredTypes union.args

        addConstructor : CA.UnionConstructor -> Undeclared -> Undeclared
        addConstructor cons un =
            List.foldl (addTypeUndeclared False typesEnv) un cons.args
    in
    List.foldl addConstructor undeclaredTypes union.constructors


addTypeUndeclared : Bool -> Set String -> CA.Type -> Undeclared -> Undeclared
addTypeUndeclared isAnnotation typesEnv ty undeclaredTypes =
    case ty of
        CA.TypeConstant { path, args } ->
            undeclaredTypes
                |> (\un -> List.foldl (addTypeUndeclared isAnnotation typesEnv) un args)
                |> maybeAdd typesEnv path

        CA.TypeVariable { name } ->
            if isAnnotation then
                undeclaredTypes

            else
                maybeAdd typesEnv name undeclaredTypes

        CA.TypeFunction { from, fromIsMutable, to } ->
            undeclaredTypes
                |> addTypeUndeclared isAnnotation typesEnv from
                |> addTypeUndeclared isAnnotation typesEnv to

        CA.TypeRecord { extensible, attrs } ->
            undeclaredTypes
                |> (\un -> Dict.foldl (\k -> addTypeUndeclared isAnnotation typesEnv) un attrs)
                |> maybeMaybeAdd typesEnv extensible

        CA.TypeAlias path t ->
            addTypeUndeclared isAnnotation typesEnv t undeclaredTypes



----
--- Values
--


addValueUndeclared : EnvDeclared -> CA.ValueDef e -> EnvUndeclared -> EnvUndeclared
addValueUndeclared env def undeclared =
    let
        undeclaredTypes =
            case def.maybeAnnotation of
                Nothing ->
                    undeclared.types

                Just ty ->
                    addTypeUndeclared True env.types ty undeclared.types
    in
    addStatementBlockUndeclared env def.body { undeclared | types = undeclaredTypes }


statementAsDefinition : CA.Statement e -> Maybe (CA.ValueDef e)
statementAsDefinition s =
    case s of
        CA.Definition d ->
            Just d

        _ ->
            Nothing


addStatementBlockUndeclared : EnvDeclared -> List (CA.Statement e) -> EnvUndeclared -> EnvUndeclared
addStatementBlockUndeclared env block undeclared =
    let
        localEnv =
            block
                |> List.filterMap statementAsDefinition
                |> List.foldl (\def envAcc -> { envAcc | values = Set.union (CA.patternNames def.pattern) envAcc.values }) env
    in
    List.foldl (addStatementUndeclared localEnv) undeclared block


addStatementUndeclared : EnvDeclared -> CA.Statement e -> EnvUndeclared -> EnvUndeclared
addStatementUndeclared env s undeclared =
    case s of
        CA.Definition def ->
            addValueUndeclared env def undeclared

        CA.Evaluation expr ->
            addExpressionUndeclared env expr undeclared


addExpressionUndeclared : EnvDeclared -> CA.Expression e -> EnvUndeclared -> EnvUndeclared
addExpressionUndeclared env expr undeclared =
    case expr of
        CA.NumberLiteral e ar ->
            undeclared

        CA.Variable e ar ->
            { undeclared | values = maybeAdd env.values ar.path undeclared.values }

        CA.Lambda e ar ->
            let
                envWithParams =
                    { env | values = Set.union env.values (CA.patternNames ar.parameter) }
            in
            addStatementBlockUndeclared envWithParams ar.body undeclared

        CA.Record e ar ->
            undeclared
                |> (\un -> { un | values = maybeMaybeAdd env.values (Maybe.map .path ar.maybeUpdateTarget) un.values })
                |> (\un -> Dict.foldl (\k -> addExpressionUndeclared env) un ar.attrs)

        CA.Call e ar ->
            undeclared
                |> addExpressionUndeclared env ar.reference
                |> addArgumentUndeclared env ar.argument

        CA.If e ar ->
            undeclared
                |> addStatementBlockUndeclared env ar.condition
                |> addStatementBlockUndeclared env ar.true
                |> addStatementBlockUndeclared env ar.false

        CA.Try e ar ->
            undeclared
                |> addExpressionUndeclared env ar.value
                |> (\u -> List.foldl (\( pa, block ) -> addStatementBlockUndeclared env block) u ar.patterns)


addArgumentUndeclared : EnvDeclared -> CA.Argument e -> EnvUndeclared -> EnvUndeclared
addArgumentUndeclared env arg undeclared =
    case arg of
        CA.ArgumentExpression expr ->
            addExpressionUndeclared env expr undeclared

        CA.ArgumentMutable ar ->
            { undeclared | values = maybeAdd env.values ar.path undeclared.values }



----
---
--


maybeAdd : Set Path -> Path -> Undeclared -> Undeclared
maybeAdd env path undeclared =
    if Set.member path env then
        undeclared

    else
        -- TODO need a location
        Dict.update path (Maybe.withDefault [] >> (::) 111 >> Just) undeclared


maybeMaybeAdd : Set Path -> Maybe Path -> Undeclared -> Undeclared
maybeMaybeAdd env maybePath un =
    case maybePath of
        Nothing ->
            un

        Just path ->
            maybeAdd env path un
