module Compiler.ApplyAliases exposing (..)

{-| The alternative to this module is to make Compiler.TypeCheck do it instead,
but at least for now I want to keep that module as light as possible.

Later on, it might be good to solve alias dependencies and maybe unions, then allow
TypeCheck to do the replacement of annotations on the fly, which would avoid
allocating another AST.

-}

import Dict exposing (Dict)
import Lib
import RefHierarchy
import Set exposing (Set)
import Types.CanonicalAst as CA exposing (Pos, Type)
import Types.Error as Error exposing (Res, errorTodo)


do =
    Lib.result_do


type alias Name =
    String


type T
    = T_Alias CA.AliasDef
    | T_Union CA.UnionDef


{-| TODO Rename to GetT
-}
type alias GetAlias =
    Pos -> Name -> Res T


getAliasSimple : Dict Name CA.AliasDef -> Dict Name CA.UnionDef -> GetAlias
getAliasSimple aliases uns pos name =
    case Dict.get name aliases of
        Just a ->
            Ok <| T_Alias a

        Nothing ->
            case Dict.get name uns of
                Just u ->
                    Ok <| T_Union u

                Nothing ->
                    errorUndefinedType pos name



----


error : Pos -> List String -> Res a
error pos description =
    Error.res { pos = pos, description = \_ -> description }


errorUndefinedType : Pos -> Name -> Res a
errorUndefinedType pos name =
    error pos [ "Undefined type: `" ++ name ++ "`" ]



----
---
--


expandType : GetAlias -> Type -> Res Type
expandType ga ty =
    case ty of
        CA.TypeVariable pos name ->
            Ok ty

        CA.TypeFunction pos from fromIsMutable to ->
            Result.map2
                (\f t -> CA.TypeFunction pos f fromIsMutable t)
                (expandType ga from)
                (expandType ga to)

        CA.TypeRecord pos extensible attrs ->
            attrs
                |> Lib.dict_mapRes (\k -> expandType ga)
                |> Result.map (CA.TypeRecord pos extensible)

        CA.TypeAlias pos path t ->
            -- it's easy to deal with, but it shouldn't happen O_O
            error pos [ "Did we apply aliases twice?" ]

        CA.TypeConstant pos ref args ->
            do (Lib.list_mapRes (expandType ga) args) <| \replacedArgs ->
            case ga pos ref of
                Err e ->
                    Err e

                Ok (T_Union un) ->
                    if List.length replacedArgs /= List.length un.args then
                        error pos [ "union " ++ un.name ++ " needs " ++ String.fromInt (List.length un.args) ++ " args, but was used with " ++ String.fromInt (List.length replacedArgs) ]

                    else
                        replacedArgs
                            |> CA.TypeConstant pos ref
                            |> Ok

                Ok (T_Alias al) ->
                    if List.length al.args /= List.length replacedArgs then
                        error pos [ "alias " ++ al.name ++ " needs " ++ String.fromInt (List.length al.args) ++ " args, but was used with " ++ String.fromInt (List.length replacedArgs) ]

                    else
                        let
                            typeByArgName =
                                List.map2 Tuple.pair al.args replacedArgs |> Dict.fromList
                        in
                        al.ty
                            |> expandAliasVariables typeByArgName
                            |> CA.TypeAlias pos ref
                            |> Ok


expandAndValidateType : GetAlias -> Type -> Res Type
expandAndValidateType ga rawTy =
    do (expandType ga rawTy) <| \expandedTy ->
    case findMutableArgsThatContainFunctions Nothing expandedTy of
        [] ->
            Ok expandedTy

        errors ->
            let
                -- TODO typeToPos?
                pos =
                    CA.S

                m =
                    -- TODO show the actual positions
                    "Mutable arguments can't be or contain functions!" :: List.map Debug.toString errors
            in
            errorTodo (String.join "\n" m)


findMutableArgsThatContainFunctions : Maybe Pos -> Type -> List ( Pos, Pos )
findMutableArgsThatContainFunctions nonFunctionPos ty =
    case ty of
        CA.TypeConstant _ _ _ ->
            []

        CA.TypeVariable _ name ->
            {- TODO
                if mutable then
                    Just "variable types can't be mutable"

               ----> except they can, they must be if we want to have functions
               capable of manipulating mutable containers

            -}
            []

        CA.TypeAlias _ path t ->
            findMutableArgsThatContainFunctions nonFunctionPos t

        CA.TypeFunction functionPos from fromIsMutable to ->
            [ case nonFunctionPos of
                Just constraintPos ->
                    [ ( constraintPos, functionPos ) ]

                Nothing ->
                    []
            , findMutableArgsThatContainFunctions (Lib.ifThenJust fromIsMutable functionPos) from
            , findMutableArgsThatContainFunctions nonFunctionPos to
            ]
                |> List.concat

        CA.TypeRecord _ ext attrs ->
            attrs
                |> Dict.values
                |> List.concatMap (findMutableArgsThatContainFunctions nonFunctionPos)



----
--- Main
--


applyAliasesToModule : CA.AllDefs -> Res CA.AllDefs
applyAliasesToModule mod =
    let
        ( aliases, unions, values ) =
            CA.split mod
    in
    do (applyAliasesToAliases unions aliases) <| \resolved_aliases ->
    do (applyAliasesToUnions resolved_aliases unions) <| \resolved_unions ->
    do (applyAliasesToValues resolved_aliases unions values) <| \resolved_values ->
    let
        a0 =
            Dict.empty

        a1 =
            Dict.foldl (\k v -> Dict.insert k (CA.Alias v)) a0 resolved_aliases

        a2 =
            Dict.foldl (\k v -> Dict.insert k (CA.Union v)) a1 resolved_unions

        a3 =
            Dict.foldl (\k v -> Dict.insert k (CA.Value v)) a2 resolved_values
    in
    Ok a3



----
--- Apply aliases to unions
--


applyAliasesToUnions : Dict Name CA.AliasDef -> Dict Name CA.UnionDef -> Res (Dict Name CA.UnionDef)
applyAliasesToUnions aliases uns =
    let
        ga : GetAlias
        ga =
            getAliasSimple aliases uns

        mapConstructor name args =
            Lib.list_mapRes (expandAndValidateType ga) args

        mapUnion name union =
            Result.map
                (\cs -> { union | constructors = cs })
                (Lib.dict_mapRes mapConstructor union.constructors)
    in
    Lib.dict_mapRes mapUnion uns



----
--- Apply aliases to annotations
--
-- type alias ValueDef a =
--     { a | maybeAnnotation : Maybe CA.Annotation, body : List CA.Statement }


applyAliasesToValues : Dict Name CA.AliasDef -> Dict Name CA.UnionDef -> Dict String CA.RootValueDef -> Res (Dict String CA.RootValueDef)
applyAliasesToValues aliases uns =
    let
        ga : GetAlias
        ga =
            getAliasSimple aliases uns
    in
    Lib.dict_mapRes (\k -> normalizeRootValueDef ga)


normalizeRootValueDef : GetAlias -> CA.RootValueDef -> Res CA.RootValueDef
normalizeRootValueDef ga vdef =
    Result.map2
        (\maybeAnnotation body ->
            { vdef
                | maybeAnnotation = maybeAnnotation
                , body = body
            }
        )
        (normalizeAnnotation ga vdef.maybeAnnotation)
        (normalizeBlock ga vdef.body)


normalizeLocalValueDef : GetAlias -> CA.LocalValueDef -> Res CA.LocalValueDef
normalizeLocalValueDef ga def =
    do (normalizePattern ga def.pattern) <| \pa ->
    do (normalizeBlock ga def.body) <| \body ->
    Ok { def | pattern = pa, body = body }


normalizePattern : GetAlias -> CA.Pattern -> Res CA.Pattern
normalizePattern ga pa =
    case pa of
        CA.PatternDiscard _ ->
            Ok pa

        CA.PatternAny a_pos name Nothing ->
            Ok pa

        CA.PatternAny a_pos name (Just ty) ->
            do (expandAndValidateType ga ty) <| \nty ->
            Ok <| CA.PatternAny a_pos name (Just nty)

        CA.PatternLiteral a_pos value ->
            Ok pa

        CA.PatternConstructor pos name args ->
            do (Lib.list_mapRes (normalizePattern ga) args) <| \nargs ->
            Ok <| CA.PatternConstructor pos name nargs

        CA.PatternRecord pos attrs ->
            do (Lib.dict_mapRes (\k -> normalizePattern ga) attrs) <| \nattrs ->
            Ok <| CA.PatternRecord pos nattrs


normalizeAnnotation : GetAlias -> Maybe CA.Annotation -> Res (Maybe CA.Annotation)
normalizeAnnotation ga maybeType =
    case maybeType of
        Nothing ->
            Ok Nothing

        Just ann ->
            do (expandAndValidateType ga ann.ty) <| \ty ->
            { ann | ty = ty }
                |> Just
                |> Ok


normalizeBlock : GetAlias -> List CA.Statement -> Res (List CA.Statement)
normalizeBlock ga =
    Lib.list_mapRes (normalizeStatement ga)


normalizeStatement : GetAlias -> CA.Statement -> Res CA.Statement
normalizeStatement ga s =
    case s of
        CA.Definition vdef ->
            Result.map CA.Definition (normalizeLocalValueDef ga vdef)

        CA.Evaluation expr ->
            Result.map CA.Evaluation (normalizeExpr ga expr)


normalizeExpr : GetAlias -> CA.Expression -> Res CA.Expression
normalizeExpr ga expr =
    case expr of
        CA.Literal _ _ ->
            Ok expr

        CA.Variable _ _ ->
            Ok expr

        CA.Lambda pos pattern body0 ->
            body0
                |> normalizeBlock ga
                |> Result.map (CA.Lambda pos pattern)

        CA.Record pos extend attrs0 ->
            attrs0
                |> Lib.dict_mapRes (\k -> normalizeExpr ga)
                |> Result.map (CA.Record pos extend)

        CA.Call pos ref0 arg0 ->
            Result.map2
                (CA.Call pos)
                (normalizeExpr ga ref0)
                (normalizeArg ga arg0)

        CA.If pos ar ->
            Result.map3
                (\c t f -> CA.If pos { condition = c, true = t, false = f })
                (normalizeBlock ga ar.condition)
                (normalizeBlock ga ar.true)
                (normalizeBlock ga ar.false)

        CA.Try pos value tries ->
            Result.map2
                (CA.Try pos)
                (normalizeExpr ga value)
                (Lib.list_mapRes (Lib.tuple_mapSecondRes (normalizeBlock ga)) tries)


normalizeArg : GetAlias -> CA.Argument -> Res CA.Argument
normalizeArg ga arg =
    case arg of
        CA.ArgumentMutable _ _ ->
            Ok arg

        CA.ArgumentExpression expr ->
            Result.map CA.ArgumentExpression (normalizeExpr ga expr)



----
--- Apply aliases to aliases
--


applyAliasesToAliases : Dict Name CA.UnionDef -> Dict Name CA.AliasDef -> Res (Dict Name CA.AliasDef)
applyAliasesToAliases uns als =
    let
        orderedAliases =
            als
                |> Dict.values
                |> RefHierarchy.reorder .name findAllRefs_alias
    in
    case orderedAliases of
        Err circular ->
            errorTodo <| "circular alias: " ++ String.join " <- " circular

        Ok oa ->
            Lib.list_foldlRes (processAlias uns als) oa Dict.empty


processAlias : Dict Name CA.UnionDef -> Dict Name CA.AliasDef -> CA.AliasDef -> Dict Name CA.AliasDef -> Res (Dict Name CA.AliasDef)
processAlias uns allAliases al processedAliases =
    let
        getAlias : GetAlias
        getAlias pos name =
            if Dict.member name allAliases then
                case Dict.get name processedAliases of
                    Nothing ->
                        Debug.todo <| "ApplyAliases should-not-happen: " ++ name

                    Just processedAlias ->
                        Ok <| T_Alias processedAlias

            else
                case Dict.get name uns of
                    Just un ->
                        Ok <| T_Union un

                    Nothing ->
                        errorUndefinedType pos name
    in
    do (expandAndValidateType getAlias al.ty) <| \ty ->
    Dict.insert al.name { al | ty = ty } processedAliases
        |> Ok


expandAliasVariables : Dict Name Type -> Type -> Type
expandAliasVariables typeByArgName ty =
    case ty of
        CA.TypeVariable pos name ->
            case Dict.get name typeByArgName of
                Nothing ->
                    ty

                Just t ->
                    t

        CA.TypeFunction pos from fromIsMutable to ->
            CA.TypeFunction pos
                (expandAliasVariables typeByArgName from)
                fromIsMutable
                (expandAliasVariables typeByArgName to)

        CA.TypeRecord pos extensible attrs ->
            CA.TypeRecord pos
                extensible
                (Dict.map (\k -> expandAliasVariables typeByArgName) attrs)

        CA.TypeConstant pos ref args ->
            CA.TypeConstant pos
                ref
                (List.map (expandAliasVariables typeByArgName) args)

        CA.TypeAlias pos ref t ->
            CA.TypeAlias pos ref (expandAliasVariables typeByArgName t)



----
--- Find all refs
--


findAllRefs_alias : CA.AliasDef -> Set String
findAllRefs_alias al =
    findAllRefs_type al.ty


findAllRefs_type : CA.Type -> Set String
findAllRefs_type ty =
    case ty of
        CA.TypeConstant pos ref args ->
            List.foldl (\ar -> Set.union (findAllRefs_type ar)) (Set.singleton ref) args

        CA.TypeVariable pos name ->
            Set.empty

        CA.TypeFunction pos from maybeMut to ->
            Set.union (findAllRefs_type from) (findAllRefs_type to)

        CA.TypeRecord pos extensible attrs ->
            Dict.foldl (\name t -> Set.union (findAllRefs_type t)) Set.empty attrs

        CA.TypeAlias pos path t ->
            findAllRefs_type t
