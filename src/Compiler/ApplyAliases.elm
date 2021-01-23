module Compiler.ApplyAliases exposing (..)

{-| The alternative to this module is to make Compiler.TypeInference do it instead,
but at least for now I want to keep that module as light as possible.

Later on, it might be good to solve alias dependencies and maybe unions, then allow
TypeInference to do the replacement of annotations on the fly, which would avoid
allocating another AST.

-}

import Dict exposing (Dict)
import Lib
import RefHierarchy
import Set exposing (Set)
import Types.CanonicalAst as CA exposing (Name, Type)
import Types.Error as Error exposing (Res, errorTodo)


type alias GetAlias =
    Name -> Res (Maybe CA.AliasDef)


replaceType : GetAlias -> Type -> Res Type
replaceType getAlias ty =
    case ty of
        CA.TypeVariable { name } ->
            Ok ty

        CA.TypeFunction { from, fromIsMutable, to } ->
            Result.map2
                (\f t ->
                    CA.TypeFunction
                        { from = f
                        , fromIsMutable = fromIsMutable
                        , to = t
                        }
                )
                (replaceType getAlias from)
                (replaceType getAlias to)

        CA.TypeRecord { extensible, attrs } ->
            attrs
                |> Lib.dict_mapRes (\k -> replaceType getAlias)
                |> Result.map (\a -> CA.TypeRecord { extensible = extensible, attrs = a })

        CA.TypeAlias path t ->
            -- it's easy to deal with, but it shouldn't happen O_O
            errorTodo "why is this happening? o_O"

        CA.TypeConstant { path, args } ->
            let
                fold arg acc =
                    arg
                        |> replaceType getAlias
                        |> Result.map (\t -> t :: acc)
            in
            Lib.result_do (Lib.list_foldlRes fold args []) <| \replacedArgs ->
            case getAlias path of
                Err e ->
                    Err e

                Ok Nothing ->
                    { path = path
                    , args = List.reverse replacedArgs
                    }
                        |> CA.TypeConstant
                        |> Ok

                Ok (Just al) ->
                    if List.length al.args /= List.length replacedArgs then
                        errorTodo <| "alias " ++ al.name ++ " needs " ++ String.fromInt (List.length al.args) ++ " args, but was used with " ++ String.fromInt (List.length replacedArgs)

                    else
                        let
                            typeByArgName =
                                List.map2 Tuple.pair al.args replacedArgs |> Dict.fromList
                        in
                        al.ty
                            |> expandAliasVariables typeByArgName
                            |> CA.TypeAlias path
                            |> Ok



----
--- Main
--


{-| -}
applyAliasesToModule : CA.Module () -> Res (CA.Module ())
applyAliasesToModule mod =
    Lib.result_do (applyAliasesToAliases mod.aliases) <| \aliases ->
    Lib.result_do (applyAliasesToUnions aliases mod.unions) <| \unions ->
    Lib.result_do (applyAliasesToValues aliases mod.values) <| \values ->
    Ok
        { aliases = aliases
        , unions = unions
        , values = values
        }



----
--- Apply aliases to unions
--


applyAliasesToUnions : Dict Name CA.AliasDef -> Dict Name CA.UnionDef -> Res (Dict Name CA.UnionDef)
applyAliasesToUnions aliases =
    let
        getAlias : GetAlias
        getAlias name =
            Ok <| Dict.get name aliases

        mapConstructor c =
            c.args
                |> Lib.list_mapRes (replaceType getAlias)
                |> Result.map (\args -> { c | args = args })

        mapUnion name union =
            Result.map
                (\cs -> { union | constructors = cs })
                (Lib.list_mapRes mapConstructor union.constructors)
    in
    Lib.dict_mapRes mapUnion



----
--- Apply aliases to annotations
--


applyAliasesToValues : Dict Name CA.AliasDef -> Dict Name (CA.ValueDef e) -> Res (Dict Name (CA.ValueDef e))
applyAliasesToValues aliases =
    let
        ga : GetAlias
        ga name =
            Dict.get name aliases |> Ok
    in
    Lib.dict_mapRes (\k -> normalizeValueDef ga)


normalizeValueDef : GetAlias -> CA.ValueDef e -> Res (CA.ValueDef e)
normalizeValueDef ga vdef =
    Result.map2
        (\maybeAnnotation body ->
            { vdef
                | maybeAnnotation = maybeAnnotation
                , body = body
            }
        )
        (normalizeAnnotation ga vdef.maybeAnnotation)
        (Lib.list_mapRes (normalizeStatement ga) vdef.body)


normalizeAnnotation : GetAlias -> Maybe Type -> Res (Maybe Type)
normalizeAnnotation ga maybeType =
    case maybeType of
        Nothing ->
            Ok Nothing

        Just ty ->
            replaceType ga ty |> Result.map Just


normalizeStatement : GetAlias -> CA.Statement e -> Res (CA.Statement e)
normalizeStatement ga s =
    case s of
        CA.Definition vdef ->
            Result.map CA.Definition (normalizeValueDef ga vdef)

        CA.Evaluation expr ->
            Result.map CA.Evaluation (normalizeExpr ga expr)


normalizeExpr : GetAlias -> CA.Expression e -> Res (CA.Expression e)
normalizeExpr ga expr =
    case expr of
        CA.NumberLiteral _ _ ->
            Ok expr

        CA.Variable _ _ ->
            Ok expr

        CA.Lambda e ar ->
            ar.body
                |> Lib.list_mapRes (normalizeStatement ga)
                |> Result.map (\body -> CA.Lambda e { ar | body = body })

        CA.Record e ar ->
            ar.attrs
                |> Lib.dict_mapRes (\k -> normalizeExpr ga)
                |> Result.map (\attrs -> CA.Record e { ar | attrs = attrs })

        CA.Call e ar ->
            Result.map2
                (\ref arg -> CA.Call e { reference = ref, argument = arg })
                (normalizeExpr ga ar.reference)
                (normalizeArg ga ar.argument)

        CA.If e ar ->
            Result.map3
                (\c t f -> CA.If e { start = ar.start, condition = c, true = t, false = f })
                (Lib.list_mapRes (normalizeStatement ga) ar.condition)
                (Lib.list_mapRes (normalizeStatement ga) ar.true)
                (Lib.list_mapRes (normalizeStatement ga) ar.false)


normalizeArg : GetAlias -> CA.Argument e -> Res (CA.Argument e)
normalizeArg ga arg =
    case arg of
        CA.ArgumentMutable _ ->
            Ok arg

        CA.ArgumentExpression expr ->
            Result.map CA.ArgumentExpression (normalizeExpr ga expr)



----
--- Apply aliases to aliases
--


applyAliasesToAliases : Dict Name CA.AliasDef -> Res (Dict Name CA.AliasDef)
applyAliasesToAliases als =
    let
        orderedAliases =
            als
                |> Dict.values
                |> RefHierarchy.reorder .name findAllRefs_alias
    in
    Lib.list_foldlRes (processAlias als) orderedAliases Dict.empty


processAlias : Dict Name CA.AliasDef -> CA.AliasDef -> Dict Name CA.AliasDef -> Res (Dict Name CA.AliasDef)
processAlias allAliases al processedAliases =
    let
        getAlias name =
            if Dict.member name allAliases then
                case Dict.get name processedAliases of
                    Nothing ->
                        errorTodo "circular!"

                    Just processedAlias ->
                        Ok (Just processedAlias)

            else
                Ok Nothing
    in
    Lib.result_do (replaceType getAlias al.ty) <| \ty ->
    Dict.insert al.name { al | ty = ty } processedAliases
        |> Ok


expandAliasVariables : Dict Name Type -> Type -> Type
expandAliasVariables typeByArgName ty =
    case ty of
        CA.TypeVariable { name } ->
            case Dict.get name typeByArgName of
                Nothing ->
                    ty

                Just t ->
                    t

        CA.TypeFunction { from, fromIsMutable, to } ->
            CA.TypeFunction
                { from = expandAliasVariables typeByArgName from
                , fromIsMutable = fromIsMutable
                , to = expandAliasVariables typeByArgName to
                }

        CA.TypeRecord { extensible, attrs } ->
            CA.TypeRecord
                { extensible = extensible
                , attrs = Dict.map (\k -> expandAliasVariables typeByArgName) attrs
                }

        CA.TypeConstant { path, args } ->
            CA.TypeConstant
                { path = path
                , args = List.map (expandAliasVariables typeByArgName) args
                }

        CA.TypeAlias path t ->
            CA.TypeAlias path (expandAliasVariables typeByArgName t)



----
--- Find all refs
--


findAllRefs_alias : CA.AliasDef -> Set String
findAllRefs_alias al =
    findAllRefs_type al.ty


findAllRefs_type : CA.Type -> Set String
findAllRefs_type ty =
    case ty of
        CA.TypeConstant { path, args } ->
            List.foldl (\ar -> Set.union (findAllRefs_type ar)) (Set.singleton path) args

        CA.TypeVariable { name } ->
            Set.empty

        CA.TypeFunction { from, to } ->
            Set.union (findAllRefs_type from) (findAllRefs_type to)

        CA.TypeRecord { extensible, attrs } ->
            Dict.foldl (\name t -> Set.union (findAllRefs_type t)) Set.empty attrs

        CA.TypeAlias path t ->
            findAllRefs_type t
