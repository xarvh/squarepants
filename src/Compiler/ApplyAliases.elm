module Compiler.ApplyAliases exposing (..)

import Dict exposing (Dict)
import Lib
import RefHierarchy
import Set exposing (Set)
import Types.CanonicalAst as CA exposing (Name, Type)
import Types.Error as Error exposing (Res, errorTodo)


{-| -}
applyAliasesToModule : CA.Module () -> Res (CA.Module ())
applyAliasesToModule mod =
    Lib.result_do (applyAliasesToAliases mod.aliases) <| \aliases ->
    Lib.result_do (applyAliasesToUnions aliases mod.types) <| \unions ->
    Ok
        -- TODO apply aliases to unions
        -- TODO apply aliases to annotations
        { mod | aliases = aliases }




applyAliasesToUnions : Dict Name CA.AliasDef -> Dict Name CA.UnionDef -> Res (Dict Name CA.UnionDef)
applyAliasesToUnions aliases unions =
  Ok unions





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
    Lib.result_fold (processAlias als) orderedAliases Dict.empty


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


replaceType : (Name -> Res (Maybe CA.AliasDef)) -> Type -> Res Type
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
                |> Lib.dict_resMap (\k -> replaceType getAlias)
                |> Result.map (\a -> CA.TypeRecord { extensible = extensible, attrs = a })

        CA.TypeAlias path t ->
            -- it's easy to deal with, but it shouldn't happen O_O
            Debug.todo "why is this happening? o_O"

        CA.TypeConstant { path, args } ->
            let
                fold arg acc =
                    arg
                        |> replaceType getAlias
                        |> Result.map (\t -> t :: acc)
            in
            Lib.result_do (Lib.result_fold fold args []) <| \replacedArgs ->
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
