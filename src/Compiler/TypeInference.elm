module Compiler.TypeInference exposing (..)

import Dict exposing (Dict)
import Types.CanonicalAst as CA


type InferredType
    = Named String
    | TypeVariable PlaceholderId
    | Function InferredType InferredType


type alias PlaceholderId =
    Int


type alias Env =
    Dict String InferredType


type alias Substitutions =
    Dict PlaceholderId InferredType



----
---
--


addSymbols : PlaceholderId -> Env -> List String -> ( Env, PlaceholderId )
addSymbols nextPlaceholderId env names =
    let
        fold : String -> ( Env, PlaceholderId ) -> ( Env, PlaceholderId )
        fold name ( envAccum, nextId ) =
            Tuple.second <| addSymbol nextId envAccum name
    in
    List.foldl fold ( env, nextPlaceholderId ) names


inferScope : Dict String CA.Expression -> Result String Env
inferScope scope =
    let
        ( env0, nextId0 ) =
            scope
                |> Dict.keys
                |> addSymbols 0 Dict.empty
                |> Debug.log "AAAAA"

        rec : PlaceholderId -> Env -> List ( String, CA.Expression ) -> Result String Env
        rec nextId env symbols =
            case symbols of
                [] ->
                    Ok env

                ( name, expr ) :: tail ->
                    case inferExpr nextId env expr of
                        Err err ->
                            Err <| name ++ ": " ++ err

                        Ok ( type_, subs0, newNextId ) ->
                            let
                                subs1 =
                                    case Dict.get name env of
                                        Just (TypeVariable oldPlaceholderId) ->
                                            if TypeVariable oldPlaceholderId == type_ then
                                                subs0

                                            else
                                                Dict.insert oldPlaceholderId type_ subs0

                                        _ ->
                                            Debug.todo "ENV DOES NOT CONTAIN VAR NAME"

                                _ =
                                    Debug.log ("substs for: " ++ name) subs1
                            in
                            rec
                                newNextId
                                (env
                                    |> Dict.insert name type_
                                    |> Dict.map (\k v -> applySubstitutions subs1 v)
                                )
                                tail
    in
    rec nextId0 env0 (Dict.toList scope)



----
---
--


applySubstitutions : Substitutions -> InferredType -> InferredType
applySubstitutions substitutions targetType =
    case targetType of
        Named s ->
            targetType

        TypeVariable placeholderId ->
            case Dict.get placeholderId substitutions of
                Just inferredType ->
                    inferredType

                Nothing ->
                    targetType

        Function paramType bodyType ->
            Function (applySubstitutions substitutions paramType) (applySubstitutions substitutions bodyType)


addSymbol : PlaceholderId -> Env -> String -> ( InferredType, ( Env, PlaceholderId ) )
addSymbol nextPlaceholderId env name =
    let
        type_ =
            TypeVariable nextPlaceholderId
    in
    ( type_
    , ( Dict.insert name type_ env
      , nextPlaceholderId + 1
      )
    )


inferExpr : PlaceholderId -> Env -> CA.Expression -> Result String ( InferredType, Substitutions, PlaceholderId )
inferExpr nextPlaceholderId env expr =
    case expr of
        CA.NumberLiteral _ ->
            Ok
                ( Named "Number"
                , Dict.empty
                , nextPlaceholderId
                )

        CA.Variable args ->
            case Dict.get args.variable env of
                Just t ->
                    Ok
                        ( t
                        , Dict.empty
                        , nextPlaceholderId
                        )

                Nothing ->
                    Err "variable not in scope"

        CA.Lambda { parameter, body } ->
            let
                ( parameterType, ( childEnv, newPlaceholderId ) ) =
                    addSymbol nextPlaceholderId env parameter
            in
            case inferExpr newPlaceholderId childEnv body of
                Err e ->
                    Err e

                Ok ( bodyType, substitutions, newNewPlaceholderId ) ->
                    Ok
                        ( Function (applySubstitutions substitutions parameterType) bodyType
                        , substitutions
                        , newNewPlaceholderId
                        )

        CA.Call { reference, argument } ->
            Debug.todo ""

        CA.If { start, condition, true, false } ->
            Debug.todo ""
