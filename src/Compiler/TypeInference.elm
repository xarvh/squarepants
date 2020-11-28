module Compiler.TypeInference exposing (..)

import Dict exposing (Dict)
import Types.CanonicalAst as CA



----
--- Should be library
--


result_do : Result err a -> (a -> Result err b) -> Result err b
result_do r f =
    Result.andThen f r



----
--- Types
--


type alias Res a =
    Result String a


type InferredType
    = Named String
    | TypeVariable PlaceholderId
    | Function InferredType InferredType
    | Tuple2 InferredType InferredType


type alias PlaceholderId =
    Int


type alias Env =
    Dict String InferredType


type alias Substitutions =
    Dict PlaceholderId InferredType



----
--- Higher level stuff
--


inferScope : Dict String CA.Expression -> Res Env
inferScope scope =
    let
        ( env0, nextId0 ) =
            scope
                |> Dict.keys
                |> addSymbols 0 Dict.empty

        rec : PlaceholderId -> Env -> List ( String, CA.Expression ) -> Res Env
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
                                    |> applySubstitutionsToEnv subs1
                                )
                                tail
    in
    rec nextId0 env0 (Dict.toList scope)


addSymbols : PlaceholderId -> Env -> List String -> ( Env, PlaceholderId )
addSymbols nextId0 env names =
    let
        fold : String -> ( Env, PlaceholderId ) -> ( Env, PlaceholderId )
        fold name ( envAccum, nextId ) =
            Tuple.second <| addSymbol nextId envAccum name
    in
    List.foldl fold ( env, nextId0 ) names



----
--- Helpers (ie, more or less straightforward stuff that doesn't contain much payload logic)
--


applySubstitutionsToEnv : Substitutions -> Env -> Env
applySubstitutionsToEnv subs env =
    Dict.map (\k v -> applySubstitutionsToType subs v) env


addSymbol : PlaceholderId -> Env -> String -> ( InferredType, ( Env, PlaceholderId ) )
addSymbol nextId0 env name =
    let
        type_ =
            TypeVariable nextId0
    in
    ( type_
    , ( Dict.insert name type_ env
      , nextId0 + 1
      )
    )


extractFunctionTypes : PlaceholderId -> InferredType -> Res { inType : InferredType, outType : InferredType, subs : Substitutions, nextId : PlaceholderId }
extractFunctionTypes nextId0 t =
    case t of
        Function inType outType ->
            Ok
                { inType = inType
                , outType = outType
                , subs = Dict.empty
                , nextId = nextId0
                }

        TypeVariable pid ->
            let
                inType =
                    TypeVariable nextId0

                outType =
                    TypeVariable (nextId0 + 1)

                nextId1 =
                    nextId0 + 2
            in
            Ok
                { inType = inType
                , outType = outType
                , subs = Dict.singleton pid (Function inType outType)
                , nextId = nextId1
                }

        _ ->
            Err "trying to call something that is not a function!"


bindPlaceholderIdToType : PlaceholderId -> InferredType -> Res Substitutions
bindPlaceholderIdToType id type_ =
    -- TODO Is this really needed? So far `unify` checks already if the two types are the same
    if TypeVariable id == type_ then
        Ok Dict.empty

    else if typeContains id type_ then
        Err <| "TypeVariable " ++ String.fromInt id ++ " is contained by " ++ Debug.toString type_

    else
        Ok <| Dict.singleton id type_


typeContains : PlaceholderId -> InferredType -> Bool
typeContains id type_ =
    case type_ of
        Named _ ->
            False

        TypeVariable tid ->
            tid == id

        Function i o ->
            typeContains id i || typeContains id o

        Tuple2 a b ->
            typeContains id a || typeContains id b



----
--- Unify
--


unify : InferredType -> InferredType -> Res Substitutions
unify a b =
    if a == b then
        Ok Dict.empty

    else
        case ( a, b ) of
            ( TypeVariable aId, _ ) ->
                bindPlaceholderIdToType aId b

            ( _, TypeVariable bId ) ->
                bindPlaceholderIdToType bId a

            ( Function aIn aOut, Function bIn bOut ) ->
                result_do (unify aIn bIn) <| \sub1 ->
                result_do (unify (applySubstitutionsToType sub1 aOut) (applySubstitutionsToType sub1 bOut)) <| \sub2 ->
                composeSubstitutions sub1 sub2

            ( Tuple2 a1 a2, Tuple2 b1 b2 ) ->
                result_do (unify a1 b1) <| \sub1 ->
                result_do (unify (applySubstitutionsToType sub1 a2) (applySubstitutionsToType sub1 b2)) <| \sub2 ->
                composeSubstitutions sub1 sub2

            _ ->
                Err <| "Cannot match `" ++ Debug.toString a ++ "` with `" ++ Debug.toString b ++ "`"



----
--- Substitutions
--


applySubstitutionsToType : Substitutions -> InferredType -> InferredType
applySubstitutionsToType substitutions targetType =
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
            Function
                (applySubstitutionsToType substitutions paramType)
                (applySubstitutionsToType substitutions bodyType)

        Tuple2 fst snd ->
            Tuple2
                (applySubstitutionsToType substitutions fst)
                (applySubstitutionsToType substitutions snd)


composeSubstitutions : Substitutions -> Substitutions -> Res Substitutions
composeSubstitutions a b =
    let
        rec : List ( PlaceholderId, InferredType ) -> Substitutions -> Res Substitutions
        rec aAsList accum =
            case aAsList of
                [] ->
                    Ok accum

                ( id, aType ) :: tail ->
                    let
                        maybeBType =
                            Dict.get id b
                    in
                    if maybeBType == Nothing || maybeBType == Just aType then
                        rec tail (Dict.insert id aType accum)

                    else
                        Err <| Debug.toString (Just aType) ++ " vs " ++ Debug.toString maybeBType
    in
    rec (Dict.toList a) b



----
--- Infer
--


inferExpr : PlaceholderId -> Env -> CA.Expression -> Res ( InferredType, Substitutions, PlaceholderId )
inferExpr nextId0 env expr =
    case expr of
        CA.NumberLiteral _ ->
            Ok
                ( Named "Number"
                , Dict.empty
                , nextId0
                )

        CA.Variable args ->
            case Dict.get args.variable env of
                Just t ->
                    Ok
                        ( t
                        , Dict.empty
                        , nextId0
                        )

                Nothing ->
                    Err "variable not in scope"

        CA.Lambda { parameter, body } ->
            let
                ( parameterType, ( childEnv, newPlaceholderId ) ) =
                    addSymbol nextId0 env parameter
            in
            case inferExpr newPlaceholderId childEnv body of
                Err e ->
                    Err e

                Ok ( bodyType, substitutions, newNewPlaceholderId ) ->
                    Ok
                        ( Function (applySubstitutionsToType substitutions parameterType) bodyType
                        , substitutions
                        , newNewPlaceholderId
                        )

        CA.Tuple2 { first, second } ->
            result_do (inferExpr nextId0 env first) <| \( firstType, firstSubs, pid0 ) ->
            result_do (inferExpr pid0 env second) <| \( secondType, secondSubs, pid1 ) ->
            result_do (composeSubstitutions firstSubs secondSubs) <| \unifiedSubs ->
            Ok
                ( Tuple2
                    (applySubstitutionsToType secondSubs firstType)
                    (applySubstitutionsToType firstSubs secondType)
                , unifiedSubs
                , pid1
                )

        CA.Call { reference, argument } ->
            result_do (inferExpr nextId0 env reference) <| \( refType, refSubs, nextId1 ) ->
            result_do (extractFunctionTypes nextId1 refType) <| \{ inType, outType, subs, nextId } ->
            let
                env0 =
                    applySubstitutionsToEnv subs
            in
            -- TODO ?? composeSubstitutions refSubs extracted.subs
            result_do (inferExpr nextId env argument) <| \( argType, argSubs, nextId3 ) ->
            result_do (unify inType argType) <| \unifSubs ->
            -- TODO result_do (composeSubstitutions yyyyyy) <| \unifiedSubs ->
            Ok
                ( outType
                , unifSubs
                , nextId3
                )

        CA.If { start, condition, true, false } ->
            Debug.todo ""
