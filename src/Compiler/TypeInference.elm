module Compiler.TypeInference exposing (..)

{-| I don't understand what I'm doing, I'm just following

<https://medium.com/@dhruvrajvanshi/type-inference-for-beginners-part-1-3e0a5be98a4b>
<http://steshaw.org/hm/hindley-milner.pdf>

<http://reasonableapproximation.net/2019/05/05/hindley-milner.html>
<https://ltbringer.github.io/blog/hindley-milner-for-humans>
<https://stackoverflow.com/questions/12532552/what-part-of-hindley-milner-do-you-not-understand>
<https://web.cecs.pdx.edu/~mpj/thih/thih.pdf>
<https://course.ccs.neu.edu/cs4410sp19/lec_type-inference_notes.html>

-}

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
    | Record (List ( String, InferredType ))


type alias PlaceholderId =
    Int


type alias Env =
    Dict String InferredType


type alias Substitutions =
    Dict PlaceholderId InferredType



----
--- Higher level stuff
--


inferScope : Env -> Dict String CA.Expression -> Res Env
inferScope preamble scope =
    let
        ( env0, nextId0 ) =
            scope
                |> Dict.keys
                |> addSymbols 0 preamble

        rec : PlaceholderId -> Env -> List ( String, CA.Expression ) -> Res Env
        rec nextId env symbols =
            case symbols of
                [] ->
                    Ok (Dict.diff env preamble)

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

                                        Just _ ->
                                            subs0

                                        Nothing ->
                                            Debug.todo <| "ENV DOES NOT CONTAIN VAR NAME: " ++ name
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

        Record attrs ->
            List.any (\( name, t ) -> typeContains id t) attrs



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

            ( Record aAttrs, Record bAttrs ) ->
                --                 result_do (unify a1 b1) <| \sub1 ->
                --                 result_do (unify (applySubstitutionsToType sub1 a2) (applySubstitutionsToType sub1 b2)) <| \sub2 ->
                --                 composeSubstitutions sub1 sub2
                Debug.todo ""

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

        Record attrs ->
            attrs
                |> List.map (Tuple.mapSecond <| applySubstitutionsToType substitutions)
                |> Record


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

        CA.Record attrs ->
            --             result_do (inferExpr nextId0 env first) <| \( firstType, firstSubs, pid0 ) ->
            --             result_do (inferExpr pid0 env second) <| \( secondType, secondSubs, pid1 ) ->
            --             result_do (composeSubstitutions firstSubs secondSubs) <| \unifiedSubs ->
            --             Ok
            --                 ( Record
            --                     (applySubstitutionsToType secondSubs firstType)
            --                     (applySubstitutionsToType firstSubs secondType)
            --                 , unifiedSubs
            --                 , pid1
            --                 )
            Debug.todo ""

        CA.Call { reference, argument } ->
            result_do (inferExpr nextId0 env reference) <| \( actualFunctionType, s1, n1 ) ->
            let
                env1 =
                    applySubstitutionsToEnv s1 env
            in
            result_do (inferExpr n1 env1 argument) <| \( argumentType, s2, n2 ) ->
            result_do (composeSubstitutions s1 s2) <| \s3 ->
            result_do (extractFunctionTypes n2 actualFunctionType) <| \extracted ->
            let
                s4 =
                    extracted.subs

                n3 =
                    extracted.nextId
            in
            result_do (composeSubstitutions s3 s4) <| \s5 ->
            result_do (unify (applySubstitutionsToType s5 extracted.inType) argumentType) <| \s6 ->
            result_do (composeSubstitutions s5 s6) <| \s7 ->
            Ok
                ( applySubstitutionsToType s7 extracted.outType
                , s7
                , n3
                )

        CA.If { start, condition, true, false } ->
            Debug.todo ""


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
