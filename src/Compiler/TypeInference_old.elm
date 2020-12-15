module Compiler.TypeInference exposing (..)

{-| I don't understand what I'm doing, I'm just following

Da leggere:
<http://steshaw.org/hm/hindley-milner.pdf>
<https://web.cecs.pdx.edu/~mpj/thih/thih.pdf>

Letti:
<http://reasonableapproximation.net/2019/05/05/hindley-milner.html>
<https://stackoverflow.com/questions/12532552/what-part-of-hindley-milner-do-you-not-understand>
<https://medium.com/@dhruvrajvanshi/type-inference-for-beginners-part-1-3e0a5be98a4b>
<https://ltbringer.github.io/blog/hindley-milner-for-humans>
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


type Type
    = TypeMutable Type
    | TypeInferencePlaceholder PlaceholderId
    | TypeConstant
        { name : String
        }
    | TypeFunction
        { from : Type
        , to : Type
        }
    | TypeRecord
        (List
            { name : String
            , type_ : Type
            }
        )


type alias PlaceholderId =
    Int


{-| type environment
-}
type alias Env =
    Dict String Type


type alias Substitutions =
    Dict PlaceholderId Type



----
--- Higher level stuff
--


inferScope : Env -> Dict String (CA.Expression e) -> Res Env
inferScope preamble scope =
    let
        ( env0, nextId0 ) =
            scope
                |> Dict.keys
                |> addSymbols 0 preamble

        rec : PlaceholderId -> Env -> List ( String, CA.Expression e ) -> Res Env
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
                                        Just (TypeInferencePlaceholder oldPlaceholderId) ->
                                            if TypeInferencePlaceholder oldPlaceholderId == type_ then
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


addSymbol : PlaceholderId -> Env -> String -> ( Type, ( Env, PlaceholderId ) )
addSymbol nextId0 env name =
    let
        type_ =
            TypeInferencePlaceholder nextId0
    in
    ( type_
    , ( Dict.insert name type_ env
      , nextId0 + 1
      )
    )


bindPlaceholderIdToType : PlaceholderId -> Type -> Res Substitutions
bindPlaceholderIdToType id type_ =
    -- TODO Is this really needed? So far `unify` checks already if the two types are the same
    if TypeInferencePlaceholder id == type_ then
        Ok Dict.empty

    else if typeContains id type_ then
        Err <| "TypeInferencePlaceholder " ++ String.fromInt id ++ " is contained by " ++ Debug.toString type_

    else
        Ok <| Dict.singleton id type_


typeContains : PlaceholderId -> Type -> Bool
typeContains id type_ =
    case type_ of
        TypeInferencePlaceholder tid ->
            tid == id

        TypeConstant _ ->
            False

        TypeFunction args ->
            typeContains id args.from || typeContains id args.to

        TypeRecord attrs ->
            List.any (\attr -> typeContains id attr.type_) attrs

        TypeMutable t ->
            typeContains id t



----
--- Unify
--


unify : Type -> Type -> Res Substitutions
unify a b =
    if a == b then
        Ok Dict.empty

    else
        case ( a, b ) of
            ( TypeInferencePlaceholder aId, _ ) ->
                bindPlaceholderIdToType aId b

            ( _, TypeInferencePlaceholder bId ) ->
                bindPlaceholderIdToType bId a

            ( TypeFunction ta, TypeFunction tb ) ->
                result_do (unify ta.from tb.from) <| \sub1 ->
                result_do (unify (applySubstitutionsToType sub1 ta.to) (applySubstitutionsToType sub1 tb.to)) <| \sub2 ->
                composeSubstitutions sub1 sub2

            ( TypeRecord aAttrs, TypeRecord bAttrs ) ->
                --                 result_do (unify a1 b1) <| \sub1 ->
                --                 result_do (unify (applySubstitutionsToType sub1 a2) (applySubstitutionsToType sub1 b2)) <| \sub2 ->
                --                 composeSubstitutions sub1 sub2
                Debug.todo ""

            _ ->
                Err <| "Cannot match `" ++ Debug.toString a ++ "` with `" ++ Debug.toString b ++ "`"



----
--- Substitutions
--


applySubstitutionsToType : Substitutions -> Type -> Type
applySubstitutionsToType substitutions targetType =
    case targetType of
        TypeConstant _ ->
            targetType

        TypeInferencePlaceholder placeholderId ->
            case Dict.get placeholderId substitutions of
                Just inferredType ->
                    inferredType

                Nothing ->
                    targetType

        TypeFunction { from, to } ->
            TypeFunction
                { from = applySubstitutionsToType substitutions from
                , to = applySubstitutionsToType substitutions to
                }

        TypeRecord attrs ->
            attrs
                |> List.map (\a -> { a | type_ = applySubstitutionsToType substitutions a.type_ })
                |> TypeRecord

        TypeMutable t ->
            TypeMutable <| applySubstitutionsToType substitutions t


composeSubstitutions : Substitutions -> Substitutions -> Res Substitutions
composeSubstitutions a b =
    let
        rec : List ( PlaceholderId, Type ) -> Substitutions -> Res Substitutions
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


inferExpr : PlaceholderId -> Env -> CA.Expression e -> Res ( Type, Substitutions, PlaceholderId )
inferExpr nextId0 env expr =
    case expr of
        CA.NumberLiteral _ _ ->
            Ok
                ( TypeConstant { name = "Number" }
                , Dict.empty
                , nextId0
                )

        CA.Variable _ args ->
            case Dict.get args.name env of
                Just t ->
                    Ok
                        ( t
                        , Dict.empty
                        , nextId0
                        )

                Nothing ->
                    Err "variable not in scope"

        CA.Lambda _ { parameter, body } ->
            let
                ( parameterType, ( childEnv, newPlaceholderId ) ) =
                    addSymbol nextId0 env parameter
            in
            case inferExpr newPlaceholderId childEnv body of
                Err e ->
                    Err e

                Ok ( bodyType, substitutions, newNewPlaceholderId ) ->
                    Ok
                        ( TypeFunction
                            { from = applySubstitutionsToType substitutions parameterType
                            , to = bodyType
                            }
                        , substitutions
                        , newNewPlaceholderId
                        )

        CA.Record _ attrs ->
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

        CA.Call _ { reference, argument, argumentIsMutable } ->
            -- TODO argumentIsMutable!!!
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
            result_do (unify (applySubstitutionsToType s5 extracted.type_.from) argumentType) <| \s6 ->
            result_do (composeSubstitutions s5 s6) <| \s7 ->
            Ok
                ( applySubstitutionsToType s7 extracted.type_.to
                , s7
                , n3
                )

        CA.If _ { start, condition, true, false } ->
            Debug.todo ""


type alias ExtractFunctionTypesReturn =
    { type_ :
        { from : Type
        , to : Type
        }
    , subs : Substitutions
    , nextId : PlaceholderId
    }


extractFunctionTypes : PlaceholderId -> Type -> Res ExtractFunctionTypesReturn
extractFunctionTypes nextId0 t =
    case t of
        TypeFunction type_ ->
            Ok
                { type_ = type_
                , subs = Dict.empty
                , nextId = nextId0
                }

        TypeInferencePlaceholder pid ->
            let
                type_ =
                    { from = TypeInferencePlaceholder nextId0
                    , to = TypeInferencePlaceholder (nextId0 + 1)
                    }

                nextId1 =
                    nextId0 + 2
            in
            Ok
                { nextId = nextId1
                , type_ = type_
                , subs =
                    type_
                        |> TypeFunction
                        |> Dict.singleton pid
                }

        _ ->
            Err "trying to call something that is not a function!"