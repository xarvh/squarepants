module Compiler.FormattableToCanonicalAst exposing (..)

import Dict exposing (Dict)
import OneOrMore
import Types.CanonicalAst as CA
import Types.Error as Error exposing (Error)
import Types.FormattableAst as FA


type alias Res ok =
    Result Error ok



----
--- Module
--


translateModule : FA.Module -> Res (CA.Module ())
translateModule faModule =
    translateModuleRec
        faModule
        { typeDefinitions = Dict.empty
        , valueDefinitions = Dict.empty
        }


translateModuleRec : FA.Module -> CA.Module () -> Res (CA.Module ())
translateModuleRec faModule caModule =
    case faModule of
        [] ->
            Ok caModule

        faStat :: faStatTail ->
            caModule
                |> insertStatement faStat
                |> Result.andThen (translateModuleRec faStatTail)


insertStatement : FA.Statement -> CA.Module () -> Res (CA.Module ())
insertStatement faStatement caModule =
    case faStatement of
        FA.Evaluation _ ->
            errorTodo "Root Evaluations don't really do much =|"

        FA.Definition fa ->
            fa
                |> translateDefinition
                |> Result.andThen
                    (\def ->
                        if Dict.member def.name caModule.valueDefinitions then
                            errorTodo <| def.name ++ " declared twice!"

                        else
                            Ok { caModule | valueDefinitions = Dict.insert def.name def caModule.valueDefinitions }
                    )

        FA.Mutation _ ->
            errorTodo "Root Mutations are not allowed o_O"

        FA.TypeAlias fa ->
            -- TODO aliases need to be loaded on their own and applied to all types
            {-
               fa.type_
                   |> translateType
                   |> Result.map
                       (\translatedType ->
                           CA.TypeAlias
                               { name = fa.name
                               , args = fa.args
                               , type_ = translatedType
                               }
                       )
            -}
            errorTodo "aliases not yet implemented"

        FA.TypeDefinition fa ->
            if Dict.member fa.name caModule.typeDefinitions then
                errorTodo <| fa.name ++ " declared twice!"

            else
                let
                    translateConstructor : FA.TypeConstructor -> Res CA.TypeConstructor
                    translateConstructor faCons =
                        faCons.args
                            |> List.map translateType
                            |> listResultToResultList
                            |> Result.map
                                (\caArgs ->
                                    { name = faCons.name
                                    , args = caArgs
                                    }
                                )

                    consListToModule : List CA.TypeConstructor -> CA.Module ()
                    consListToModule consList =
                        { caModule
                            | typeDefinitions =
                                Dict.insert
                                    fa.name
                                    { name = fa.name
                                    , args = fa.args
                                    , constructors = consList
                                    }
                                    caModule.typeDefinitions
                        }
                in
                fa.constructors
                    |> List.map translateConstructor
                    |> listResultToResultList
                    |> Result.map consListToModule



----
--- Definition
--


translateDefinition : FA.DefinitionArgs -> Res (CA.ValueDefinition ())
translateDefinition fa =
    let
        (FA.PatternAny name) =
            fa.name

        resultMaybeAnnotation =
            fa.maybeAnnotation
                |> Maybe.map translateType
                |> maybeResultToResultMaybe

        resultBody =
            fa.body
                |> OneOrMore.toList
                |> List.map translateStatement
                |> listResultToResultList
    in
    Result.map2
        (\maybeAnnotation body ->
            { name = name
            , maybeAnnotation = maybeAnnotation
            , body = List.foldr wrapLambda body fa.parameters
            }
        )
        resultMaybeAnnotation
        resultBody



----
--- Statement
--


translateStatement : FA.Statement -> Res (CA.Statement ())
translateStatement faStat =
    case faStat of
        FA.Evaluation faExpr ->
            -- TODO Non-return, non-mutable, non-debug evaluations should produce an error.
            -- Debug evaluations should be optimized away in production build
            faExpr
                |> translateExpression
                |> Result.map CA.Evaluation

        FA.Definition fa ->
            fa
                |> translateDefinition
                |> Result.map CA.Definition

        FA.Mutation fa ->
            -- TODO!!
            errorTodo "Mutation not supported yet"

        FA.TypeAlias fa ->
            errorTodo "Aliases can be declared only in the root scope"

        FA.TypeDefinition fa ->
            errorTodo "Types can be declared only in the root scope"



----
--- Expression
--


translateExpression : FA.Expression -> Result Error (CA.Expression ())
translateExpression faExpr =
    case faExpr of
        FA.NumberLiteral args ->
            Ok <| CA.NumberLiteral () args

        FA.Variable args ->
            { start = args.start
            , end = args.end
            , name = args.variable

            -- TODO check that args.willBeMutated is on only if being used in an expression?
            -- Probably needs to be handled at the function call level
            }
                |> CA.Variable ()
                |> Ok

        FA.Lambda fa ->
            let
                ( FA.PatternAny paramsHead, paramsTail ) =
                    fa.parameters
            in
            fa.body
                |> OneOrMore.toList
                |> List.map translateStatement
                |> listResultToResultList
                |> Result.map
                    (\body ->
                        CA.Lambda ()
                            -- TODO start!
                            { start = 0
                            , parameter = paramsHead
                            , body = List.foldr wrapLambda body paramsTail
                            }
                    )

        FA.FunctionCall { reference, arguments } ->
            -- ref arg1 arg2 arg3...
            let
                fold : CA.Expression () -> CA.Expression () -> CA.Expression ()
                fold argument refAccum =
                    { reference = refAccum
                    , argument = argument

                    -- TODO
                    , argumentIsMutable = False
                    }
                        |> CA.Call ()
            in
            Result.map2
                (List.foldl fold)
                (translateExpression reference)
                (arguments
                    |> OneOrMore.toList
                    |> List.map translateExpression
                    |> listResultToResultList
                )

        FA.If { start, condition, true, false } ->
            Result.map3
                (\c t f ->
                    CA.If ()
                        { start = start
                        , condition = c
                        , true = t
                        , false = f
                        }
                )
                (translateExpression condition)
                (translateExpression true)
                (translateExpression false)

        FA.Tuple list ->
            case list of
                [ first, second ] ->
                    Result.map2
                        (\f s ->
                            CA.Record ()
                                [ { name = "first", value = f }
                                , { name = "second", value = s }
                                ]
                        )
                        (translateExpression first)
                        (translateExpression second)

                _ ->
                    errorTodo "sorry, I'm supporting only tuples of size 2"

        _ ->
            errorTodo "NOT SUPPORTED FOR NOW"



----
--- Type
--


translateAndCons : List FA.Type -> List CA.Type -> Res (List CA.Type)
translateAndCons faTypes caTypesAccum =
    case faTypes of
        [] ->
            Ok caTypesAccum

        faHead :: faTail ->
            case translateType faHead of
                Err e ->
                    Err e

                Ok caType ->
                    translateAndCons faTail (caType :: caTypesAccum)


addAttribute : List ( String, FA.Type ) -> List { name : String, type_ : CA.Type } -> Res CA.Type
addAttribute faAttrs caAttrsAccum =
    case faAttrs of
        [] ->
            caAttrsAccum
                |> CA.TypeRecord
                |> Ok

        ( name, faType ) :: faTail ->
            case translateType faType of
                Err e ->
                    -- TODO add `name` in the error?
                    Err e

                Ok caType ->
                    addAttribute faTail ({ name = name, type_ = caType } :: caAttrsAccum)


firstCharIsUpper : String -> Bool
firstCharIsUpper s =
    case String.uncons s of
        Nothing ->
            False

        Just ( head, tail ) ->
            Char.isUpper head


translateType : FA.Type -> Res CA.Type
translateType faType =
    case faType of
        FA.TypeConstantOrVariable { name } ->
            if firstCharIsUpper name then
                { name = name }
                    |> CA.TypeConstant
                    |> Ok

            else
                errorTodo "type vars not yet supported"

        FA.TypeFunction argsAndReturn ->
            -- translateAndCons will conveniently *reverse* the list, so that the return type is first
            case translateAndCons argsAndReturn [] of
                Err e ->
                    Err e

                Ok [] ->
                    errorTodo "This shouldn't happen #98765"

                Ok (return :: args) ->
                    Ok <| List.foldl (\arg accum -> CA.TypeFunction { from = arg, fromIsMutable = False, to = accum }) return args

        FA.TypePolymorphic { name, args } ->
            errorTodo "polymorphism not yet implemented"

        FA.TypeTuple types ->
            errorTodo "tuples are not supported. Use `pair` for a tuple-2, or a record instead."

        FA.TypeRecord attrs ->
            addAttribute attrs []

        FA.TypeMutable t ->
            {-
               ensure that this is being used only "in" a function call '?'
               add an arg to this function "can be mutable" '?'
            -}
            errorTodo "mutability not  yet implemented"



----
--- Helpers
--


wrapLambda : FA.Pattern -> List (CA.Statement ()) -> List (CA.Statement ())
wrapLambda (FA.PatternAny paramName) bodyAccum =
    -- TODO start?
    [ { start = 0
      , parameter = paramName
      , body = bodyAccum
      }
        |> CA.Lambda ()
        |> CA.Evaluation
    ]


errorTodo : String -> Res a
errorTodo s =
    Err
        { kind = Error.Whatever s
        , pos = -1
        }


listResultToResultList : List (Result e o) -> Result e (List o)
listResultToResultList =
    firstErrorRec [] >> Result.map List.reverse


firstErrorRec : List o -> List (Result e o) -> Result e (List o)
firstErrorRec os rs =
    case rs of
        [] ->
            Ok os

        (Err e) :: _ ->
            Err e

        (Ok o) :: rt ->
            firstErrorRec (o :: os) rt


maybeResultToResultMaybe : Maybe (Result e o) -> Result e (Maybe o)
maybeResultToResultMaybe maybeResult =
    case maybeResult of
        Nothing ->
            Ok Nothing

        Just (Ok something) ->
            Ok (Just something)

        Just (Err e) ->
            -- Unfortunately Elm doesn't realize that the two `Err e` have the same type
            -- Is it a limit of how pattern matching is implemented?
            Err e
