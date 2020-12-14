module Compiler.FormattableToCanonicalAst exposing (..)

import OneOrMore
import Types.CanonicalAst as CA
import Types.Error as Error exposing (Error)
import Types.FormattableAst as FA



----
--- Lib
--


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



---


type alias Res ok =
    Result Error ok


errorTodo : String -> Result Error a
errorTodo s =
    Err
        { kind = Error.Whatever s
        , pos = -1
        }


firstError : List o -> List (Result e o) -> Result e (List o)
firstError os rs =
    case rs of
        [] ->
            Ok os

        (Err e) :: _ ->
            Err e

        (Ok o) :: rt ->
            firstError (o :: os) rt


expression : FA.Expression -> Result Error (CA.Expression ())
expression faExpr =
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

        FA.Lambda { start, parameters, body } ->
            -- fn x y z = expr -> fn x = fn y = fn z = expr
            let
                bodyExpression =
                    case body of
                        ( FA.Evaluate expr, [] ) ->
                            expr

                        _ ->
                            Debug.todo "STATEMENTS"

                fold : FA.Pattern -> CA.Expression () -> CA.Expression ()
                fold (FA.PatternAny paramName) bodyAccum =
                    CA.Lambda () { start = start, parameter = paramName, body = bodyAccum }
            in
            bodyExpression
                |> expression
                |> Result.map
                    (\expr ->
                        parameters
                            |> OneOrMore.toList
                            |> List.foldr fold expr
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
                (expression reference)
                (arguments
                    |> OneOrMore.toList
                    |> List.map expression
                    |> firstError []
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
                (expression condition)
                (expression true)
                (expression false)

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
                        (expression first)
                        (expression second)

                _ ->
                    errorTodo "sorry, I'm supporting only tuples of size 2"

        _ ->
            errorTodo "NOT SUPPORTED FOR NOW"


statement : FA.Statement -> Result Error (CA.Statement ())
statement faStat =
    case faStat of
        FA.Definition { name, maybeAnnotation, parameters, body } ->
            case body of
                ( FA.Evaluate bodyExpression, [] ) ->
                    let
                        (FA.PatternAny n) =
                            name

                        fold : FA.Pattern -> CA.Expression () -> CA.Expression ()
                        fold (FA.PatternAny paramName) bodyAccum =
                            -- TODO start?
                            CA.Lambda () { start = 0, parameter = paramName, body = bodyAccum }
                    in
                    Result.map2
                        (\expr maybeAnn ->
                            CA.Definition
                                { name = n
                                , maybeAnnotation = maybeAnn
                                , body = List.foldr fold expr parameters
                                }
                        )
                        (expression bodyExpression)
                        (Maybe.map type_ maybeAnnotation |> maybeResultToResultMaybe)

                _ ->
                    errorTodo "STATEMENT IS TOO COMPLICATED"

        _ ->
            errorTodo "STAT NOT SUPPORTED FOR NOW"


type_ : FA.Type -> Res CA.Type
type_ faType =
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


translateAndCons : List FA.Type -> List CA.Type -> Res (List CA.Type)
translateAndCons faTypes caTypesAccum =
    case faTypes of
        [] ->
            Ok caTypesAccum

        faHead :: faTail ->
            case type_ faHead of
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
            case type_ faType of
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
