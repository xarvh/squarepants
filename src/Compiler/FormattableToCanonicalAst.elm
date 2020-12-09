module Compiler.FormattableToCanonicalAst exposing (..)

import OneOrMore
import Types.CanonicalAst as CA
import Types.Error as Error exposing (Error)
import Types.FormattableAst as FA


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


expression : FA.Expression -> Result Error CA.Expression
expression faExpr =
    case faExpr of
        FA.NumberLiteral args ->
            Ok <| CA.NumberLiteral args

        FA.Variable args ->
            Ok <| CA.Variable args

        FA.Lambda { start, parameters, body } ->
            -- fn x y z = expr -> fn x = fn y = fn z = expr
            let
                bodyExpression =
                    case body of
                        ( FA.Evaluate expr, [] ) ->
                            expr

                        _ ->
                            Debug.todo "STATEMENTS"

                fold : FA.Pattern -> CA.Expression -> CA.Expression
                fold (FA.PatternAny paramName) bodyAccum =
                    CA.Lambda { start = start, parameter = paramName, body = bodyAccum }
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
                fold : CA.Expression -> CA.Expression -> CA.Expression
                fold argument refAccum =
                    CA.Call { reference = refAccum, argument = argument }
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
                    CA.If
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
                            CA.Record
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


statement : FA.Statement -> Result Error CA.Statement
statement faStat =
    case faStat of
        FA.Definition { name, maybeAnnotation, parameters, body } ->
            case body of
                ( FA.Evaluate bodyExpression, [] ) ->
                    let
                        (FA.PatternAny n) =
                            name

                        fold : FA.Pattern -> CA.Expression -> CA.Expression
                        fold (FA.PatternAny paramName) bodyAccum =
                            -- TODO start?
                            CA.Lambda { start = 0, parameter = paramName, body = bodyAccum }
                    in
                    bodyExpression
                        |> expression
                        |> Result.map
                            (\expr ->
                                CA.Definition
                                    { name = n
                                    , maybeAnnotation = Maybe.map annotation maybeAnnotation
                                    , body = List.foldr fold expr parameters
                                    }
                            )

                _ ->
                    errorTodo "STATEMENT IS TOO COMPLICATED"

        _ ->
            errorTodo "STAT NOT SUPPORTED FOR NOW"


annotation : FA.TypeAnnotation -> CA.TypeAnnotation
annotation faAnn =
    { union = CA.TypeConstant { name = "TODO", args = [] }
    , isMutable = False
    }
