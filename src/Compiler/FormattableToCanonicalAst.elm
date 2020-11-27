module Compiler.FormattableToCanonicalAst exposing (..)

import OneOrMore
import Types.CanonicalAst as CA
import Types.FormattableAst as FA


expression : FA.Expression -> CA.Expression
expression faExpr =
    case faExpr of
        FA.NumberLiteral args ->
            CA.NumberLiteral args

        FA.Variable args ->
            CA.Variable args

        FA.Lambda { start, parameters, body } ->
            -- fn x y z = expr -> fn x = fn y = fn z = expr
            let
                bodyExpression =
                    case body of
                        ( FA.Evaluate expr, [] ) ->
                            expr

                        ( FA.Return expr, [] ) ->
                            expr

                        _ ->
                            Debug.todo "STATEMENTS"

                fold : FA.Pattern -> CA.Expression -> CA.Expression
                fold (FA.PatternAny paramName) bodyAccum =
                    CA.Lambda { start = start, parameter = paramName, body = bodyAccum }
            in
            parameters
                |> OneOrMore.toList
                |> List.foldr fold (expression bodyExpression)

        FA.FunctionCall { reference, arguments } ->
            -- ref arg1 arg2 arg3...
            let
                fold : FA.Expression -> CA.Expression -> CA.Expression
                fold argument refAccum =
                    CA.Call { reference = refAccum, argument = expression argument }
            in
            arguments
                |> OneOrMore.toList
                |> List.foldl fold (expression reference)

        FA.If_Functional { start, condition, true, false } ->
            CA.If
                { start = start
                , condition = expression condition
                , true = expression condition
                , false = expression condition
                }

        FA.Tuple2 { first, second } ->
            CA.Tuple2
                { first = expression first
                , second = expression second
                }

        _ ->
            Debug.todo "NOT SUPPORTED FOR NOW"


statement : FA.Statement -> CA.Statement
statement faStat =
    case faStat of
        FA.Definition { name, parameters, body } ->
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
                    CA.Definition
                        { name = n
                        , body = List.foldr fold (expression bodyExpression) parameters
                        }

                _ ->
                    Debug.todo "STATEMENT IS TOO COMPLICATED"

        _ ->
            Debug.todo "STAT NOT SUPPORTED FOR NOW"
