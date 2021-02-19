module Compiler.JsToString exposing (..)

import Dict exposing (Dict)
import Lib
import Set exposing (Set)
import Types.JavascriptAst as JA


id : Int -> String
id level =
    String.repeat level "  "


emitStatement : Int -> JA.Statement -> String
emitStatement l stat =
    let
        std mid expr =
            id l ++ mid ++ emitExpr l expr ++ ";"
    in
    case stat of
        JA.Eval e ->
            std "" e

        JA.Return e ->
            std "return " e

        JA.Define name e ->
            std ("const " ++ name ++ " = ") e

        JA.If condition block ->
            id l ++ "if (" ++ emitExpr l condition ++ ") " ++ emitBlock l block


emitBlock : Int -> List JA.Statement -> String
emitBlock l block =
    let
        lines =
            block
                |> List.map (emitStatement (l + 1))
                |> String.join "\n"
    in
    "{\n" ++ lines ++ "\n" ++ id l ++ "}"


{-| TODO reduce the number of parens?
-}
emitExpr : Int -> JA.Expr -> String
emitExpr l expression =
    case expression of
        JA.Literal s ->
            s

        JA.Var n ->
            n

        JA.Call ref args ->
            "(" ++ emitExpr l ref ++ ")(" ++ String.join ", " (List.map (emitExpr l) args) ++ ")"

        JA.Binop op left right ->
            "(" ++ emitExpr l left ++ " " ++ op ++ " " ++ emitExpr l right ++ ")"

        JA.Mutop op yield left right ->
            "(" ++ emitExpr l left ++ " " ++ op ++ " " ++ emitExpr l right ++ ", " ++ yield ++ ")"

        JA.SimpleLambda params expr ->
            "((" ++ String.join ", " params ++ ") => " ++ emitExpr l expr ++ ")"

        JA.BlockLambda params stats ->
            "((" ++ String.join ", " params ++ ") => " ++ emitBlock l stats ++ ")"

        JA.Record attrs ->
            if attrs == Dict.empty then
                "{}"

            else
                attrs
                    |> Dict.toList
                    |> List.sortBy Tuple.first
                    |> List.map (\( key, value ) -> id (l + 1) ++ key ++ ": " ++ emitExpr (l + 1) value ++ ",")
                    |> (\a -> "({\n" ++ String.join "\n" a ++ "\n" ++ id l ++ "})")

        JA.AccessWithDot name e ->
            emitExpr l e ++ "." ++ name

        JA.AccessWithBrackets i expr ->
            "(" ++ emitExpr l expr ++ ")[" ++ emitExpr l i ++ "]"

        JA.Conditional p true false ->
            ("(" ++ emitExpr l p ++ "\n")
                ++ (id (l + 1) ++ "? " ++ emitExpr (l + 1) true)
                ++ "\n"
                ++ (id (l + 1) ++ ": " ++ emitExpr (l + 1) false)
                ++ ")"

        JA.Array items ->
            if items == [] then
                "[]"

            else
                items
                    |> List.map (\i -> id (l + 1) ++ emitExpr (l + 1) i ++ ",")
                    |> (\a -> "([\n" ++ String.join "\n" a ++ "\n" ++ id l ++ "])")



--
