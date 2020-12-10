module Compiler.TypeInference_Test exposing (..)

import Compiler.TypeInference as TI
import Dict exposing (Dict)
import Test exposing (Test)
import Compiler.TestHelpers exposing (stringToCanonicalAst)


simpleTest =
    Test.simple Debug.toString




preamble : TI.Env
preamble =
    Dict.fromList
        [ ( "add", TI.Function (TI.Named "Number") (TI.Function (TI.Named "Number") (TI.Named "Number")) )
        , ( "not", TI.Function (TI.Named "Bool") (TI.Named "Bool") )
        , ( "True", TI.Named "Bool" )
        , ( "False", TI.Named "Bool" )
        ]


tests : List Test
tests =
    [ simpleTest
        { name =
            "Known function with correct params"
        , run =
            \_ ->
                "a = add 3 1"
                    |> stringToCanonicalAst
                    |> Result.andThen (Dict.get "a" >> Result.fromMaybe "Dict fail")
                    |> Result.andThen (TI.inferExpr 0 preamble)
                    |> Result.map (\( inferredType, subs, nextId ) -> inferredType)
        , expected =
            Ok <| TI.Named "Number"
        }
    , simpleTest
        { name =
            "Known function with wrong params"
        , run =
            \_ ->
                "a = add False"
                    |> stringToCanonicalAst
                    |> Result.andThen (Dict.get "a" >> Result.fromMaybe "Dict fail")
                    |> Result.andThen (TI.inferExpr 0 preamble)
                    |> Result.map (\( inferredType, subs, nextId ) -> inferredType)
        , expected =
            Err """Cannot match `Named "Number"` with `Named "Bool"`"""
        }
    , simpleTest
        { name =
            "Function inference 1"
        , run =
            \_ ->
                "a x = add x 1"
                    |> stringToCanonicalAst
                    |> Result.andThen (Dict.get "a" >> Result.fromMaybe "Dict fail")
                    |> Result.andThen (TI.inferExpr 0 preamble)
                    |> Result.map (\( inferredType, subs, nextId ) -> inferredType)
        , expected =
            Ok <| TI.Function (TI.Named "Number") (TI.Named "Number")
        }
    , simpleTest
        { name =
            "Function inference 2: same as 1, but with swapped args"
        , run =
            \_ ->
                "a x = add 1 x"
                    |> stringToCanonicalAst
                    |> Result.andThen (Dict.get "a" >> Result.fromMaybe "Dict fail")
                    |> Result.andThen (TI.inferExpr 0 preamble)
                    |> Result.map (\( inferredType, subs, nextId ) -> inferredType)
        , expected =
            Ok <| TI.Function (TI.Named "Number") (TI.Named "Number")
        }
    ]
