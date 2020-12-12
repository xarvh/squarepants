module Compiler.TypeInference_Test exposing (..)

import Compiler.TestHelpers exposing (stringToCanonicalStatements)
import Compiler.TypeInference as TI
import Dict exposing (Dict)
import Test exposing (Test)


simpleTest =
    Test.simple Debug.toString


constant n =
    TI.TypeConstant { name = n }


function from to =
    TI.TypeFunction { from = from, to = to }


preamble : TI.Env
preamble =
    Dict.fromList
        [ ( "add", function (constant "Number") (function (constant "Number") (constant "Number")) )
        , ( "not", function (constant "Bool") (constant "Bool") )
        , ( "True", constant "Bool" )
        , ( "False", constant "Bool" )
        ]


tests : List Test
tests =
    [ simpleTest
        { name =
            "Known function with correct params"
        , run =
            \_ ->
                "a = add 3 1"
                    |> stringToCanonicalStatements
                    |> Result.andThen (Dict.get "a" >> Result.fromMaybe "Dict fail")
                    |> Result.andThen (TI.inferExpr 0 preamble)
                    |> Result.map (\( inferredType, subs, nextId ) -> inferredType)
        , expected =
            Ok <| constant "Number"
        }
    , simpleTest
        { name =
            "Known function with wrong params"
        , run =
            \_ ->
                "a = add False"
                    |> stringToCanonicalStatements
                    |> Result.andThen (Dict.get "a" >> Result.fromMaybe "Dict fail")
                    |> Result.andThen (TI.inferExpr 0 preamble)
                    |> Result.map (\( inferredType, subs, nextId ) -> inferredType)
        , expected =
            Err """Cannot match `TypeConstant { name = "Number" }` with `TypeConstant { name = "Bool" }`"""
        }
    , simpleTest
        { name =
            "Function inference 1"
        , run =
            \_ ->
                "a x = add x 1"
                    |> stringToCanonicalStatements
                    |> Result.andThen (Dict.get "a" >> Result.fromMaybe "Dict fail")
                    |> Result.andThen (TI.inferExpr 0 preamble)
                    |> Result.map (\( inferredType, subs, nextId ) -> inferredType)
        , expected =
            Ok <| function (constant "Number") (constant "Number")
        }
    , simpleTest
        { name =
            "Function inference 2: same as 1, but with swapped args"
        , run =
            \_ ->
                "a x = add 1 x"
                    |> stringToCanonicalStatements
                    |> Result.andThen (Dict.get "a" >> Result.fromMaybe "Dict fail")
                    |> Result.andThen (TI.inferExpr 0 preamble)
                    |> Result.map (\( inferredType, subs, nextId ) -> inferredType)
        , expected =
            Ok <| function (constant "Number") (constant "Number")
        }
    ]
