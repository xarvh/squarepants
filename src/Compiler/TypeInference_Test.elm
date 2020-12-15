module Compiler.TypeInference_Test exposing (..)

import Compiler.TestHelpers exposing (stringToCanonicalModule)
import Compiler.TypeInference as TI
import Dict exposing (Dict)
import Set exposing (Set)
import Test exposing (Test)
import Types.CanonicalAst as CA


simpleTest =
    Test.simple Debug.toString


isOk =
    Test.isOk Debug.toString


constant n =
    CA.TypeConstant { name = n }


function from to =
    CA.TypeFunction { from = from, fromIsMutable = False, to = to }



-- preamble : TI.Env


preamble =
    let
        em x =
            ( x, Set.empty )
    in
    Dict.fromList
        [ ( "add", em <| function (constant "Number") (function (constant "Number") (constant "Number")) )
        , ( "not", em <| function (constant "Bool") (constant "Bool") )
        , ( "True", em <| constant "Bool" )
        , ( "False", em <| constant "Bool" )
        ]


tests : List Test
tests =
    [ simpleTest
        { name =
            "Known function with correct params"
        , run =
            \_ ->
                "a = add 3 1"
                    |> stringToCanonicalModule
                    |> Result.andThen (TI.inspectModule preamble)
                    |> Result.andThen (Dict.get "a" >> Result.fromMaybe "Dict fail")
        , expected =
            Ok <| ( constant "Number", Set.empty )
        }
    , simpleTest
        { name =
            "Known function with wrong params"
        , run =
            \_ ->
                "a = add False"
                    |> stringToCanonicalModule
                    |> Result.andThen (TI.inspectModule preamble)
                    |> Result.andThen (Dict.get "a" >> Result.fromMaybe "Dict fail")
        , expected =
            Err """cannot unify Bool and Number"""
        }
    , simpleTest
        { name =
            "Function inference 1"
        , run =
            \_ ->
                "a x = add x 1"
                    |> stringToCanonicalModule
                    |> Result.andThen (TI.inspectModule preamble)
                    |> Result.andThen (Dict.get "a" >> Result.fromMaybe "Dict fail")
        , expected =
            Ok <| ( function (constant "Number") (constant "Number"), Set.empty )
        }
    , simpleTest
        { name =
            "Function inference 2: same as 1, but with swapped args"
        , run =
            \_ ->
                "a x = add 1 x"
                    |> stringToCanonicalModule
                    |> Result.andThen (TI.inspectModule preamble)
                    |> Result.andThen (Dict.get "a" >> Result.fromMaybe "Dict fail")
        , expected =
            Ok <| ( function (constant "Number") (constant "Number"), Set.empty )
        }

    ----
    --- Statements
    --
    , simpleTest
        { name =
            "Statement blocks should return the last statement's type"
        , run =
            \_ ->
                """
                a =
                  3
                  False
                """
                    |> stringToCanonicalModule
                    |> Result.andThen (TI.inspectModule preamble)
                    |> Result.andThen (Dict.get "a" >> Result.fromMaybe "Dict fail")
        , expected =
            Ok <| ( constant "Bool", Set.empty )
        }
    , simpleTest
        { name =
            "Definition statement return type None"
        , run =
            \_ ->
                """
                a =
                  f x = 3
                """
                    |> stringToCanonicalModule
                    |> Result.andThen (TI.inspectModule preamble)
                    |> Result.andThen (Dict.get "a" >> Result.fromMaybe "Dict fail")
        , expected =
            Ok <| ( constant "None", Set.empty )
        }

    ----
    --- variable types
    --
    , isOk
        { name =
            "TyVar definitions: lambda scope"
        , run =
            \_ ->
                """
                a b =
                  f x = x
                  f 3
                  f False
                """
                    |> stringToCanonicalModule
                    |> Result.andThen (TI.inspectModule preamble)
                    |> Result.andThen (Dict.get "a" >> Result.fromMaybe "Dict fail")
        }
    , isOk
        { name =
            "TyVar definitions: non-lambda scope"
        , run =
            \_ ->
                """
                a =
                  f x = x
                  f 3
                  f False
                """
                    |> stringToCanonicalModule
                    |> Result.andThen (TI.inspectModule preamble)
                    |> Result.andThen (Dict.get "a" >> Result.fromMaybe "Dict fail")
        }
    , isOk
        { name =
            "TyVar definitions: root scope"
        , run =
            \_ ->
                """
                a x = x
                g =
                  a 3
                  a False
                """
                    |> stringToCanonicalModule
                    |> Result.andThen (TI.inspectModule preamble)
                    |> Result.andThen (Dict.get "a" >> Result.fromMaybe "Dict fail")
        }
    , simpleTest
        { name =
            {- This error happens only when the identity function (`b`) follows alphabetically
               the definition that references it.
               Just to be sure, I've added another test below that is identical to this one
               with the only difference that `a` is renamed to `c`, and it passes.
            -}
            "Type inference regression 1: `a` was variable type instead than number"
        , run =
            \_ ->
                """
                b x = x
                a = b 1
                """
                    |> stringToCanonicalModule
                    |> Result.andThen (TI.inspectModule preamble)
                    |> Result.andThen (Dict.get "a" >> Result.fromMaybe "Dict fail")
        , expected =
            Ok <| ( constant "Number", Set.empty )
        }
    , simpleTest
        { name =
            -- See note for the test above!
            "Type inference regression 2: Just make sure that `c` works"
        , run =
            \_ ->
                """
                b x = x
                c = b 1
                """
                    |> stringToCanonicalModule
                    |> Result.andThen (TI.inspectModule preamble)
                    |> Result.andThen (Dict.get "c" >> Result.fromMaybe "Dict fail")
        , expected =
            Ok <| ( constant "Number", Set.empty )
        }
    ]
