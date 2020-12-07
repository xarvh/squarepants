module Compiler.TypeInference_Test exposing (..)

import Compiler.FormattableToCanonicalAst
import Compiler.StringToTokens
import Compiler.TokensToFormattableAst
import Compiler.TypeInference as TI
import Dict exposing (Dict)
import Test exposing (Test)
import Types.CanonicalAst as CA
import Types.Error
import Types.FormattableAst as FA


simpleTest =
    Test.simple Debug.toString


insertStatement : FA.Statement -> Dict String CA.Expression -> Dict String CA.Expression
insertStatement statement scope =
    case Compiler.FormattableToCanonicalAst.statement statement of
        CA.Definition { name, body } ->
            Dict.insert name body scope

        _ ->
            scope


stringToCanonicalAst : String -> Result String (Dict String CA.Expression)
stringToCanonicalAst code =
    code
        |> Compiler.StringToTokens.lexer
        |> Result.andThen Compiler.TokensToFormattableAst.parse
        |> Result.map (List.foldl insertStatement Dict.empty)
        |> Result.mapError (Types.Error.toString code)


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
