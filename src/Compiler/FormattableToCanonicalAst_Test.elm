module Compiler.FormattableToCanonicalAst_Test exposing (..)

import Compiler.FormattableToCanonicalAst
import Compiler.TestHelpers exposing (stringToCanonicalModule)
import Dict exposing (Dict)
import Set exposing (Set)
import Test exposing (Test)
import Types.CanonicalAst as CA exposing (Name)


simpleTest =
    Test.simple Debug.toString


isOk =
    Test.isOk Debug.toString


hasError =
    Test.hasError Debug.toString


firstEvaluation : String -> String -> Result String (CA.Expression ())
firstEvaluation name code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Result.andThen (\mod -> Dict.get name mod.valueDefinitions |> Result.fromMaybe "Dict fail")
        |> Result.andThen (\def -> List.head def.body |> Result.fromMaybe "head fail")
        |> Result.andThen (asEvaluation >> Result.fromMaybe "asEval fail")


asEvaluation : CA.Statement e -> Maybe (CA.Expression e)
asEvaluation s =
    case s of
        CA.Evaluation expr ->
            Just expr

        _ ->
            Nothing



----
---
--


tests : Test
tests =
    Test.Group "FormattableToCanonicalAst"
        [ unionTypes
        , binops
        ]



----
--- Union Types
--


unionTypes : Test
unionTypes =
    Test.Group "unionTypes"
        [ hasError
            { name = "name starts with uppercase"
            , run = \_ -> stringToCanonicalModule "type a = A"
            , test = Test.errorShouldContain "uppercase"
            }
        , hasError
            { name = "constructor names start with uppercase"
            , run = \_ -> stringToCanonicalModule "type A = a"
            , test = Test.errorShouldContain "constructor"
            }
        ]


binops : Test
binops =
    Test.Group "Binops"
        [ simpleTest
            { name = "left-association"
            , run =
                \_ ->
                    firstEvaluation "a" "a = 1 + 2 + 3"
            , expected =
                Ok
                    (CA.Call ()
                        { reference =
                            CA.Call ()
                                { reference = CA.Variable () { end = 0, name = "+", start = 0 }
                                , argument = CA.ArgumentExpression (CA.NumberLiteral () { end = 13, number = "3", start = 12 })
                                }
                        , argument =
                            CA.ArgumentExpression
                                (CA.Call ()
                                    { reference =
                                        CA.Call ()
                                            { reference = CA.Variable () { end = 0, name = "+", start = 0 }
                                            , argument = CA.ArgumentExpression (CA.NumberLiteral () { end = 9, number = "2", start = 8 })
                                            }
                                    , argument = CA.ArgumentExpression (CA.NumberLiteral () { end = 5, number = "1", start = 4 })
                                    }
                                )
                        }
                    )
            }
        , simpleTest
            { name = "precedence"
            , run =
                \_ ->
                    firstEvaluation "a" "a = 1 + 2 * 3"
            , expected =
                Ok
                    (CA.Call ()
                        { argument = CA.ArgumentExpression (CA.NumberLiteral () { end = 5, number = "1", start = 4 })
                        , reference =
                            CA.Call ()
                                { argument =
                                    CA.ArgumentExpression
                                        (CA.Call ()
                                            { argument = CA.ArgumentExpression (CA.NumberLiteral () { end = 9, number = "2", start = 8 })
                                            , reference =
                                                CA.Call ()
                                                    { argument = CA.ArgumentExpression (CA.NumberLiteral () { end = 13, number = "3", start = 12 })
                                                    , reference = CA.Variable () { end = 0, name = "*", start = 0 }
                                                    }
                                            }
                                        )
                                , reference = CA.Variable () { end = 0, name = "+", start = 0 }
                                }
                        }
                    )
            }
        ]
