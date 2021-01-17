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


firstDefinition : String -> String -> Result String (CA.ValueDefinition ())
firstDefinition name code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Result.andThen (\mod -> Dict.get name mod.valueDefinitions |> Result.fromMaybe "Dict fail")


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
        , tuples
        , moduleAndAttributePaths
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
            , run = \_ -> firstEvaluation "a" "a = 1 + 2 + 3"
            , expected =
                Ok
                    (CA.Call ()
                        { reference =
                            CA.Call ()
                                { reference = CA.Variable () { end = 0, path = "+", start = 0, attrPath = [] }
                                , argument = CA.ArgumentExpression (CA.NumberLiteral () { end = 13, number = "3", start = 12 })
                                }
                        , argument =
                            CA.ArgumentExpression
                                (CA.Call ()
                                    { reference =
                                        CA.Call ()
                                            { reference = CA.Variable () { end = 0, path = "+", start = 0, attrPath = [] }
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
            , run = \_ -> firstEvaluation "a" "a = 1 + 2 * 3"
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
                                                    , reference = CA.Variable () { end = 0, path = "*", start = 0, attrPath = [] }
                                                    }
                                            }
                                        )
                                , reference = CA.Variable () { end = 0, path = "+", start = 0, attrPath = [] }
                                }
                        }
                    )
            }
        ]



----
--- Tuples
--


tuples : Test
tuples =
    Test.Group "Tuples"
        [ simpleTest
            { name = "tuple2"
            , run = \_ -> firstEvaluation "a" "a = 1 & 2"
            , expected =
                Ok
                    (CA.Record ()
                        [ { name = "first", value = CA.NumberLiteral () { end = 5, number = "1", start = 4 } }
                        , { name = "second", value = CA.NumberLiteral () { end = 9, number = "2", start = 8 } }
                        ]
                    )
            }
        , simpleTest
            { name = "tuple3"
            , run = \_ -> firstEvaluation "a" "a = 1 & 2 & 3"
            , expected =
                Ok
                    (CA.Record ()
                        [ { name = "first", value = CA.NumberLiteral () { end = 5, number = "1", start = 4 } }
                        , { name = "second", value = CA.NumberLiteral () { end = 9, number = "2", start = 8 } }
                        , { name = "third", value = CA.NumberLiteral () { end = 13, number = "3", start = 12 } }
                        ]
                    )
            }
        , hasError
            { name = "tuple4"
            , run = \_ -> firstEvaluation "a" "a = 1 & 2 & 3 & 4"
            , test = Test.errorShouldContain "use a record"
            }
        , simpleTest
            { name = "tuple2 type"
            , run =
                \_ ->
                    firstDefinition "a"
                        """
                        a : Blah & Blah
                        a = a
                        """
            , expected =
                Ok
                    { body = [ CA.Evaluation (CA.Variable () { end = 22, path = "a", start = 21, attrPath = [] }) ]
                    , maybeAnnotation =
                        Just
                            (CA.TypeRecord
                                { attrs =
                                    [ { name = "first", type_ = CA.TypeConstant { args = [], path = "Blah" } }
                                    , { name = "second", type_ = CA.TypeConstant { args = [], path = "Blah" } }
                                    ]
                                , extensible = Nothing
                                }
                            )
                    , mutable = False
                    , name = "a"
                    }
            }
        , hasError
            { name = "tuple4, type"
            , run =
                \_ ->
                    firstDefinition "a"
                        """
                        a : Blah & Blah & Blah & Blah
                        a = a
                        """
            , test = Test.errorShouldContain "Use a record"
            }
        ]



----
--- Module and Attribute Paths
--


moduleAndAttributePaths : Test
moduleAndAttributePaths =
    let
        accept s =
            isOk { name = s, run = \_ -> firstDefinition "a" ("a = " ++ s) }

        reject s m =
            hasError { name = s, run = \_ -> firstDefinition "a" ("a = " ++ s), test = Test.errorShouldContain m }
    in
    Test.Group "Module and Attribute Paths"
        [ accept "blah.blah.blah"
        , accept "Blah.Blah.blah"
        , reject "blah.Blah.blah" "lower"
        , reject "Blah.blah.Blah" "lower"
        , reject ".." "dot"
        , reject "Blah..blah" "dot"
        , reject ".Blah" "lower"
        , reject ".blah.blah" "dot"
        , reject ".blah" "NI"
        ]
