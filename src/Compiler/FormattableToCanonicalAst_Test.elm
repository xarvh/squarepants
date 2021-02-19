module Compiler.FormattableToCanonicalAst_Test exposing (..)

import Compiler.FormattableToCanonicalAst
import Compiler.TestHelpers
import Dict exposing (Dict)
import Set exposing (Set)
import Test exposing (Test)
import Types.CanonicalAst as CA exposing (Name)
import Types.Literal as Literal


tests : Test
tests =
    Test.Group "FormattableToCanonicalAst"
        [ unionTypes
        , binops
        , tuples
        , lists
        , moduleAndAttributePaths
        , records
        , patterns
        , annotations
        ]



----
---
--


simpleTest =
    Test.simple Debug.toString


isOk =
    Test.isOk Debug.toString


hasError =
    Test.hasError Debug.toString


firstDefinition : String -> String -> Result String (CA.ValueDef ())
firstDefinition name code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Compiler.TestHelpers.resultErrorToString code
        |> Result.andThen (CA.findValue name >> Result.fromMaybe "Dict fail")


firstEvaluation : String -> String -> Result String (CA.Expression ())
firstEvaluation name code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Compiler.TestHelpers.resultErrorToString code
        |> Result.andThen (CA.findValue name >> Result.fromMaybe "findValue fail")
        |> Result.andThen (\def -> List.head def.body |> Result.fromMaybe "head fail")
        |> Result.andThen (asEvaluation >> Result.fromMaybe "asEval fail")


stringToCanonicalModule code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Compiler.TestHelpers.resultErrorToString code


asEvaluation : CA.Statement e -> Maybe (CA.Expression e)
asEvaluation s =
    case s of
        CA.Evaluation expr ->
            Just expr

        _ ->
            Nothing



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
        , hasError
            { name = "tuples op precedence"
            , run = \_ -> stringToCanonicalModule "type A = X Bool & Bool"
            , test = Test.errorShouldContain "operators"
            }
        , isOk
            { name = "tuples op precedence works with parens"
            , run = \_ -> stringToCanonicalModule "type A = X (Bool & Bool)"
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
                                , argument = CA.ArgumentExpression (CA.Literal () { end = 13, value = Literal.Number "3", start = 12 })
                                }
                        , argument =
                            CA.ArgumentExpression
                                (CA.Call ()
                                    { reference =
                                        CA.Call ()
                                            { reference = CA.Variable () { end = 0, path = "+", start = 0, attrPath = [] }
                                            , argument = CA.ArgumentExpression (CA.Literal () { end = 9, value = Literal.Number "2", start = 8 })
                                            }
                                    , argument = CA.ArgumentExpression (CA.Literal () { end = 5, value = Literal.Number "1", start = 4 })
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
                        { argument = CA.ArgumentExpression (CA.Literal () { end = 5, value = Literal.Number "1", start = 4 })
                        , reference =
                            CA.Call ()
                                { argument =
                                    CA.ArgumentExpression
                                        (CA.Call ()
                                            { argument = CA.ArgumentExpression (CA.Literal () { end = 9, value = Literal.Number "2", start = 8 })
                                            , reference =
                                                CA.Call ()
                                                    { argument = CA.ArgumentExpression (CA.Literal () { end = 13, value = Literal.Number "3", start = 12 })
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
--- Lists
--


lists : Test
lists =
    Test.Group "Lists"
        [ simpleTest
            { name = "list type sugar"
            , run =
                \_ ->
                    firstDefinition "l"
                        """
                        l : [ Bool ]
                        l = l
                        """
            , expected =
                Ok
                    { body = [ CA.Evaluation (CA.Variable () { end = 19, path = "l", start = 18, attrPath = [] }) ]
                    , maybeAnnotation =
                        Just
                            (CA.TypeConstant
                                { path = "List"
                                , args = [ CA.TypeConstant { path = "Bool", args = [] } ]
                                }
                            )
                    , mutable = False
                    , pattern = CA.PatternAny "l"
                    }
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
                        { maybeUpdateTarget = Nothing
                        , attrs =
                            Dict.fromList
                                [ ( "first", CA.Literal () { end = 5, value = Literal.Number "1", start = 4 } )
                                , ( "second", CA.Literal () { end = 9, value = Literal.Number "2", start = 8 } )
                                ]
                        }
                    )
            }
        , simpleTest
            { name = "tuple3"
            , run = \_ -> firstEvaluation "a" "a = 1 & 2 & 3"
            , expected =
                Ok
                    (CA.Record ()
                        { maybeUpdateTarget = Nothing
                        , attrs =
                            Dict.fromList
                                [ ( "first", CA.Literal () { end = 5, value = Literal.Number "1", start = 4 } )
                                , ( "second", CA.Literal () { end = 9, value = Literal.Number "2", start = 8 } )
                                , ( "third", CA.Literal () { end = 13, value = Literal.Number "3", start = 12 } )
                                ]
                        }
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
                                    Dict.fromList
                                        [ ( "first", CA.TypeConstant { args = [], path = "Blah" } )
                                        , ( "second", CA.TypeConstant { args = [], path = "Blah" } )
                                        ]
                                , extensible = Nothing
                                }
                            )
                    , mutable = False
                    , pattern = CA.PatternAny "a"
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
        , reject ".." "shorthand"
        , reject "Blah..blah" "dot"
        , reject ".Blah" "shorthand"
        , reject ".blah.blah" "shorthand"
        , reject ".blah" "shorthand"
        ]



----
--- Records
--


records : Test
records =
    let
        accept s =
            isOk { name = s, run = \_ -> firstDefinition "a" ("a = " ++ s) }

        reject s m =
            hasError { name = s, run = \_ -> firstDefinition "a" ("a = " ++ s), test = Test.errorShouldContain m }
    in
    Test.Group "Records"
        [ simpleTest
            { name = "functional update"
            , run = \_ -> firstEvaluation "a" "a = { m with b, c = 1 }"
            , expected =
                { attrs =
                    Dict.fromList
                        [ ( "c", CA.Literal () { end = 21, value = Literal.Number "1", start = 20 } )
                        , ( "b", CA.Variable () { attrPath = [], end = 0, path = "b", start = 0 } )
                        ]
                , maybeUpdateTarget = Just { attrPath = [], end = 7, path = "m", start = 6 }
                }
                    |> CA.Record ()
                    |> Ok
            }
        , simpleTest
            { name = "update shorthand"
            , run = \_ -> firstEvaluation "b" "b = { a.k with y = .x }"
            , expected =
                { attrs = Dict.singleton "y" (CA.Variable () { attrPath = [ "k", "x" ], end = 21, path = "a", start = 19 })
                , maybeUpdateTarget = Just { attrPath = [ "k" ], end = 9, path = "a", start = 6 }
                }
                    |> CA.Record ()
                    |> Ok
            }
        ]



----
--- Pattern
--


patterns : Test
patterns =
    Test.Group "Patterns"
        [ hasError
            { name = "can't declare functions inside patterns "
            , run =
                \_ ->
                    """
                    x =
                      c (a b) = 2
                    """
                        |> firstEvaluation "x"
            , test = Test.errorShouldContain "function"
            }
        ]



----
--- Annotations
--


annotations : Test
annotations =
    Test.Group "Annotations"
        [ hasError
            { name = "annotation mutability must match definition's"
            , run =
                \_ ->
                    """
                    a : Number
                    a @= 3
                    """
                        |> firstEvaluation "a"
            , test = Test.errorShouldContain "mutability"
            }
        , hasError
            { name = "annotation mutability must match definition's"
            , run =
                \_ ->
                    """
                    a : Number
                    b = 3
                    """
                        |> firstEvaluation "b"
            , test = Test.errorShouldContain "name"
            }
        ]
