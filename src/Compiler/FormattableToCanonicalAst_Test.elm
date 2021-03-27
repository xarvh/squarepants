module Compiler.FormattableToCanonicalAst_Test exposing (..)

import Compiler.FormattableToCanonicalAst
import Compiler.TestHelpers
import Dict exposing (Dict)
import Set exposing (Set)
import Test exposing (Test)
import Types.CanonicalAst as CA exposing (Pos)
import Types.Literal as Literal


type alias Name =
    String


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
        , pipes
        , functions
        ]



----
---
--


simpleTest =
    Test.simple Debug.toString


codeTest =
    Test.codeTest Debug.toString


isOk =
    Test.isOk Debug.toString


hasError =
    Test.hasError Debug.toString


firstDefinition : String -> String -> Result String (CA.ValueDef ())
firstDefinition name code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Compiler.TestHelpers.resErrorToString
        |> Result.andThen (CA.findValue ("Test." ++ name) >> Result.fromMaybe "Dict fail")


firstEvaluation : String -> String -> Result String (CA.Expression ())
firstEvaluation name code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Compiler.TestHelpers.resErrorToString
        |> Result.andThen (CA.findValue ("Test." ++ name) >> Result.fromMaybe "findValue fail")
        |> Result.andThen (\def -> List.head def.body |> Result.fromMaybe "head fail")
        |> Result.andThen (asEvaluation >> Result.fromMaybe "asEval fail")


stringToCanonicalModule code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Compiler.TestHelpers.resErrorToString


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
            , run = \_ -> stringToCanonicalModule "union a = A"
            , test = Test.errorShouldContain "uppercase"
            }
        , hasError
            { name = "constructor names start with uppercase"
            , run = \_ -> stringToCanonicalModule "union A = a"
            , test = Test.errorShouldContain "constructor"
            }
        , hasError
            { name = "tuples op precedence"
            , run = \_ -> stringToCanonicalModule "union A = X Bool & Bool"
            , test = Test.errorShouldContain "operators"
            }
        , isOk
            { name = "tuples op precedence works with parens"
            , run = \_ -> stringToCanonicalModule "union A = X (Bool & Bool)"
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
                                { reference = CA.Variable () { name = "+", isRoot = True, attrPath = [] }
                                , argument = CA.ArgumentExpression (CA.Literal () (Literal.Number "3"))
                                }
                        , argument =
                            CA.ArgumentExpression
                                (CA.Call ()
                                    { reference =
                                        CA.Call ()
                                            { reference = CA.Variable () { name = "+", isRoot = True, attrPath = [] }
                                            , argument = CA.ArgumentExpression (CA.Literal () (Literal.Number "2"))
                                            }
                                    , argument = CA.ArgumentExpression (CA.Literal () (Literal.Number "1"))
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
                        { argument = CA.ArgumentExpression (CA.Literal () (Literal.Number "1"))
                        , reference =
                            CA.Call ()
                                { argument =
                                    CA.ArgumentExpression
                                        (CA.Call ()
                                            { argument = CA.ArgumentExpression (CA.Literal () (Literal.Number "2"))
                                            , reference =
                                                CA.Call ()
                                                    { argument = CA.ArgumentExpression (CA.Literal () (Literal.Number "3"))
                                                    , reference = CA.Variable () { name = "*", isRoot = True, attrPath = [] }
                                                    }
                                            }
                                        )
                                , reference = CA.Variable () { name = "+", isRoot = True, attrPath = [] }
                                }
                        }
                    )
            }
        , codeTest "functional notation"
            """
            a = (-)
            """
            (firstEvaluation "a")
            (Test.okEqual <|
                CA.Variable () { name = "-", isRoot = True, attrPath = [] }
            )
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
                        l : [ SPCore.Bool ]
                        l = l
                        """
            , expected =
                Ok
                    { body = [ CA.Evaluation (CA.Variable () { name = "Test.l", isRoot = True, attrPath = [] }) ]
                    , maybeAnnotation =
                        Just
                            (CA.TypeConstant
                                { ref = "SPCore.List"
                                , args = [ CA.TypeConstant { ref = "SPCore.Bool", args = [] } ]
                                }
                            )
                    , mutable = False
                    , pattern = CA.PatternAny "Test.l"
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
                                [ ( "first", CA.Literal () (Literal.Number "1") )
                                , ( "second", CA.Literal () (Literal.Number "2") )
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
                                [ ( "first", CA.Literal () (Literal.Number "1") )
                                , ( "second", CA.Literal () (Literal.Number "2") )
                                , ( "third", CA.Literal () (Literal.Number "3") )
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
                    { body = [ CA.Evaluation (CA.Variable () { name = "Test.a", attrPath = [], isRoot = True }) ]
                    , maybeAnnotation =
                        Just
                            (CA.TypeRecord
                                { attrs =
                                    Dict.fromList
                                        [ ( "first", CA.TypeConstant { args = [], ref = "Test.Blah" } )
                                        , ( "second", CA.TypeConstant { args = [], ref = "Test.Blah" } )
                                        ]
                                , extensible = Nothing
                                }
                            )
                    , mutable = False
                    , pattern = CA.PatternAny "Test.a"
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
        , reject "Blah.Blah.blah" "Constructor"
        , reject "blah.Blah.blah" "lower"
        , reject "Blah.blah.Blah" "lower"
        , reject "Blah..blah" "dot"
        , reject ".Blah" "shorthand"
        , reject ".blah.blah" "shorthand"
        , reject ".blah" "shorthand"
        , reject "..." ""
        , accept "x .. y"
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
                        [ ( "c", CA.Literal () (Literal.Number "1") )
                        , ( "b", CA.Variable () { attrPath = [], name = "Test.b", isRoot = True } )
                        ]
                , maybeUpdateTarget = Just { isRoot = True, attrPath = [], name = "Test.m" }
                }
                    |> CA.Record ()
                    |> Ok
            }
        , simpleTest
            { name = "update shorthand"
            , run = \_ -> firstEvaluation "b" "b = { a.k with y = .x }"
            , expected =
                { attrs = Dict.singleton "y" (CA.Variable () { isRoot = True, attrPath = [ "k", "x" ], name = "Test.a" })
                , maybeUpdateTarget = Just { isRoot = True, attrPath = [ "k" ], name = "Test.a" }
                }
                    |> CA.Record ()
                    |> Ok
            }
        , codeTest "annotation, extensible"
            """
            a : { b with x : Bool }
            a = a
            """
            (firstEvaluation "a")
            (Test.errContain "disabled")
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
        , codeTest "[reg] record patterns are NOT extensible"
            """
            a =
              { b with c } = d
            """
            (firstEvaluation "a")
            (Test.errContain "with")
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



----
--- Pipes
--


pipes : Test
pipes =
    Test.Group "Pipes"
        [ codeTest "sendLeft is inlined"
            """
            a = thing >> function
            """
            (firstEvaluation "a")
            (Test.okEqual <|
                CA.Call ()
                    { reference = CA.Variable () { name = "Test.function", isRoot = True, attrPath = [] }
                    , argument = CA.ArgumentExpression <| CA.Variable () { name = "Test.thing", isRoot = True, attrPath = [] }
                    }
            )
        , codeTest "sendRight is inlined"
            """
            a = function << thing
            """
            (firstEvaluation "a")
            (Test.okEqual <|
                CA.Call ()
                    { reference = CA.Variable () { name = "Test.function", isRoot = True, attrPath = [] }
                    , argument = CA.ArgumentExpression <| CA.Variable () { name = "Test.thing", isRoot = True, attrPath = [] }
                    }
            )
        ]



----
--- Functions
--


functions : Test
functions =
    Test.Group "Functions"
        [ codeTest "[rec] lambda with two arguments"
            """
            f =
              fn a b = 1
            """
            (firstEvaluation "f")
            Test.justOk
        ]
