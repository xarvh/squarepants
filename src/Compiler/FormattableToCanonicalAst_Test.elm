module Compiler.FormattableToCanonicalAst_Test exposing (..)

import Compiler.FormattableToCanonicalAst
import Compiler.TestHelpers exposing (p)
import Dict exposing (Dict)
import Set exposing (Set)
import Test exposing (Test)
import Types.CanonicalAst as CA
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


codeTest =
    Test.codeTest Debug.toString


hasError { name, code, run, test } =
    Test.codeTest Debug.toString name code run test


firstDefinition : String -> String -> Result String CA.RootValueDef
firstDefinition name code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Compiler.TestHelpers.resErrorToString code
        |> Result.andThen (CA.findValue ("Test." ++ name) >> Result.fromMaybe "Dict fail")


firstEvaluation : String -> String -> Result String CA.Expression
firstEvaluation name code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Compiler.TestHelpers.resErrorToString code
        |> Result.andThen (CA.findValue ("Test." ++ name) >> Result.fromMaybe "findValue fail")
        |> Result.andThen (\def -> List.head def.body |> Result.fromMaybe "head fail")
        |> Result.andThen (asEvaluation >> Result.fromMaybe "asEval fail")


stringToCanonicalModule code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Compiler.TestHelpers.resErrorToString code


asEvaluation : CA.Statement -> Maybe CA.Expression
asEvaluation s =
    case s of
        CA.Evaluation expr ->
            Just expr

        _ ->
            Nothing


{-| TODO move this to Helpers?
-}
transformAB : String -> Result String ( CA.RootValueDef, CA.RootValueDef )
transformAB code =
    let
        findAB mod =
            Maybe.map2
                Tuple.pair
                (CA.findValue "Test.a" mod)
                (CA.findValue "Test.b" mod)
    in
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Compiler.TestHelpers.resErrorToString code
        |> Result.andThen (findAB >> Result.fromMaybe "findAB fail")


shouldHaveSameAB : (ab -> c) -> Test.CodeExpectation ( ab, ab )
shouldHaveSameAB getter =
    Test.freeform <| \( a, b ) ->
    if getter a == getter b then
        Nothing

    else
        [ "The two don't match:"
        , Debug.toString (getter a)
        , Debug.toString (getter b)
        ]
            |> String.join "\n"
            |> Just


transformABC : String -> Result String ( CA.RootValueDef, CA.RootValueDef, CA.RootValueDef )
transformABC code =
    let
        findABC mod =
            Maybe.map3
                (\a b c -> ( a, b, c ))
                (CA.findValue "Test.a" mod)
                (CA.findValue "Test.b" mod)
                (CA.findValue "Test.c" mod)
    in
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Compiler.TestHelpers.resErrorToString code
        |> Result.andThen (findABC >> Result.fromMaybe "findABC fail")



----
--- Union Types
--


unionTypes : Test
unionTypes =
    Test.Group "unionTypes"
        [ hasError
            { name = "name starts with uppercase"
            , code = "union a = A"
            , run = stringToCanonicalModule
            , test = Test.errContain "uppercase"
            }
        , hasError
            { name = "constructor names start with uppercase"
            , code = "union A = a"
            , run = stringToCanonicalModule
            , test = Test.errContain "constructor"
            }
        , hasError
            { name = "tuples op precedence"
            , code = "union A = X Bool & Bool"
            , run = stringToCanonicalModule
            , test = Test.errContain "operators"
            }
        , codeTest "tuples op precedence works with parens"
            "union A = X (Bool & Bool)"
            stringToCanonicalModule
            Test.isOk
        ]


binops : Test
binops =
    Test.Group "Binops"
        [ codeTest "left associativity"
            """
            a = v >> f >> g
            b = (v >> f) >> g
            """
            transformAB
            (shouldHaveSameAB .body)
        , codeTest "right associativity"
            """
            a = v :: f :: g
            b = v :: (f :: g)
            """
            transformAB
            (shouldHaveSameAB .body)
        , codeTest "precedence"
            """
            a = 1 + 2 * 3 + 4
            b = 1 + (2 * 3) + 4
            """
            transformAB
            (shouldHaveSameAB .body)
        , codeTest "functional notation"
            """
            a = (-)
            """
            (firstEvaluation "a")
            (Test.okEqual <|
                CA.Variable p { name = "-", isRoot = True, attrPath = [] }
            )
        ]



----
--- Lists
--


lists : Test
lists =
    Test.Group "Lists"
        [ codeTest "list type sugar"
            """
                        l =
                          as [ SPCore.Bool ]
                          l
                        """
            (firstDefinition "l")
            (Test.okEqual
                { body = [ CA.Evaluation (CA.Variable p { name = "Test.l", isRoot = True, attrPath = [] }) ]
                , maybeAnnotation =
                    Just
                        { pos = p
                        , ty =
                            CA.TypeConstant p
                                "SPCore.List"
                                [ CA.TypeConstant p "SPCore.Bool" [] ]
                        , nonFn = Dict.empty
                        }
                , isNative = False
                , name = "Test.l"
                , localName = "l"
                , pos = p
                }
            )
        ]



----
--- Tuples
--


tuples : Test
tuples =
    Test.Group "Tuples"
        [ codeTest "tuple2"
            "a = 1 & 2"
            (firstEvaluation "a")
            (Test.okEqual <|
                CA.Record p
                    Nothing
                    (Dict.fromList
                        [ ( "first", CA.Literal p (Literal.Number "1") )
                        , ( "second", CA.Literal p (Literal.Number "2") )
                        ]
                    )
            )
        , codeTest "tuple3"
            "a = 1 & 2 & 3"
            (firstEvaluation "a")
            (Test.okEqual <|
                CA.Record p
                    Nothing
                    (Dict.fromList
                        [ ( "first", CA.Literal p (Literal.Number "1") )
                        , ( "second", CA.Literal p (Literal.Number "2") )
                        , ( "third", CA.Literal p (Literal.Number "3") )
                        ]
                    )
            )
        , hasError
            { name = "tuple4"
            , code = "a = 1 & 2 & 3 & 4"
            , run = firstEvaluation "a"
            , test = Test.errContain "use a record"
            }
        , codeTest "tuple2 type"
            """
                        a =
                          as Blah & Blah
                          a
                        """
            (firstDefinition "a")
            (Test.okEqual
                { body = [ CA.Evaluation (CA.Variable p { name = "Test.a", attrPath = [], isRoot = True }) ]
                , maybeAnnotation =
                    Just
                        { pos = p
                        , ty =
                            CA.TypeRecord p
                                Nothing
                                (Dict.fromList
                                    [ ( "first", CA.TypeConstant p "Test.Blah" [] )
                                    , ( "second", CA.TypeConstant p "Test.Blah" [] )
                                    ]
                                )
                        , nonFn = Dict.empty
                        }
                , name = "Test.a"
                , localName = "a"
                , isNative = False
                , pos = p
                }
            )
        , hasError
            { name = "tuple4, type"
            , code =
                """
                        a =
                          as Blah & Blah & Blah & Blah
                          a
                        """
            , run = firstDefinition "a"
            , test = Test.errContain "Use a record"
            }
        ]



----
--- Module and Attribute Paths
--


moduleAndAttributePaths : Test
moduleAndAttributePaths =
    let
        accept s =
            codeTest s
                ("a = " ++ s)
                (firstDefinition "a")
                Test.isOk

        reject s m =
            codeTest s
                ("a = " ++ s)
                (firstDefinition "a")
                (Test.errContain m)
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
    Test.Group "Records"
        [ codeTest "functional update"
            "a = { m with b, c = 1 }"
            (firstEvaluation "a")
            ([ ( "c", CA.Literal p (Literal.Number "1") )
             , ( "b", CA.Variable p { attrPath = [], name = "Test.b", isRoot = True } )
             ]
                |> Dict.fromList
                |> CA.Record p (Just { isRoot = True, attrPath = [], name = "Test.m" })
                |> Test.okEqual
            )
        , codeTest "update shorthand"
            "b = { a.k with y = .x }"
            (firstEvaluation "b")
            (Dict.singleton "y" (CA.Variable p { isRoot = True, attrPath = [ "k", "x" ], name = "Test.a" })
                |> CA.Record p (Just { isRoot = True, attrPath = [ "k" ], name = "Test.a" })
                |> Test.okEqual
            )
        , codeTest "annotation, extensible"
            """
            a =
              as { b with x as Bool }
              a
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
            , code =
                """
                x =
                  c (a b) = 2
                """
            , run = firstEvaluation "x"
            , test = Test.errContain "function"
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
        [ codeTest "annotation on mutable value"
            """
            x =
              a @=
                as Number
                3
              a
            """
            (firstDefinition "x")
            Test.isOk

        --
        , codeTest "annotation on immutable value"
            """
            b =
              as Number
              3
            """
            (firstEvaluation "b")
            Test.isOk
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
                CA.Call p
                    (CA.Variable p { name = "Test.function", isRoot = True, attrPath = [] })
                    (CA.ArgumentExpression <| CA.Variable p { name = "Test.thing", isRoot = True, attrPath = [] })
            )
        , codeTest "sendRight is inlined"
            """
            a = function << thing
            """
            (firstEvaluation "a")
            (Test.okEqual <|
                CA.Call p
                    (CA.Variable p { name = "Test.function", isRoot = True, attrPath = [] })
                    (CA.ArgumentExpression <| CA.Variable p { name = "Test.thing", isRoot = True, attrPath = [] })
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
              fn a b: 1
            """
            (firstEvaluation "f")
            Test.isOk
        , codeTest "short function notation"
            """
            a x y z = x + y + z
            b = fn x y z: x + y + z
            c = fn x: fn y: fn z: x + y + z
            """
            transformABC
            (Test.freeform <| \( a, b, c ) ->
            if a.body == b.body && b.body == c.body then
                Nothing

            else
                [ "The three don't match:"
                , Debug.toString a
                , Debug.toString b
                , Debug.toString c
                ]
                    |> String.join "\n"
                    |> Just
            )
        ]
