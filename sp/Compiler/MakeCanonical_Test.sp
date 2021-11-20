
p =
    Pos.T


codeTest =
    Test.codeTest Debug.toHuman


hasError { name, code, run, test } =
    Test.codeTest Debug.toHuman name code run test


firstDefinition name code =
    as Text: Text: Result Text CA.ValueDef
    code
        >> Compiler/TestHelpers.textToCanonicalModule
        >> Compiler/TestHelpers.resErrorToStrippedText code
        >> Result.andThen (fn mod: mod >> CA.findValue name >> Result.fromMaybe "findValue fail")


firstEvaluation name code =
    as Text: Text: Result Text CA.Expression
    code
        >> firstDefinition name
        >> Result.andThen (fn def: List.head def.body >> Result.fromMaybe "head fail")
        >> Result.andThen (fn x: x >> asEvaluation >> Result.fromMaybe "asEval fail")


textToCanonicalModule code =
    code
        >> Compiler/TestHelpers.textToCanonicalModule
        >> Compiler/TestHelpers.resErrorToStrippedText code


asEvaluation s =
   as CA.Statement: Maybe CA.Expression
   try s as
        CA.Evaluation expr:
            Just expr

        _:
            Nothing


# TODO move this to Helpers?
transformAB code =
    as Text: Result Text ( CA.ValueDef & CA.ValueDef )

    findAB mod =
        Maybe.map2
            Tuple.pair
            (CA.findValue "a" mod)
            (CA.findValue "b" mod)

    code
        >> Compiler/TestHelpers.textToCanonicalModule
        >> Compiler/TestHelpers.resErrorToStrippedText code
        >> Result.andThen (fn x: x >> findAB >> Result.fromMaybe "findAB fail")


shouldHaveSameAB getter =
    as (ab: c): Test.CodeExpectation ( ab & ab )
    Test.freeform << fn ( a & b ):
    if getter a == getter b:
        Nothing

    else
        [ "The two don't match:"
        , Debug.toHuman (getter a)
        , Debug.toHuman (getter b)
        ]
            >> Text.join "\n"
            >> Just


#transformABC code =
#    as Text: Result Text ( CA.ValueDef & CA.ValueDef & CA.ValueDef )
#
#    findABC mod =
#        Maybe.map3
#            (fn a b c: ( a & b & c ))
#            (CA.findValue "Test.a" mod)
#            (CA.findValue "Test.b" mod)
#            (CA.findValue "Test.c" mod)
#
#    code
#        >> Compiler/TestHelpers.textToCanonicalModule
#        >> Compiler/TestHelpers.resErrorToStrippedText code
#        >> Result.andThen (fn x: x >> findABC >> Result.fromMaybe "findABC fail")



#
# Tests
#


tests =
    as Test
    Test.Group
        """
        MakeCanonical
        """
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




#
# Union Types
#


unionTypes =
    as Test
    Test.Group
        """
        Union types
        """
        [ hasError
            { name = "name starts with uppercase"
            , code = "union a = A"
            , run = textToCanonicalModule
            , test = Test.errorContains ["uppercase"]
            }
        , hasError
            { name = "constructor names start with uppercase"
            , code = "union A = a"
            , run = textToCanonicalModule
            , test = Test.errorContains ["constructor"]
            }
        , hasError
            { name = "SKIP tuples op precedence"
            , code = "union A = X Bool & Bool"
            , run = textToCanonicalModule
            , test = Test.errorContains ["operators"]
            }
        , codeTest
            """
            Tuples op precedence works with parens
            """
            """
            union A = X (Bool & Bool)
            """
            textToCanonicalModule
            Test.isOk
        , codeTest
            """
            SKIP [reg] Should reject uppercase arg name
            """
            """
            union Outcome Token output = A
            """
            textToCanonicalModule
            (Test.errorContains ["Token"])
        ]


binops =
    as Test
    Test.Group
        """
        Binops
        """
        [ codeTest "left associativity"
            """
            a = v >> f >> g
            b = (v >> f) >> g
            """
            transformAB
            (shouldHaveSameAB fn x: x.body)
        , codeTest "right associativity"
            """
            a = v :: f :: g
            b = v :: (f :: g)
            """
            transformAB
            (shouldHaveSameAB fn x: x.body)
        , codeTest "precedence"
            """
            a = 1 + 2 * 3 + 4
            b = 1 + (2 * 3) + 4
            """
            transformAB
            (shouldHaveSameAB fn x: x.body)
        , codeTest "SKIP functional notation"
            """
            a = (-)
            """
            (firstEvaluation "a")
            (Test.isOkAndEqualTo <<
                CA.Variable p { ref = CA.Foreign (Meta.USR Meta.Core [] "-"), attrPath = [] }
            )
        ]



#
# Lists
#


lists =
    as Test
    Test.Group
        """
        Lists
        """
        [ codeTest "list type sugar"
            """
            l as [ Bool ] =
              l
            """
            (firstDefinition "l")
            (Test.isOkAndEqualTo
                { body = [ CA.Evaluation (CA.Variable p { ref = CA.ModuleLocal "l", attrPath = [] }) ]
                , native = False
                , mutable = False
                , pattern =
                    CA.PatternAny p
                        (Just "l")
                        (Compiler/TestHelpers.boolType
                            >> CoreTypes.list
                            >> Just
                        )
                , nonFn = Dict.empty
                , parentDefinitions = []
                }
            )
        ]



#
# Tuples
#


tuples =
    as Test
    Test.Group
        """
        Tuples
        """
        [ codeTest "tuple2"
            "a = 1 & 2"
            (firstEvaluation "a")
            (Test.isOkAndEqualTo <<
                CA.Record p
                    Nothing
                    (Dict.fromList
                        [ ( "first" & CA.LiteralNumber p 1 )
                        , ( "second" & CA.LiteralNumber p 2 )
                        ]
                    )
            )
        , codeTest "tuple3"
            "a = 1 & 2 & 3"
            (firstEvaluation "a")
            (Test.isOkAndEqualTo <<
                CA.Record p
                    Nothing
                    (Dict.fromList
                        [ ( "first" & CA.LiteralNumber p 1 )
                        , ( "second" & CA.LiteralNumber p 2 )
                        , ( "third" & CA.LiteralNumber p 3 )
                        ]
                    )
            )
        , hasError
            { name = "tuple4"
            , code = "a = 1 & 2 & 3 & 4"
            , run = firstEvaluation "a"
            , test = Test.errorContains ["use a record"]
            }
        , codeTest "tuple2 type"
            """
            a as Number & Number =
              a
            """
            (firstDefinition "a")
            (Test.isOkAndEqualTo
                { body = [ CA.Evaluation (CA.Variable p { ref = CA.ModuleLocal "a", attrPath = [] }) ]
                , pattern =
                    CA.PatternAny p
                        (Just "a")
                        (Dict.empty
                            >> Dict.insert "first" Compiler/TestHelpers.numberType
                            >> Dict.insert "second" Compiler/TestHelpers.numberType
                            >> CA.TypeRecord p Nothing
                            >> Just
                        )
                , native = False
                , mutable = False
                , nonFn = Dict.empty
                , parentDefinitions = []
                }
            )
        , hasError
            { name = "tuple4, type"
            , code =
                """
                a as Blah & Blah & Blah & Blah =
                  a
                """
            , run = firstDefinition "a"
            , test = Test.errorContains ["Use a record"]
            }
        ]



#
# Module and Attribute Paths
#


moduleAndAttributePaths =
    as Test
    accept s =
        codeTest s
            ("a = " .. s)
            (firstDefinition "a")
            Test.isOk

    reject s m =
        codeTest s
            ("a = " .. s)
            (firstDefinition "a")
            (Test.errorContains [m])

    Test.Group
        """
        Module and Attribute Paths
        """
        [ accept "blah.blah.blah"
        , reject "Blah.Blah.blah" "Constructor"
        , reject "blah.Blah.blah" "lower"
        , reject "List.blah.Blah" "lower"
        , reject "List..blah" "dot"
        , reject ".Blah" "shorthand"
        , reject ".blah.blah" "shorthand"
        , reject ".blah" "shorthand"
        , reject "..." ""
        , accept "x .. y"
        ]



#
# Records
#


records =
    as Test
    Test.Group
        """
        Records
        """
        [
        , codeTest "functional update"
            "a = { m with b, c = 1 }"
            (firstEvaluation "a")
            ([ ( "c" & CA.LiteralNumber p 1 ) , ( "b" & CA.Variable Pos.G { attrPath = [], ref = CA.ModuleLocal "b" } ) ]
                >> Dict.fromList
                >> CA.Record p (Just { attrPath = [], ref = CA.ModuleLocal "m" })
                >> Test.isOkAndEqualTo
            )
        , codeTest "update shorthand"
            "b = { a.k with y = .x }"
            (firstEvaluation "b")
            (Dict.singleton "y" (CA.Variable p { attrPath = [ "k", "x" ], ref = CA.ModuleLocal "a" })
                >> CA.Record p (Just { attrPath = [ "k" ], ref = CA.ModuleLocal "a" })
                >> Test.isOkAndEqualTo
            )
        , codeTest "annotation, extensible"
            """
            a as { b with x as Bool } =
              a
            """
            (firstEvaluation "a")
            (Test.errorContains ["disabled"])
        ]



#
# Pattern
#


patterns =
    as Test
    Test.Group
        """
        Patterns
        """
        [ hasError
            { name = "can't declare functions inside patterns "
            , code =
                """
                x =
                  c (a b) = 2
                """
            , run = firstEvaluation "x"
            , test = Test.errorContains ["function"]
            }
        , codeTest "[reg] record patterns are NOT extensible"
            """
            a =
              { b with c } = d
            """
            (firstEvaluation "a")
            (Test.errorContains ["with"])
        ]



#
# Annotations
#


annotations =
    as Test
    Test.Group
        """
        Annotations
        """
        [ codeTest "annotation on mutable value"
            """
            x =
              a as Number @=
                3
              a
            """
            (firstDefinition "x")
            Test.isOk

        , codeTest "annotation on immutable value"
            """
            b as Number =
              3
            """
            (firstEvaluation "b")
            Test.isOk
        ]



#
# Pipes
#


pipes =
    as Test
    Test.Group
        """
        Pipes
        """
        [ codeTest "sendLeft is inlined"
            """
            a = thing >> function
            """
            (firstEvaluation "a")
            (Test.isOkAndEqualTo <<
                CA.Call p
                    (CA.Variable p { ref = CA.ModuleLocal "function", attrPath = [] })
                    (CA.ArgumentExpression << CA.Variable p { ref = CA.ModuleLocal "thing", attrPath = [] })
            )
        , codeTest "sendRight is inlined"
            """
            a = function << thing
            """
            (firstEvaluation "a")
            (Test.isOkAndEqualTo <<
                CA.Call p
                    (CA.Variable p { ref = CA.ModuleLocal "function", attrPath = [] })
                    (CA.ArgumentExpression << CA.Variable p { ref = CA.ModuleLocal "thing", attrPath = [] })
            )
        ]



#
# Functions
#


functions =
    as Test
    Test.Group
        """
        Functions
        """
        [ codeTest "[rec] lambda with two arguments"
            """
            f =
              a: b: 1
            """
            (firstEvaluation "f")
            Test.isOk
#        , codeTest "short function notation"
#            """
#            a = x: y: z: x + y + z
#            b = x: y: z: x + y + z
#            c = x: y: z: x + y + z
#            """
#            transformABC
#            (Test.freeform << fn ( a & b & c ):
#            if a.body == b.body and b.body == c.body:
#                Nothing
#
#            else
#                [ "The three don't match:"
#                , Debug.toHuman a
#                , Debug.toHuman b
#                , Debug.toHuman c
#                ]
#                    >> Text.join "\n"
#                    >> Just
#            )
        ]
