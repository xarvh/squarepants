

params as Compiler/MakeCanonical.Params = {
    , meta = TH.meta
    , stripLocations = True
    , source = TH.source
    , name = TH.moduleName
    }


textToModule as Text: Result Text CA.Module =
    code:
    code
        >> Compiler/MakeCanonical.textToCanonicalModule params
        >> TH.resErrorToStrippedText code


codeTest as Text: Text: (Text: Result Text ok): Test.CodeExpectation ok: Test =
    Test.codeTest toHuman


firstDefinition as Text: Result Text CA.ValueDef =
    code:
    code
        >> textToModule
        >> onOk (mod: mod.valueDefs >> Dict.values >> List.head >> Result.fromMaybe "firstDefinition fail")


firstDefinitionStripDeps as Text: Result Text CA.ValueDef =
    code:
    code
        >> firstDefinition
        >> Result.map (v: { v with directConsDeps = Dict.empty, directTypeDeps = Dict.empty, directValueDeps = Dict.empty })


firstEvaluation as Text: Text: Result Text CA.Expression =
    name: code:
    code
        >> firstDefinition
        >> onOk (def: Ok def.body)


# TODO move this to Helpers?
transformAB as Text: Result Text ( CA.ValueDef & CA.ValueDef ) =
    code:

    findAB = mod:
        try mod.valueDefs >> Dict.values >> List.sortBy (def: def.pattern) as
            [a, b]: Just (a & b)
            _: Nothing

    code
        >> textToModule
        >> onOk (x: x >> findAB >> Result.fromMaybe "findAB fail")


shouldHaveSameAB as (ab: c): Test.CodeExpectation ( ab & ab ) =
    getter:
    Test.freeform << ( a & b ):
    if getter a == getter b then
        Nothing

    else
        [ "The two don't match:"
        , toHuman (getter a)
        , toHuman (getter b)
        ]
            >> Text.join "\n"
            >> Just


p as Pos =
    Pos.T



#
# Tests
#


tests as Test =
    Test.Group
        """
        MakeCanonical
        """
        [
        , unionTypes
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


unionTypes as Test =
    Test.Group
        """
        Union types
        """
        [
        , codeTest
            "SKIP tuples op precedence"
            "union A = X Bool & Bool"
            textToModule
            (Test.errorContains ["operators"])
        , codeTest
            """
            Tuples op precedence works with parens
            """
            """
            union A = X (Bool & Bool)
            """
            textToModule
            Test.isOk
        , codeTest
            """
            SKIP [reg] Should reject uppercase arg name
            """
            """
            union Outcome Token output = A
            """
            textToModule
            (Test.errorContains ["Token"])
        ]


binops as Test =
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
            (shouldHaveSameAB x: x.body)
        , codeTest "right associativity"
            """
            a = v :: f :: g
            b = v :: (f :: g)
            """
            transformAB
            (shouldHaveSameAB x: x.body)
        , codeTest "precedence"
            """
            a = 1 + 2 * 3 + 4
            b = 1 + (2 * 3) + 4
            """
            transformAB
            (shouldHaveSameAB x: x.body)
        , codeTest "SKIP functional notation"
            """
            a = (-)
            """
            (firstEvaluation "a")
            (Test.isOkAndEqualTo <<
                CA.Variable p { ref = CA.RefRoot (CoreTypes.makeUsr "-"), attrPath = [] }
            )
        ]



#
# Lists
#


lists as Test =
    Test.Group
        """
        Lists
        """
        [ codeTest "list type sugar"
            """
            l as [ Bool ] =
              l
            """
            firstDefinitionStripDeps
            (Test.isOkAndEqualTo
                { body = CA.Variable p { ref = TH.rootLocal "l", attrPath = [] }
                , native = False
                , pattern =
                    CA.PatternNamed p CA.Immutable "l"
                        (TH.boolType
                            >> CoreTypes.list
                            >> Just
                        )
                , nonFn = Dict.empty
                , parentDefinitions = []

                , directConsDeps = Dict.empty
                , directTypeDeps = Dict.empty
                , directValueDeps = Dict.empty
                }
            )
        ]



#
# Tuples
#


tuples as Test =
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
        , codeTest
            "tuple4"
            "a = 1 & 2 & 3 & 4"
            (firstEvaluation "a")
            (Test.errorContains ["use a record"])
        , codeTest "tuple2 type"
            """
            a as Number & Number =
              a
            """
            firstDefinitionStripDeps
            (Test.isOkAndEqualTo
                { body = CA.Variable p { ref = TH.rootLocal "a", attrPath = [] }
                , pattern =
                    CA.PatternNamed p CA.Immutable "a"
                        (Dict.empty
                            >> Dict.insert "first" TH.numberType
                            >> Dict.insert "second" TH.numberType
                            >> CA.TypeRecord p Nothing
                            >> Just
                        )
                , native = False
                , nonFn = Dict.empty
                , parentDefinitions = []
                , directConsDeps = Dict.empty
                , directTypeDeps = Dict.empty
                , directValueDeps = Dict.empty
                }
            )
        , codeTest
            "tuple4, type"
            """
            a as Blah & Blah & Blah & Blah =
              a
            """
            firstDefinition
            (Test.errorContains ["Use a record"])
        ]



#
# Module and Attribute Paths
#


moduleAndAttributePaths as Test =
    accept = s:
        codeTest s
            ("a = " .. s)
            firstDefinition
            Test.isOk

    reject = s: m:
        codeTest s
            ("a = " .. s)
            firstDefinition
            (Test.errorContains [m])

    Test.Group
        """
        Module and Attribute Paths
        """
        [ accept "blah.blah.blah"
        , reject "Blah.Blah.blah" "constructor"
        , reject "blah.Blah.blah" "case"
        , reject "List.blah.Blah" "lower"
        , reject "List..blah" "space"
        , reject ".Blah" "upper"
        , reject ".blah.blah" "shorthand"
        , reject ".blah" "shorthand"
        , reject "..." ""
        , accept "x .. y"
        ]



#
# Records
#


records as Test =
    Test.Group
        """
        Records
        """
        [
        , codeTest "functional update"
            "a = { m with b, c = 1 }"
            (firstEvaluation "a")
            ([ ( "c" & CA.LiteralNumber p 1 ) , ( "b" & CA.Variable p { attrPath = [], ref = TH.rootLocal "b" } ) ]
                >> Dict.fromList
                >> CA.Record p (Just { attrPath = [], ref = TH.rootLocal "m" })
                >> Test.isOkAndEqualTo
            )
        , codeTest "update shorthand"
            "b = { a.k with y = .x }"
            (firstEvaluation "b")
            (Dict.singleton "y" (CA.Variable p { attrPath = [ "k", "x" ], ref = TH.rootLocal "a" })
                >> CA.Record p (Just { attrPath = [ "k" ], ref = TH.rootLocal "a" })
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


patterns as Test =
    Test.Group
        """
        Patterns
        """
        [ codeTest "[reg] record patterns are NOT extensible"
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


annotations as Test =
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
            firstDefinition
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


pipes as Test =
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
                    (CA.Variable p { ref = TH.rootLocal "function", attrPath = [] })
                    (CA.ArgumentExpression << CA.Variable p { ref = TH.rootLocal "thing", attrPath = [] })
            )
        , codeTest "sendRight is inlined"
            """
            a = function << thing
            """
            (firstEvaluation "a")
            (Test.isOkAndEqualTo <<
                CA.Call p
                    (CA.Variable p { ref = TH.rootLocal "function", attrPath = [] })
                    (CA.ArgumentExpression << CA.Variable p { ref = TH.rootLocal "thing", attrPath = [] })
            )
        ]



#
# Functions
#


functions as Test =
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
#            (Test.freeform << ( a & b & c ):
#            if a.body == b.body and b.body == c.body then
#                Nothing
#
#            else
#                [ "The three don't match:"
#                , toHuman a
#                , toHuman b
#                , toHuman c
#                ]
#                    >> Text.join "\n"
#                    >> Just
#            )
        ]
