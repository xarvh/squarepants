

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


valueDef as Name: CA.Expression: CA.ValueDef =
    name: body:

    {
    , pattern = CA.PatternAny Pos.G { isUnique = False, maybeName = Just name, maybeAnnotation = Nothing }
    , native = False
    , body

    , tyvars = Dict.empty

    , directTypeDeps = Dict.empty
    , directConsDeps = Dict.empty
    , directValueDeps = Dict.empty
    }




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
        , nonFunction
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
            "tuples op precedence"
            "union A = X Bool & Bool"
            textToModule
            (Test.errorContains ["expecting a constructor"])
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
            [reg] Should reject uppercase arg name
            """
            """
            union Outcome Token output = A
            """
            textToModule
            (Test.errorContains ["must start with a lowercase"])
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
                { body = CA.Variable p (TH.rootLocal "l")
                , native = False
                , pattern = CA.PatternAny p { isUnique = False, maybeName = Just "l", maybeAnnotation = (TH.caBool >> CoreTypes.list >> Just) }
                , tyvars = Dict.empty

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
                { body = CA.Variable p (TH.rootLocal "a")
                , pattern =
                    CA.PatternAny p {
                      , isUnique = False
                      , maybeName = Just "a"
                      , maybeAnnotation =
                         Dict.empty
                            >> Dict.insert "first" TH.caNumber
                            >> Dict.insert "second" TH.caNumber
                            >> CA.TypeRecord
                            >> CA.Type p Imm
                            >> Just
                      }
                , native = False
                , tyvars = Dict.empty
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
            (Test.errorContains ["use a record"])
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
            (Test.isOkAndEqualTo <<
                CA.LetIn
                    (valueDef "0" (CA.Variable p (TH.rootLocal "m" )))
                    (CA.Record p
                        (Just (CA.Variable Pos.G (RefLocal "0" )))
                        (Dict.fromList
                            [
                            , "c" & CA.LiteralNumber p 1
                            , "b" & CA.Variable p (TH.rootLocal "b")
                            ]
                        )
                    )
            )
        , codeTest "update shorthand"
            "b = { a with y = .x }"
            (firstEvaluation "b")
            (Test.isOkAndEqualTo <<
                CA.LetIn
                    (valueDef "0" (CA.Variable p (TH.rootLocal "a" )))
                    (CA.Record p
                        (Just (CA.Variable Pos.G (RefLocal "0" )))
                        (Dict.fromList
                            [
                            , "y" & (CA.RecordAccess p "x" (CA.Variable Pos.G (RefLocal "0" )))
                            ]
                        )
                    )
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
        [
        , codeTest "Record patterns can be partial"
            """
            a =
              { with c } = d
            """
            (firstEvaluation "a")
            Test.isOk
        , codeTest "[reg] record patterns are NOT extensible"
            """
            a =
              { b with c } = d
            """
            (firstEvaluation "a")
            (Test.errorContains ["extend pattern"])
        ]



#
# Annotations
#


annotations as Test =
    Test.Group
        """
        Annotations
        """
        [
        , codeTest "annotation on unique value"
            """
            x =
              !a as !Number =
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
                    (CA.Variable p (TH.rootLocal "function"))
                    [CA.ArgumentExpression (CA.Variable p (TH.rootLocal "thing"))]
            )
        , codeTest "sendRight is inlined"
            """
            a = function << thing
            """
            (firstEvaluation "a")
            (Test.isOkAndEqualTo <<
                CA.Call p
                    (CA.Variable p (TH.rootLocal "function"))
                    [CA.ArgumentExpression (CA.Variable p (TH.rootLocal "thing"))]
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
        [
        , codeTest "[rec] function with call"
            """
            a =
                fn x:
                    add x 1
            """
            (firstEvaluation "f")
            (Test.isOkAndEqualTo
                (CA.Fn p
                    [CA.ParameterPattern (CA.PatternAny p { isUnique = False, maybeAnnotation = Nothing, maybeName = Just "x"})]
                    (CA.Call p
                        (CA.Variable p (TH.rootLocal "add"))
                        [
                        , CA.ArgumentExpression (CA.Variable p (RefLocal "x"))
                        , CA.ArgumentExpression (CA.LiteralNumber p 1)
                        ]
                    )
                )
            )
        , codeTest "[rec] function with two arguments"
            """
            f =
              fn a, b: 1
            """
            (firstEvaluation "f")
            (Test.isOkAndEqualTo
                (CA.Fn p
                    [
                    , CA.ParameterPattern (CA.PatternAny p { isUnique = False, maybeAnnotation = Nothing, maybeName = Just "a"})
                    , CA.ParameterPattern (CA.PatternAny p { isUnique = False, maybeAnnotation = Nothing, maybeName = Just "b"})
                    ]
                    (CA.LiteralNumber p 1)
                )
            )
        ]


nonFunction as Test =
    Test.Group
        """
        NonFunction
        """
        [
        , codeTest "one"
            """
            funz as a with a NonFunction =
                1
            """
            firstDefinitionStripDeps
            (Test.isOkAndEqualTo
                { body = CA.LiteralNumber p 1
                , native = False
                , pattern = CA.PatternAny p { isUnique = False, maybeName = Just "funz", maybeAnnotation =  Just << CA.Type p Imm << CA.TypeAnnotationVariable "a" }
                , tyvars = Dict.singleton "a" { allowFunctions = False, allowUniques = False }
                , directConsDeps = Dict.empty
                , directTypeDeps = Dict.empty
                , directValueDeps = Dict.empty
                }
            )
        ]

