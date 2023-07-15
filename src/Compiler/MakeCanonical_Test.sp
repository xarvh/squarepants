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
        , argumentPlaceholders
        , polymorphicUniques
        , numbers
        ]


#
# Helpers
#

params as fn Error.Module: Compiler/MakeCanonical.ReadOnly =
    fn errorModule:
    {
    , meta = TH.meta
    , umr = TH.moduleUmr
    , errorModule
    }


textToModule as fn Text: Result Text CA.Module =
    fn code:
    code
    >> TH.errorModule
    >> params
    >> Compiler/MakeCanonical.textToCanonicalModule True __
    >> TH.resErrorToStrippedText


codeTest as fn Text, Text, (fn Text: Result Text ok), Test.CodeExpectation ok: Test =
    Test.codeTest toHuman __ __ __ __


firstDefinition as fn Text: Result Text CA.ValueDef =
    fn code:
    code
        >> textToModule
        >> onOk (fn mod: mod.valueDefs >> Dict.values >> List.head >> Result.fromMaybe "firstDefinition fail" __)


firstDefinitionStripDeps as fn Text: Result Text CA.ValueDef =
    fn code:
    code
        >> firstDefinition
        >> Result.map (fn v: { v with directTypeDeps = Dict.empty, directValueDeps = Dict.empty }) __


firstEvaluation as fn Text: fn Text: Result Text CA.Expression =
    fn name: fn code:
    code
    >> firstDefinition
    >> onOk (fn def: Ok def.body)


# TODO move this to Helpers?
transformAB as fn Text: Result Text ( CA.ValueDef & CA.ValueDef ) =
    fn code:

    findAB =
        fn mod:
        try mod.valueDefs >> Dict.values >> List.sortBy (fn def: def.pattern) __ as
            , [a, b]: Just (a & b)
            , _: Nothing

    code
        >> textToModule
        >> onOk (fn x: x >> findAB >> Result.fromMaybe "findAB fail" __)


shouldHaveSameAB as fn (fn ab: c): Test.CodeExpectation ( ab & ab ) =
    fn getter:
    Test.freeform << fn ( a & b ):
    if getter a == getter b then
        Nothing

    else
        [ "The two don't match:"
        , toHuman (getter a)
        , toHuman (getter b)
        ]
        >> Text.join "\n" __
        >> Just


p as Pos =
    Pos.T


# Same as core type, but has Pos.T rather than Pos.N
# Needed because tests will produce Pos.T!
caNumber as CA.RawType =
    CA.TypeNamed Pos.T (Meta.spCoreUSR "Number") []


valueDef as fn Name, CA.Expression: CA.ValueDef =
    fn name, body:

    {
    , uni = Imm
    , pattern = CA.PatternAny Pos.G { maybeName = Just name, maybeAnnotation = Nothing }
    , native = False
    , body

    , tyvars = Dict.empty
    , univars = Dict.empty

    , directTypeDeps = Dict.empty
    , directValueDeps = Dict.empty
    }



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
                {
                , uni = Imm
                , body = CA.Variable p (TH.rootLocal "l")
                , native = False
                , pattern = CA.PatternAny p { maybeName = Just "l", maybeAnnotation = (CoreTypes.boolType >> CoreTypes.listType >> Just) }
                , tyvars = Dict.empty
                , univars = Dict.empty

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
                , uni = Imm
                , pattern =
                    CA.PatternAny p
                      {
                      , maybeName = Just "a"
                      , maybeAnnotation =
                         Dict.empty
                            >> Dict.insert "first" caNumber __
                            >> Dict.insert "second" caNumber __
                            >> CA.TypeRecord p __
                            >> Just
                      }
                , native = False
                , tyvars = Dict.empty
                , univars = Dict.empty
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
    accept = fn s:
        codeTest s
            ("a = " .. s)
            firstDefinition
            Test.isOk

    reject = fn s, m:
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
              !a as Number =
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
        , codeTest "annotation of recycling function"
            """
            b as fn @Result e a: !Result e a =
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
                    [CA.ParameterPattern Imm (CA.PatternAny p { maybeAnnotation = Nothing, maybeName = Just "x"})]
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
                    , CA.ParameterPattern Imm (CA.PatternAny p { maybeAnnotation = Nothing, maybeName = Just "a"})
                    , CA.ParameterPattern Imm (CA.PatternAny p { maybeAnnotation = Nothing, maybeName = Just "b"})
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
                , uni = Imm
                , native = False
                , pattern = CA.PatternAny p { maybeName = Just "funz", maybeAnnotation = CA.TypeAnnotationVariable p "a" >> Just }
                , tyvars = Dict.ofOne "a" { allowFunctions = False }
                , univars = Dict.empty
                , directTypeDeps = Dict.empty
                , directValueDeps = Dict.empty
                }
            )
        ]


argumentPlaceholders as Test =
    Test.Group
        """
        Argument placeholders
        """
        [
        , codeTest
            """
            Base
            """
            """
            f = f __ __
            """
            firstDefinitionStripDeps
            (Test.isOkAndEqualTo
                {
                , native = False
                , uni = Imm
                , pattern = CA.PatternAny p { maybeName = Just "f", maybeAnnotation = Nothing }
                , tyvars = Dict.empty
                , univars = Dict.empty
                , directTypeDeps = Dict.empty
                , directValueDeps = Dict.empty
                , body =
                    CA.Fn p
                        [
                        , CA.ParameterPlaceholder "0" 0
                        , CA.ParameterPlaceholder "1" 1
                        ]
                        ( CA.Call p
                            (CA.Variable p (RefGlobal (USR (UMR (Meta.SourceDirId "<Test>") "(test)") "f")))
                            [
                            , CA.ArgumentExpression (CA.Variable p (RefLocal "0"))
                            , CA.ArgumentExpression (CA.Variable p (RefLocal "1"))
                            ]
                        )
                }
            )
        ]

polymorphicUniques as Test =
    Test.Group
        """
        Polymorphic Uniques
        """
        [
        , codeTest
            """
            In pattern
            """
            """
            scope =
                1?f as a = meh
            """
            firstDefinitionStripDeps
            (Test.isOkAndEqualTo
                {
                , native = False
                , uni = Imm
                , pattern = CA.PatternAny p { maybeName = Just "scope", maybeAnnotation = Nothing }
                , tyvars = Dict.empty
                , univars = Dict.empty
                , directTypeDeps = Dict.empty
                , directValueDeps = Dict.empty
                , body =
                    CA.LetIn
                        {
                        , native = False
                        , uni = Depends 1
                        , pattern = CA.PatternAny p { maybeName = Just "f", maybeAnnotation = Just (CA.TypeAnnotationVariable p "a") }
                        , tyvars = Dict.ofOne "a" { allowFunctions = True }
                        , univars = Dict.ofOne 1 None
                        , directTypeDeps = Dict.empty
                        , directValueDeps = Dict.empty
                        , body = CA.Variable p (TH.rootLocal "meh")
                        }
                        (CA.Constructor Pos.G CoreTypes.noneName [])
                }
            )
        , codeTest
            """
            In annotation
            """
            """
            isOk as fn (fn 1?a: 2?Re error b), 1?Re error a: 2?Re error b = meh
            """
            (fn t: firstDefinitionStripDeps t >> Result.map (fn x: x.univars) __)
            (Test.isOkAndEqualTo << Dict.fromList [1 & None, 2 & None])
        ]


numbers as Test =
    Test.Group
        """
        Numbers
        """
        [
        , codeTest
            "Percent"
            "a = 1%"
            (firstEvaluation "a")
            (Test.isOkAndEqualTo << CA.LiteralNumber p 0.01)
        , codeTest
            "Underscore"
            "a = 1_000_000"
            (firstEvaluation "a")
            (Test.isOkAndEqualTo << CA.LiteralNumber p (1000 * 1000))
        ]
