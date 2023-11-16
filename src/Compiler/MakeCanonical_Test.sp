#
# Tests
#

tests as Test =
    Test.'group
        """
        MakeCanonical
        """
        [
        , varTypes
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
        , shadowing
        ]


#
# Helpers
#

params as fn Error.Module: Compiler/MakeCanonical.ReadOnly =
    fn errorModule:
    {
    , errorModule
    , resolvePars = fn pos: TH.resolvePars
    , umr = TH.moduleUmr
    }


textToModule as fn Text: Result Text CA.Module =
    fn code:
    code
    >> TH.errorModule
    >> params
    >> Compiler/MakeCanonical.textToCanonicalModule 'true __
    >> TH.resErrorToStrippedText


codeTest as fn Text, Text, fn Text: Result Text ok, Test.CodeExpectation ok: Test =
    Test.codeTest toHuman __ __ __ __


firstDefinition as fn Text: Result Text CA.ValueDef =
    fn code:
    code
    >> textToModule
    >> onOk (fn mod: mod.valueDefs >> Dict.values >> List.head >> Result.fromMaybe "firstDefinition fail" __)


firstDefinitionStripDeps as fn Text: Result Text CA.ValueDef =
    __
    >> firstDefinition
    >> Result.map (fn v: { v with directDeps = Dict.empty }) __


firstEvaluation as fn Text: fn Text: Result Text CA.Expression =
    fn name:
    fn code:
    code
    >> firstDefinition
    >> onOk (fn def: Maybe.toResult "body is 'nothing" def.maybeBody)


# TODO move this to Helpers?
transformAB as fn Text: Result Text (CA.ValueDef & CA.ValueDef) =
    fn code:
    findAB =
        fn mod:
        try mod.valueDefs >> Dict.values >> List.sortBy (fn def: def.pattern) __ as
            [ a, b ]: 'just (a & b)
            _: 'nothing

    code
    >> textToModule
    >> onOk (fn x: x >> findAB >> Result.fromMaybe "findAB fail" __)


shouldHaveSameAB as fn fn ab: c: Test.CodeExpectation (ab & ab) =
    fn getter:
    Test.freeform
    << (fn a & b:
         if getter a == getter b then
             'nothing
         else
             [
             , "The two don't match:"
             , toHuman (getter a)
             , toHuman (getter b)
             ]
             >> Text.join "\n" __
             >> 'just
    )


p as Pos =
    Pos.'t


localDef as fn Name, CA.Expression: CA.LocalDef =
    fn name, body:
    {
    , body
    , pattern = CA.'patternAny Pos.'g ('just name) 'nothing
    , uni = 'imm
    }


#
# Variant Types
#

varTypes as Test =
    Test.'group
        """
        Variant types
        """
        [
        , codeTest
            """
            Tuples op precedence
            """
            """
            var A = 'x Bool & Bool
            """
            textToModule
            (Test.errorContains [ "I need a 'constructor" ])
        , codeTest
            """
            Tuples op precedence works with parens
            """
            """
            var A = 'x (Bool & Bool)
            """
            textToModule
            Test.isOk
        , codeTest
            """
            SKIP (make `var` a keyword?) [reg] Should reject uppercase arg name
            """
            """
            var Outcome Token output = 'a
            """
            textToModule
            (Test.errorContains [ "must start with a lowercase" ])
        ]


binops as Test =
    Test.'group
        """
        Binops
        """
        [
        , codeTest
            "left associativity"
            """
            a = v >> f >> g
            b = (v >> f) >> g
            """
            transformAB
            (shouldHaveSameAB (fn x: x.maybeBody))
        , codeTest
            "right associativity"
            """
            a = v :: f :: g
            b = v :: (f :: g)
            """
            transformAB
            (shouldHaveSameAB (fn x: x.maybeBody))
        , codeTest
            "precedence"
            """
            a = 1 + 2 * 3 + 4
            b = 1 + (2 * 3) + 4
            """
            transformAB
            (shouldHaveSameAB (fn x: x.maybeBody))
        , codeTest
            """
            SKIP (burned out) Pipe optimization 1
            """
            """
            a = b >> a __
            b = a b
            """
            transformAB
            (shouldHaveSameAB (fn x: x.maybeBody))
        , codeTest
            """
            SKIP (burned out) Pipe optimization 2
            """
            """
            a = (__ >> __ >> __) a b c
            b = a >> b >> c
            """
            transformAB
            (shouldHaveSameAB (fn x: x.maybeBody))
        , codeTest
            """
            Op chain definition and optimization
            """
            """
            a = __ + __ + 3 + __
            """
            (firstEvaluation "a")
            (Test.isOkAndEqualTo
                 (CA.'fn
                      p
                      [
                      , CA.'parameterPlaceholder 0
                      , CA.'parameterPlaceholder 1
                      , CA.'parameterPlaceholder 2
                      ]
                      (CA.'call
                           p
                           (CA.'variable p ('refGlobal CoreDefs.add.usr))
                           [
                           , CA.'argumentExpression (CA.'variable p ('refPlaceholder 0))
                           , CA.'argumentExpression
                               (CA.'call
                                    p
                                    (CA.'variable p ('refGlobal CoreDefs.add.usr))
                                    [
                                    , CA.'argumentExpression (CA.'variable p ('refPlaceholder 1))
                                    , CA.'argumentExpression
                                        (CA.'call
                                             p
                                             (CA.'variable p ('refGlobal CoreDefs.add.usr))
                                             [
                                             , CA.'argumentExpression (CA.'literalNumber p 3)
                                             , CA.'argumentExpression (CA.'variable p ('refPlaceholder 2))
                                             ]
                                        )
                                    ]
                               )
                           ]
                      )
                 )
            )
        ]


#
# Lists
#

lists as Test =
    Test.'group
        """
        Lists
        """
        [
        , codeTest
            "list type sugar"
            """
            l as [ Bool ] =
              l
            """
            firstDefinitionStripDeps
            (Test.isOkAndEqualTo
                 {
                 , directDeps = Dict.empty
                 , maybeAnnotation =
                     'just
                         {
                         , raw = CoreDefs.listType TH.caBool
                         , tyvars = Dict.empty
                         , univars = Dict.empty
                         }
                 , maybeBody = 'just << CA.'variable p (TH.rootLocal "l")
                 , name = "l"
                 , namePos = p
                 }
            )
        ]


#
# Tuples
#

tuples as Test =
    Test.'group
        """
        Tuples
        """
        [
        , codeTest
            "tuple2"
            "a = 1 & 2"
            (firstEvaluation "a")
            (Test.isOkAndEqualTo
             << CA.'record
                 p
                 'nothing
                 (Dict.fromList
                      [
                      , "first" & CA.'literalNumber p 1
                      , "second" & CA.'literalNumber p 2
                      ]
                 )
            )
        , codeTest
            "tuple3"
            "a = 1 & 2 & 3"
            (firstEvaluation "a")
            (Test.isOkAndEqualTo
             << CA.'record
                 p
                 'nothing
                 (Dict.fromList
                      [
                      , "first" & CA.'literalNumber p 1
                      , "second" & CA.'literalNumber p 2
                      , "third" & CA.'literalNumber p 3
                      ]
                 )
            )
        , codeTest "tuple4" "a = 1 & 2 & 3 & 4" (firstEvaluation "a") (Test.errorContains [ "use a record" ])
        , codeTest
            """
            Tuple2 type
            """
            """
            a as Number & Number =
              a
            """
            firstDefinitionStripDeps
            (Test.isOkAndEqualTo
                 {
                 , directDeps = Dict.empty
                 , maybeAnnotation =
                     'just
                         {
                         , raw =
                             Dict.empty
                             >> Dict.insert "first" TH.caNumber __
                             >> Dict.insert "second" TH.caNumber __
                             >> CA.'typeRecord p __
                         , tyvars = Dict.empty
                         , univars = Dict.empty
                         }
                 , maybeBody = 'just << CA.'variable p (TH.rootLocal "a")
                 , name = "a"
                 , namePos = p
                 }
            )
        , codeTest
            "tuple4, type"
            """
            a as Blah & Blah & Blah & Blah =
              a
            """
            firstDefinition
            (Test.errorContains [ "use a record" ])
        ]


#
# Module and Attribute Paths
#

moduleAndAttributePaths as Test =
    accept =
        fn s:
            codeTest s ("a = " .. s) firstDefinition Test.isOk

    reject =
        fn s, m:
            codeTest s ("a = " .. s) firstDefinition (Test.errorContains [ m ])

    # TODO this stuff has been moved back to the Lexer, so I should also move these tests?
    Test.'group
        """
        Module and Attribute Paths
        """
        [
        , accept "blah.blah.blah"
        , reject "Blah.Blah.blah" "attribute"
#        , reject "blah.Blah.blah" "case"
        , reject "List.blah.Blah" "lower"
        , reject "List..blah" "space"
        , reject ".Blah" "must start with a lowercase"
        , reject ".blah.blah" "shorthand"
        , reject ".blah" "shorthand"
        , reject "..." ""
        , accept "x .. y"
        ]


#
# Records
#

records as Test =
    Test.'group
        """
        Records
        """
        [
        , codeTest
            "functional update"
            "a = { m with b, c = 1 }"
            (firstEvaluation "a")
            (Test.isOkAndEqualTo
             << CA.'letIn
                 (localDef "0" (CA.'variable p (TH.rootLocal "m")))
                 (CA.'record
                      p
                      ('just (CA.'variable Pos.'g ('refLocal "0")))
                      (Dict.fromList
                           [
                           , "c" & CA.'literalNumber p 1
                           , "b" & CA.'variable p (TH.rootLocal "b")
                           ]
                      )
                 )
            )
        , codeTest
            """
            Update shorthand
            """
            """
            b = { a with y = .x }
            """
            (firstEvaluation "b")
            (Test.isOkAndEqualTo
             << CA.'letIn
                 (localDef "0" (CA.'variable p (TH.rootLocal "a")))
                 (CA.'record
                      p
                      ('just (CA.'variable Pos.'g ('refLocal "0")))
                      (Dict.fromList
                           [
                           , "y" & CA.'recordAccess p "x" (CA.'variable Pos.'g ('refLocal "0"))
                           ]
                      )
                 )
            )
        , codeTest
            "annotation, extensible"
            """
            a as { b with x as Bool } =
              a
            """
            (firstEvaluation "a")
            (Test.errorContains [ "disabled" ])
        ]


#
# Pattern
#

patterns as Test =
    Test.'group
        """
        Patterns
        """
        [
        , codeTest
            "Record patterns can be partial"
            """
            a =
              { with c } = d
            """
            (firstEvaluation "a")
            Test.isOk
        , codeTest
            "[reg] record patterns are NOT extensible"
            """
            a =
              { b with c } = d
            """
            (firstEvaluation "a")
            (Test.errorContains [ "extend pattern" ])
        ]


#
# Annotations
#

annotations as Test =
    Test.'group
        """
        Annotations
        """
        [
        , codeTest
            "annotation on unique value"
            """
            x =
              !a as Number =
                3
              a
            """
            firstDefinition
            Test.isOk
        , codeTest
            "annotation on immutable value"
            """
            b as Number =
              3
            """
            (firstEvaluation "b")
            Test.isOk
        , codeTest
            "annotation of recycling function"
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
    Test.'group
        """
        Pipes
        """
        [
        , codeTest
            "sendLeft is inlined"
            """
            a = thing >> function
            """
            (firstEvaluation "a")
            (Test.isOkAndEqualTo << CA.'call p (CA.'variable p (TH.rootLocal "function")) [ CA.'argumentExpression (CA.'variable p (TH.rootLocal "thing")) ])
        , codeTest
            "sendRight is inlined"
            """
            a = function << thing
            """
            (firstEvaluation "a")
            (Test.isOkAndEqualTo << CA.'call p (CA.'variable p (TH.rootLocal "function")) [ CA.'argumentExpression (CA.'variable p (TH.rootLocal "thing")) ])
        ]


#
# Functions
#

functions as Test =
    Test.'group
        """
        Functions
        """
        [
        , codeTest
            "[rec] function with call"
            """
            a =
                fn x:
                    add x 1
            """
            (firstEvaluation "f")
            (Test.isOkAndEqualTo
                 (CA.'fn
                      p
                      [ CA.'parameterPattern 'imm (CA.'patternAny p ('just "x") 'nothing) ]
                      (CA.'call
                           p
                           (CA.'variable p (TH.rootLocal "add"))
                           [
                           , CA.'argumentExpression (CA.'variable p ('refLocal "x"))
                           , CA.'argumentExpression (CA.'literalNumber p 1)
                           ]
                      )
                 )
            )
        , codeTest
            "[rec] function with two arguments"
            """
            f =
              fn a, b: 1
            """
            (firstEvaluation "f")
            (Test.isOkAndEqualTo
                 (CA.'fn
                      p
                      [
                      , CA.'parameterPattern 'imm (CA.'patternAny p ('just "a") 'nothing)
                      , CA.'parameterPattern 'imm (CA.'patternAny p ('just "b") 'nothing)
                      ]
                      (CA.'literalNumber p 1)
                 )
            )
        ]


nonFunction as Test =
    Test.'group
        """
        NonFunction
        """
        [
        , codeTest
            "one"
            """
            funz as a with a NonFunction =
                1
            """
            firstDefinitionStripDeps
            (Test.isOkAndEqualTo
                 {
                 , directDeps = Dict.empty
                 , maybeAnnotation =
                     'just
                         {
                         , raw = CA.'typeAnnotationVariable p "a"
                         , tyvars = Dict.ofOne "a" { nonFn = 'just Pos.'t }
                         , univars = Dict.empty
                         }
                 , maybeBody = 'just << CA.'literalNumber p 1
                 , name = "funz"
                 , namePos = p
                 }
            )
        ]


argumentPlaceholders as Test =
    Test.'group
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
                 , directDeps = Dict.empty
                 , maybeAnnotation = 'nothing
                 , maybeBody =
                     'just
                     << CA.'fn
                         p
                         [
                         , CA.'parameterPlaceholder 0
                         , CA.'parameterPlaceholder 1
                         ]
                         (CA.'call
                              p
                              (CA.'variable p ('refGlobal (TH.moduleUsr "f")))
                              [
                              , CA.'argumentExpression (CA.'variable p ('refPlaceholder 0))
                              , CA.'argumentExpression (CA.'variable p ('refPlaceholder 1))
                              ]
                         )
                 , name = "f"
                 , namePos = p
                 }
            )
        , codeTest
            """
            Pipelines work with placeholders
            """
            """
            f = __ >> a >> b
            """
            (firstEvaluation "f")
            (Test.isOkAndEqualTo
                 (CA.'fn
                      p
                      [ CA.'parameterPlaceholder 0 ]
                      (CA.'call
                           p
                           (CA.'variable p ('refGlobal (TH.moduleUsr "b")))
                           [
                           , CA.'argumentExpression
                               (CA.'call
                                    p
                                    (CA.'variable p ('refGlobal (TH.moduleUsr "a")))
                                    [
                                    , CA.'argumentExpression (CA.'variable p ('refPlaceholder 0))
                                    ]
                               )
                           ]
                      )
                 )
            )
        , codeTest
            """
            try..as
            """
            """
            f = try __ as "": 1
            """
            (firstEvaluation "f")
            (Test.isOkAndEqualTo
                 (CA.'fn
                      p
                      [ CA.'parameterPlaceholder 0 ]
                      (CA.'try
                           p
                           {
                           , patternsAndExpressions = [ 'imm & CA.'patternLiteralText p "" & CA.'literalNumber p 1 ]
                           , value = CA.'variable p ('refPlaceholder 0)
                           }
                      )
                 )
            )
        ]


polymorphicUniques as Test =
    Test.'group
        """
        Polymorphic Uniques
        """
        [
        , codeTest
            """
            In pattern
            """
            """
            f =
                fn 1?a:
                1?b = a
                b
            """
            firstDefinitionStripDeps
            (Test.isOkAndEqualTo
                 {
                 , directDeps = Dict.empty
                 , maybeAnnotation = 'nothing
                 , maybeBody =
                     'just
                     << CA.'fn
                         p
                         [
                         , CA.'parameterPattern ('depends 1) (CA.'patternAny p ('just "a") 'nothing)
                         ]
                         (CA.'letIn
                              {
                              , body = CA.'variable p ('refLocal "a")
                              , pattern = CA.'patternAny p ('just "b") 'nothing
                              , uni = 'depends 1
                              }
                              (CA.'variable p ('refLocal "b"))
                         )
                 , name = "f"
                 , namePos = p
                 }
            )
        , codeTest
            """
            In annotation
            """
            """
            isOk as fn (fn 1?a: 2?Re error b), 1?Re error a: 2?Re error b = meh
            """
            (fn t:
                 t
                 >> firstDefinitionStripDeps
                 >> onOk fn def:
                 try def.maybeAnnotation as
                     ('just ann): 'ok ann.univars
                     _: 'err "no ann"
            )
            (Test.isOkAndEqualTo << Set.fromList [ 1, 2 ])
        ]


numbers as Test =
    Test.'group
        """
        Numbers
        """
        [
        , codeTest "Percent" "a = 1%" (firstEvaluation "a") (Test.isOkAndEqualTo << CA.'literalNumber p 0.01)
        , codeTest "Underscore" "a = 1_000_000" (firstEvaluation "a") (Test.isOkAndEqualTo << CA.'literalNumber p (1000 * 1000))
        ]


shadowing as Test =
    Test.'group
        """
        Shadowing
        """
        [
        , codeTest
            """
            Root
            """
            """
            a = 0
            a = 0
            """
            (firstEvaluation "a")
            (Test.errorContains [ "`a`" ])
        , codeTest
            """
            Local
            """
            """
            a = 0
            b =
                a = 0
                a + a
            """
            (firstEvaluation "b")
            (Test.errorContains [ "`a`" ])
        , codeTest
            """
            Function parameter
            """
            """
            a = 0
            b = fn a: a + a
            """
            (firstEvaluation "b")
            (Test.errorContains [ "`a`" ])
        , codeTest
            """
            try..as
            """
            """
            a = 0
            b = try x as
                 a: 0
            """
            (firstEvaluation "b")
            (Test.errorContains [ "`a`", "already been defined" ])
        , codeTest
            """
            Types
            """
            """
            var X = 'meh
            X = {}
            b = 0
            """
            (firstEvaluation "b")
            (Test.errorContains [ "X", "twice" ])
        , codeTest
            """
            Constructors
            """
            """
            var A = 'meh
            var B = 'meh
            b = 0
            """
            (firstEvaluation "b")
            (Test.errorContains [ "meh", "already been defined" ])
        ]
