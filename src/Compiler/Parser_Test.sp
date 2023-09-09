tests as Test =
    Test.'group
        "Parser"
        [
        , values
        , parens
        , functions
        , annotations
        , unionDefs
        , lists
        , records
        , ifs
        , tries
        , patterns
        , binops
        , unops
        , comments
        ]


word as fn Text: Pos & Name =
    fn name:
    Pos.'t & name


codeTest as fn Text, Text, fn Text: Result Text z, Test.CodeExpectation z: Test =
    Test.codeTest toHuman __ __ __ __


asDefinition as fn FA.Statement: Result Text { body as FA.Expression, nonFn as [ Pos & Name ], pattern as FA.Expression } =
    fn s:
    try s as
        FA.'valueDef a: 'ok a
        _: 'err "Test says: no def"


asEvaluation as fn FA.Statement: Result Text FA.Expression =
    fn s:
    try s as
        FA.'evaluation a: 'ok a
        _: 'err "Test says: no eval"


firstStatement as fn Text: Result Text FA.Statement =
    fn code:
    grabFirst =
        fn stats:
        try stats as
            []: 'err "Test says: no statements"
            head :: tail: 'ok head

    {
    , errorModule =
        {
        , content = code
        , fsPath = "Test"
        }
    , keepComments = 'true
    , stripLocations = 'true
    }
    >> Compiler/Parser.textToFormattableModule
    >> TH.resErrorToStrippedText
    >> onOk grabFirst


firstEvaluation as fn Text: Result Text FA.Expression =
    fn code:
    code >> firstStatement >> onOk asEvaluation


firstDefinition as fn Text: Result Text { body as FA.Expression, nonFn as [ Pos & Name ], pattern as FA.Expression } =
    fn code:
    code >> firstStatement >> onOk asDefinition


firstEvaluationOfDefinition as fn Text: Result Text FA.Expression =
    fn code:
    code
    >> firstStatement
    >> onOk asDefinition
    >> onOk (fn def: 'ok def.body)


values as Test =
    Test.'group
        "Values"
        [
        , codeTest
            """
            [reg] Parse root comments
            """
            """
            # blank
            a = 1
            """
            firstStatement
            Test.isOk
        , codeTest
            """
            [reg] deal with spurious NewSiblingLine introduced by inline comments
            """
            """
            library =
                # "spcore" is a special value for the core library
                source = "spcore"
            """
            firstDefinition
            Test.isOk
        , codeTest
            """
            [reg] SPON
            """
            """
            library =
                source = "core:prelude"

                module =
                    path = Core
                    importAs = Core
            """
            firstDefinition
            Test.isOk
        ]


#
#
#

parens as Test =
    Test.'group
        "Parens"
        [
        , codeTest
            """
            Can exist on multiple lines even when useless
            """
            """
            tests =
                (Ok
                )
            """
            firstDefinition
            Test.isOk
        , codeTest
            """
            Can exist on multiple lines
            """
            """
            tests =
                blah
                    (Ok
                    )
            """
            firstDefinition
            Test.isOk
        ]


#
#
#

e as fn FA.Expr_: FA.Expression =
    FA.'expression [] Pos.'t __


faBinop as fn Op.Binop: FA.Binop =
    fn op:
    {
    , comments = []
    , line = -1
    , pos = Pos.'t
    , precedence = op.precedence
    , symbol = op.symbol
    , usr = op.usr
    }


tuple as fn FA.Expression, FA.Expression: FA.Expression =
    fn a, b:
    FA.'binopChain Op.precedence_tuple (a & [ faBinop Prelude.tuple & b ]) >> e


lowercase as fn Name: FA.Expression =
    fn name:
    { attrPath = [], maybeModule = 'nothing, maybeType = 'nothing, name }
    >> FA.'lowercase
    >> e


uppercase as fn Name: FA.Expression =
    fn name:
    { maybeModule = 'nothing, name }
    >> FA.'uppercase
    >> e


constructor as fn Name: FA.Expression =
    fn name:
    { maybeModule = 'nothing, name }
    >> FA.'constructor
    >> e


annotatedVariable as fn Name, FA.Expression: FA.Expression =
    fn name, type:
    {
    , attrPath = []
    , maybeModule = 'nothing
    , maybeType = 'just type
    , name
    }
    >> FA.'lowercase
    >> e


functions as Test =
    Test.'group
        "functions"
        [
        , codeTest
            """
            Inline body
            """
            """
            fn a, b: 3
            """
            firstEvaluation
            (Test.isOkAndEqualTo
             << e
             << FA.'fn
                 FA.'inline
                 [
                 , lowercase "a"
                 , lowercase "b"
                 ]
                 (e << FA.'literalNumber 'false "3")
            )
        , codeTest
            """
            Indented body
            """
            """
            fn a:
                3
            """
            firstEvaluation
            (Test.isOkAndEqualTo
             << e
             << FA.'fn FA.'indented [ lowercase "a" ] (e << FA.'literalNumber 'false "3")
            )
        , codeTest
            """
            Sibling nesting
            """
            """
            z =
              fn a:
              fn b:
              3
            """
            firstEvaluationOfDefinition
            (Test.isOkAndEqualTo
             << e
             << FA.'fn FA.'aligned [ lowercase "a" ] (e << FA.'fn FA.'aligned [ lowercase "b" ] (e << FA.'literalNumber 'false "3"))
            )
        , codeTest
            """
            Tuple vs lambda precedence
            """
            """
            fn a & b: a
            """
            firstEvaluation
            (Test.isOkAndEqualTo
             << e
             << FA.'fn FA.'inline [ tuple (lowercase "a") (lowercase "b") ] (lowercase "a")
            )
        , codeTest
            """
            [reg] Pass to function without parens
            """
            """
            xxx fn y: y
            """
            firstEvaluation
            (Test.isOkAndEqualTo
             << e
             << FA.'call (lowercase "xxx") [ e << FA.'fn FA.'inline [ lowercase "y" ] (lowercase "y") ]
            )
        , codeTest
            """
            Pass to function without parens, below
            """
            """
            z =
              xxx fn y:
              y
            """
            firstEvaluationOfDefinition
            (Test.isOkAndEqualTo
             << e
             << FA.'call (lowercase "xxx") [ e << FA.'fn FA.'aligned [ lowercase "y" ] (lowercase "y") ]
            )
        , codeTest
            """
            SKIP Function should not swallow pipeline
            """
            """
            value
            >> map fn x: blah
            >> sblorp
            """
            firstEvaluation
            (Test.isOkAndEqualTo
             << e
             << FA.'binopChain Op.precedence_pipe __
             << lowercase "value"
             & [
             , faBinop Prelude.sendRight & e (FA.'call (lowercase "map") [ e << FA.'fn FA.'aligned [ lowercase "x" ] (lowercase "blah") ])
             , faBinop Prelude.sendRight & lowercase "sblorp"
             ]
            )
        , codeTest
            """
            [reg] Fn with multiple indented statements
            """
            """
            fn x:
              x
              x
            """
            firstEvaluation
            (Test.isOkAndEqualTo
             << e
             << FA.'fn
                 FA.'indented
                 [ lowercase "x" ]
                 (e
                  << FA.'statements
                      [
                      , FA.'evaluation << lowercase "x"
                      , FA.'evaluation << lowercase "x"
                      ]
                 )
            )
        , codeTest
            """
            [reg] Fn with definition
            """
            """
            a =
                fn x:
                y = 1
                x
            """
            firstDefinition
            (Test.isOkAndEqualTo
             << {
             , body =
                 e
                 << FA.'fn
                     FA.'aligned
                     [ lowercase "x" ]
                     (e
                      << FA.'statements
                          [
                          , FA.'valueDef { body = e << FA.'literalNumber 'false "1", nonFn = [], pattern = lowercase "y" }
                          , FA.'evaluation << lowercase "x"
                          ]
                     )
             , nonFn = []
             , pattern = lowercase "a"
             }
            )
        , codeTest
            """
            [reg] argument not being recognized?
            """
            """
            @b += 1
            """
            firstEvaluation
            (Test.isOkAndEqualTo
             << e
             << FA.'binopChain Op.precedence_mutop __
             << (e << FA.'unopCall Op.'unopRecycle __ << lowercase "b") & [ faBinop Prelude.mutableAdd & (e << FA.'literalNumber 'false "1") ]
            )
        , codeTest
            """
            [reg] multiple lines, compact
            """
            """
            x =
                a = pop a
                b = 1
            """
            firstDefinition
            Test.isOk
        ]


annotations as Test =
    Test.'group
        "Annotations"
        [
        , codeTest
            "Trivial case"
            """
            a as b =
                z
            """
            firstDefinition
            (Test.isOkAndEqualTo
                 {
                 , body = lowercase "z"
                 , nonFn = []
                 , pattern = annotatedVariable "a" (lowercase "b")
                 }
            )
        , codeTest
            "Tuple precedence"
            """
            a as fn int & int: bool =
                b
            """
            firstDefinition
            (Test.isOkAndEqualTo
                 {
                 , body = lowercase "b"
                 , nonFn = []
                 , pattern = annotatedVariable "a" (e << FA.'fn FA.'inline [ tuple (lowercase "int") (lowercase "int") ] (lowercase "bool"))
                 }
            )
        ]


unionDefs as Test =
    asTypeDef =
        fn s:
        try s as
            FA.'unionDef a: 'ok a
            _: 'err "no type def"

    firstTypeDef =
        fn x: x >> firstStatement >> onOk asTypeDef

    Test.'group
        """
        Type Definitions
        """
        [
        , codeTest
            """
            Parse inline def
            """
            """
            var A b c = 'v1 b, 'v2 c, 'v3, 'v4 b c
            """
            firstTypeDef
            (Test.isOkAndEqualTo
                 {
                 , args = [ word "b", word "c" ]
                 , constructors =
                     [
                     , e << FA.'call (constructor "'v1") [ lowercase "b" ]
                     , e << FA.'call (constructor "'v2") [ lowercase "c" ]
                     , constructor "'v3"
                     , e << FA.'call (constructor "'v4") [ lowercase "b", lowercase "c" ]
                     ]
                 , name = word "A"
                 }
            )
        , codeTest
            """
            Parse multiline def
            """
            """
            var A b c =
                , 'v1 b
                , 'v2 c
                , 'v3
                , 'v4 b c
            """
            firstTypeDef
            (Test.isOkAndEqualTo
                 {
                 , args = [ word "b", word "c" ]
                 , constructors =
                     [
                     , e << FA.'call (constructor "'v1") [ lowercase "b" ]
                     , e << FA.'call (constructor "'v2") [ lowercase "c" ]
                     , constructor "'v3"
                     , e << FA.'call (constructor "'v4") [ lowercase "b", lowercase "c" ]
                     ]
                 , name = word "A"
                 }
            )
        , codeTest
            "List argument"
            "var A = 'a [Int]"
            firstTypeDef
            (Test.isOkAndEqualTo
                 {
                 , args = []
                 , constructors =
                     [
                     , e
                     << FA.'call
                         (constructor "'a")
                         [
                         , e << FA.'list 'false [ 'false & uppercase "Int" ]
                         ]
                     ]
                 , name = word "A"
                 }
            )
        ]


lists as Test =
    Test.'group
        "Lists"
        [
        , codeTest
            """
            Empty
            """
            "[]"
            firstEvaluation
            (Test.isOkAndEqualTo (e << FA.'list 'false []))
        , codeTest
            """
            Inline
            """
            "[a, b]"
            firstEvaluation
            (Test.isOkAndEqualTo (e << FA.'list 'false [ 'false & lowercase "a", 'false & lowercase "b" ]))
        , codeTest
            """
            Multiline
            """
            """
            z =
              [
              , a
              , b
              ]
            """
            firstEvaluationOfDefinition
            (Test.isOkAndEqualTo (e << FA.'list 'true [ 'false & lowercase "a", 'false & lowercase "b" ]))
        , codeTest
            """
            Ancient egyptian
            """
            """
            z =
              blah [
              , a
              , b
              ]
            """
            firstEvaluationOfDefinition
            (Test.isOkAndEqualTo (e << FA.'call (lowercase "blah") [ e << FA.'list 'true [ 'false & lowercase "a", 'false & lowercase "b" ] ]))
        , codeTest
            """
            Dots
            """
            "[a..., b, c...]"
            firstEvaluation
            (Test.isOkAndEqualTo (e << FA.'list 'false [ 'true & lowercase "a", 'false & lowercase "b", 'true & lowercase "c" ]))
        ]


records as Test =
    Test.'group
        "Records"
        [
        , codeTest
            """
            Empty
            """
            "{}"
            firstEvaluation
            (Test.isOkAndEqualTo (e << FA.'record { attrs = [], isMultiline = 'false, maybeExtension = 'nothing }))
        , codeTest
            "Inline"
            "{ x = b }"
            firstEvaluation
            (Test.isOkAndEqualTo
                 (e
                  << FA.'record
                      {
                      , attrs =
                          [
                          , {
                          , maybeExpr = 'just << lowercase "b"
                          , name = lowercase "x"
                          }
                          ]
                      , isMultiline = 'false
                      , maybeExtension = 'nothing
                      }
                 )
            )
        , codeTest
            "Multiline"
            """
            z =
              {
              , x = a
              , y = b
              }
            """
            firstEvaluationOfDefinition
            (Test.isOkAndEqualTo
                 (e
                  << FA.'record
                      {
                      , attrs =
                          [
                          , {
                          , maybeExpr = 'just << lowercase "a"
                          , name = lowercase "x"
                          }
                          , {
                          , maybeExpr = 'just << lowercase "b"
                          , name = lowercase "y"
                          }
                          ]
                      , isMultiline = 'true
                      , maybeExtension = 'nothing
                      }
                 )
            )
        , codeTest
            "Pattern extension"
            """
            z =
              { with
              , x = a
              }
            """
            firstEvaluationOfDefinition
            (Test.isOkAndEqualTo
                 (e
                  << FA.'record
                      {
                      , attrs = [ { maybeExpr = 'just << lowercase "a", name = lowercase "x" } ]
                      , isMultiline = 'true
                      , maybeExtension = 'just 'nothing
                      }
                 )
            )
        , codeTest
            "Expression extension"
            """
            q =
              { z with
              , x = a
              }
            """
            firstEvaluationOfDefinition
            (Test.isOkAndEqualTo
                 (e
                  << FA.'record
                      {
                      , attrs = [ { maybeExpr = 'just << lowercase "a", name = lowercase "x" } ]
                      , isMultiline = 'true
                      , maybeExtension = 'just ('just (lowercase "z"))
                      }
                 )
            )
        , codeTest
            """
            Type or annotated implicit value
            """
            """
            { x as Bool }
            """
            firstEvaluation
            (Test.isOkAndEqualTo
                 (e
                  << FA.'record
                      {
                      , attrs = [ { maybeExpr = 'nothing, name = annotatedVariable "x" (uppercase "Bool") } ]
                      , isMultiline = 'false
                      , maybeExtension = 'nothing
                      }
                 )
            )
        , codeTest
            """
            Type or annotated explicit value
            """
            """
            { x as Bool = y }
            """
            firstEvaluation
            (Test.isOkAndEqualTo
                 (e
                  << FA.'record
                      {
                      , attrs = [ { maybeExpr = 'just << lowercase "y", name = annotatedVariable "x" (uppercase "Bool") } ]
                      , isMultiline = 'false
                      , maybeExtension = 'nothing
                      }
                 )
            )
        , codeTest
            "[reg] real-world use"
            """
            a =
              { state with
                  , pos = endPos
                  , code = rest
                  , accum =
                      [
                      , { kind = Token.Comment
                        , start = startPos
                        , end = endPos
                        }
                      , state.accum...
                      ]
              }
            """
            firstDefinition
            Test.isOk
        ]


ifs as Test =
    Test.'group
        "Ifs"
        [
        , codeTest
            """
            Inline
            """
            """
            if a then b else c
            """
            firstEvaluation
            (Test.isOkAndEqualTo
             << e
             << FA.'if
                 {
                 , condition = lowercase "a"
                 , false = lowercase "c"
                 , isMultiline = 'false
                 , true = lowercase "b"
                 }
            )
        , codeTest
            """
            Multiline, formatted
            """
            """
            z =
              if a then
                  b
              else
                  c
            """
            firstEvaluationOfDefinition
            (Test.isOkAndEqualTo
             << e
             << FA.'if
                 {
                 , condition = lowercase "a"
                 , false = lowercase "c"
                 , isMultiline = 'true
                 , true = lowercase "b"
                 }
            )
        , codeTest
            """
            Multiline, compact
            """
            """
            z =
              if a then b
              else c
            """
            firstEvaluationOfDefinition
            (Test.isOkAndEqualTo
             << e
             << FA.'if
                 {
                 , condition = lowercase "a"
                 , false = lowercase "c"
                 , isMultiline = 'true
                 , true = lowercase "b"
                 }
            )
        ]


tries as Test =
    Test.'group
        "Try"
        [
        , codeTest
            """
            Multiline, formatted
            """
            """
            try a as
               b:
                c
               d:
                e
            """
            firstEvaluation
            (Test.isOkAndEqualTo
             << e
             << FA.'try
                 {
                 , patterns =
                     [
                     , lowercase "b" & lowercase "c"
                     , lowercase "d" & lowercase "e"
                     ]
                 , value = lowercase "a"
                 }
            )
        , codeTest
            """
            Multiline, compact
            """
            """
            try a as
               b: c
               d: e
            """
            firstEvaluation
            (Test.isOkAndEqualTo
             << e
             << FA.'try
                 {
                 , patterns =
                     [
                     , lowercase "b" & lowercase "c"
                     , lowercase "d" & lowercase "e"
                     ]
                 , value = lowercase "a"
                 }
            )
        , codeTest
            """
            Nested
            """
            """
            try a as
               b:
                try c as
                     q:
                        q
               d:
                e
            """
            firstEvaluation
            (Test.isOkAndEqualTo
             << e
             << FA.'try
                 {
                 , patterns =
                     [
                     , lowercase "b"
                     & (e
                      << FA.'try
                          {
                          , patterns = [ lowercase "q" & lowercase "q" ]
                          , value = lowercase "c"
                          }
                     )
                     , lowercase "d" & lowercase "e"
                     ]
                 , value = lowercase "a"
                 }
            )
        ]


patterns as Test =
    Test.'group
        "Patterns"
        [
        , codeTest
            """
            List unpacking
            """
            "[a as Int, b] = x"
            firstDefinition
            (Test.isOkAndEqualTo
                 {
                 , body = lowercase "x"
                 , nonFn = []
                 , pattern =
                     e
                     << FA.'list
                         'false
                         [
                         , 'false & annotatedVariable "a" (uppercase "Int")
                         , 'false & lowercase "b"
                         ]
                 }
            )
        , codeTest
            """
            Record argument unpacking
            """
            """
            fn { with a, b }:
              x
            """
            firstEvaluation
            (Test.isOkAndEqualTo
             << e
             << FA.'fn
                 FA.'indented
                 [
                 , e
                 << FA.'record
                     {
                     , attrs =
                         [
                         , { maybeExpr = 'nothing, name = lowercase "a" }
                         , { maybeExpr = 'nothing, name = lowercase "b" }
                         ]
                     , isMultiline = 'false
                     , maybeExtension = 'just 'nothing
                     }
                 ]
                 (lowercase "x")
            )
        ]


binops as Test =
    sendBtoC =
        e << FA.'binopChain Op.precedence_pipe __ << lowercase "b" & [ faBinop Prelude.sendRight & lowercase "c" ]

    sendBtoCtoD =
        e << FA.'binopChain Op.precedence_pipe __ << lowercase "b" & [ faBinop Prelude.sendRight & lowercase "c", faBinop Prelude.sendRight & lowercase "d" ]

    Test.'group
        "Binops"
        [
        , codeTest
            "no indent"
            """
            b >> c
            """
            firstEvaluation
            (Test.isOkAndEqualTo sendBtoC)
        , codeTest
            """
            pipe indent 1
            """
            """
            b
              >> c
            """
            firstEvaluation
            (Test.isOkAndEqualTo sendBtoC)
        , codeTest
            """
            pipe indent 2
            """
            """
            b
              >> c
              >> d
            """
            firstEvaluation
            (Test.isOkAndEqualTo sendBtoCtoD)
        , codeTest
            """
            pyramid indent
            """
            """
            b
              >> c
                >> d
            """
            firstEvaluation
            (Test.isOkAndEqualTo sendBtoCtoD)
        , codeTest
            """
            no indent
            """
            """
            x =
                b
                >>
                c
            """
            firstEvaluationOfDefinition
            (Test.isOkAndEqualTo sendBtoC)
        , codeTest
            """
            SKIP (I'm tired) Starting
            """
            """
            x = >> c
            """
            firstEvaluationOfDefinition
            (Test.errorContains [ "TODO" ])
        , codeTest
            """
            SKIP (I'm tired) Double
            """
            """
            x = a >> >> c
            """
            firstEvaluationOfDefinition
            (Test.errorContains [ "TODO" ])
        , codeTest
            """
            SKIP (I'm tired) Ending
            """
            """
            x = a >>
            """
            firstEvaluationOfDefinition
            (Test.errorContains [ "TODO" ])
        ]


unops as Test =
    Test.'group
        "Unops"
        [
        , codeTest "[reg] Unop" "a = f -n" firstDefinition Test.isOk
        , codeTest
            """
            Precedence 1
            """
            """
            -a b
            """
            firstEvaluation
            (Test.isOkAndEqualTo (e << FA.'unopCall Op.'unopMinus (e << FA.'call (lowercase "a") [ lowercase "b" ])))
        , codeTest
            """
            Precedence 2
            """
            """
            a -b
            """
            firstEvaluation
            (Test.isOkAndEqualTo (e << FA.'call (lowercase "a") [ e << FA.'unopCall Op.'unopMinus (lowercase "b") ]))
        , codeTest
            """
            Precedence 3
            """
            """
            a -b c
            """
            firstEvaluation
            (Test.isOkAndEqualTo
                 (e
                  << FA.'call
                      (lowercase "a")
                      [
                      , e << FA.'unopCall Op.'unopMinus (lowercase "b")
                      , lowercase "c"
                      ]
                 )
            )
        ]


comments as Test =
    Test.'group
        "Comments"
        [
        , codeTest
            """
            [reg] trailing
            """
            """
            a =
              b
              # c
            """
            firstDefinition
            Test.isOk
        ]
