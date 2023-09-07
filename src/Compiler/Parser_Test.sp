tests as Test =
    Test.Group
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
    Pos.T & name


codeTest as fn Text, Text, fn Text: Result Text z, Test.CodeExpectation z: Test =
    Test.codeTest toHuman __ __ __ __


asDefinition as fn FA.Statement: Result Text { body as FA.Expression, nonFn as [ Pos & Name ], pattern as FA.Expression } =
    fn s:
    try s as
        , FA.ValueDef a: Ok a
        , _: Err "Test says: no def"


asEvaluation as fn FA.Statement: Result Text FA.Expression =
    fn s:
    try s as
        , FA.Evaluation a: Ok a
        , _: Err "Test says: no eval"


firstStatement as fn Text: Result Text FA.Statement =
    fn code:
    grabFirst =
        fn stats:
        try stats as
            , []: Err "Test says: no statements"
            , head :: tail: Ok head

    {
    , errorModule =
        {
        , content = code
        , fsPath = "Test"
        }
    , keepComments = True
    , stripLocations = True
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
    >> onOk (fn def: Ok def.body)


values as Test =
    Test.Group
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
    Test.Group
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
    FA.Expression [] Pos.T __


faBinop as fn Op.Binop: FA.Binop =
    fn op:
    {
    , comments = []
    , line = -1
    , pos = Pos.T
    , precedence = op.precedence
    , symbol = op.symbol
    , usr = op.usr
    }


tuple as fn FA.Expression, FA.Expression: FA.Expression =
    fn a, b:
    FA.BinopChain Op.precedence_tuple (a & [ faBinop Prelude.tuple & b ]) >> e


lowercase as fn Name: FA.Expression =
    fn name:
    { attrPath = [], maybeModule = Nothing, maybeType = Nothing, name }
    >> FA.Lowercase
    >> e


uppercase as fn Name: FA.Expression =
    fn name:
    { maybeModule = Nothing, name }
    >> FA.Uppercase
    >> e


constructor as fn Name: FA.Expression =
    fn name:
    { maybeModule = Nothing, name }
    >> FA.Constructor
    >> e


annotatedVariable as fn Name, FA.Expression: FA.Expression =
    fn name, type:
    {
    , attrPath = []
    , maybeModule = Nothing
    , maybeType = Just type
    , name
    }
    >> FA.Lowercase
    >> e


functions as Test =
    Test.Group
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
             << FA.Fn
                 FA.Inline
                 [
                 , lowercase "a"
                 , lowercase "b"
                 ]
                 (e << FA.LiteralNumber False "3")
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
             << FA.Fn FA.Indented [ lowercase "a" ] (e << FA.LiteralNumber False "3")
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
             << FA.Fn FA.Aligned [ lowercase "a" ] (e << FA.Fn FA.Aligned [ lowercase "b" ] (e << FA.LiteralNumber False "3"))
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
             << FA.Fn FA.Inline [ tuple (lowercase "a") (lowercase "b") ] (lowercase "a")
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
             << FA.Call (lowercase "xxx") [ e << FA.Fn FA.Inline [ lowercase "y" ] (lowercase "y") ]
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
             << FA.Call (lowercase "xxx") [ e << FA.Fn FA.Aligned [ lowercase "y" ] (lowercase "y") ]
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
             << FA.BinopChain Op.precedence_pipe __
             << lowercase "value"
             & [
             , faBinop Prelude.sendRight & e (FA.Call (lowercase "map") [ e << FA.Fn FA.Aligned [ lowercase "x" ] (lowercase "blah") ])
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
             << FA.Fn
                 FA.Indented
                 [ lowercase "x" ]
                 (e
                  << FA.Statements
                      [
                      , FA.Evaluation << lowercase "x"
                      , FA.Evaluation << lowercase "x"
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
                 << FA.Fn
                     FA.Aligned
                     [ lowercase "x" ]
                     (e
                      << FA.Statements
                          [
                          , FA.ValueDef { body = e << FA.LiteralNumber False "1", nonFn = [], pattern = lowercase "y" }
                          , FA.Evaluation << lowercase "x"
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
             << FA.BinopChain Op.precedence_mutop __
             << (e << FA.UnopCall Op.UnopRecycle __ << lowercase "b") & [ faBinop Prelude.mutableAdd & (e << FA.LiteralNumber False "1") ]
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
    Test.Group
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
                 , pattern = annotatedVariable "a" (e << FA.Fn FA.Inline [ tuple (lowercase "int") (lowercase "int") ] (lowercase "bool"))
                 }
            )
        ]


unionDefs as Test =
    asTypeDef =
        fn s:
        try s as
            , FA.UnionDef a: Ok a
            , _: Err "no type def"

    firstTypeDef =
        fn x: x >> firstStatement >> onOk asTypeDef

    Test.Group
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
                     , e << FA.Call (constructor "'v1") [ lowercase "b" ]
                     , e << FA.Call (constructor "'v2") [ lowercase "c" ]
                     , constructor "'v3"
                     , e << FA.Call (constructor "'v4") [ lowercase "b", lowercase "c" ]
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
                     , e << FA.Call (constructor "'v1") [ lowercase "b" ]
                     , e << FA.Call (constructor "'v2") [ lowercase "c" ]
                     , constructor "'v3"
                     , e << FA.Call (constructor "'v4") [ lowercase "b", lowercase "c" ]
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
                     << FA.Call
                         (constructor "'a")
                         [
                         , e << FA.List False [ False & (uppercase "Int" ) ]
                         ]
                     ]
                 , name = word "A"
                 }
            )
        ]


lists as Test =
    Test.Group
        "Lists"
        [
        , codeTest
            """
            Empty
            """
            "[]"
            firstEvaluation
            (Test.isOkAndEqualTo (e << FA.List False []))
        , codeTest
            """
            Inline
            """
            "[a, b]"
            firstEvaluation
            (Test.isOkAndEqualTo (e << FA.List False [ False & lowercase "a", False & lowercase "b" ]))
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
            (Test.isOkAndEqualTo (e << FA.List True [ False & lowercase "a", False & lowercase "b" ]))
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
            (Test.isOkAndEqualTo (e << FA.Call (lowercase "blah") [ e << FA.List True [ False & lowercase "a", False & lowercase "b" ] ]))
        , codeTest
            """
            Dots
            """
            "[...a, b, ...c]"
            firstEvaluation
            (Test.isOkAndEqualTo (e << FA.List False [ True & lowercase "a", False & lowercase "b", True & lowercase "c" ]))
        ]


records as Test =
    Test.Group
        "Records"
        [
        , codeTest
            """
            Empty
            """
            "{}"
            firstEvaluation
            (Test.isOkAndEqualTo (e << FA.Record { attrs = [], isMultiline = False, maybeExtension = Nothing }))
        , codeTest
            "Inline"
            "{ x = b }"
            firstEvaluation
            (Test.isOkAndEqualTo
                 (e
                  << FA.Record
                      {
                      , attrs =
                          [
                          , {
                          , maybeExpr = Just << lowercase "b"
                          , name = lowercase "x"
                          }
                          ]
                      , isMultiline = False
                      , maybeExtension = Nothing
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
                  << FA.Record
                      {
                      , attrs =
                          [
                          , {
                          , maybeExpr = Just << lowercase "a"
                          , name = lowercase "x"
                          }
                          , {
                          , maybeExpr = Just << lowercase "b"
                          , name = lowercase "y"
                          }
                          ]
                      , isMultiline = True
                      , maybeExtension = Nothing
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
                  << FA.Record
                      {
                      , attrs = [ { maybeExpr = Just << lowercase "a", name = lowercase "x" } ]
                      , isMultiline = True
                      , maybeExtension = Just Nothing
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
                  << FA.Record
                      {
                      , attrs = [ { maybeExpr = Just << lowercase "a", name = lowercase "x" } ]
                      , isMultiline = True
                      , maybeExtension = Just (Just (lowercase "z"))
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
                  << FA.Record
                      {
                      , attrs = [ { maybeExpr = Nothing, name = annotatedVariable "x" (uppercase "Bool") } ]
                      , isMultiline = False
                      , maybeExtension = Nothing
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
                  << FA.Record
                      {
                      , attrs = [ { maybeExpr = Just << lowercase "y", name = annotatedVariable "x" (uppercase "Bool") } ]
                      , isMultiline = False
                      , maybeExtension = Nothing
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
                      , ...state.accum
                      ]
              }
            """
            firstDefinition
            Test.isOk
        ]


ifs as Test =
    Test.Group
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
             << FA.If
                 {
                 , condition = lowercase "a"
                 , false = lowercase "c"
                 , isMultiline = False
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
             << FA.If
                 {
                 , condition = lowercase "a"
                 , false = lowercase "c"
                 , isMultiline = True
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
             << FA.If
                 {
                 , condition = lowercase "a"
                 , false = lowercase "c"
                 , isMultiline = True
                 , true = lowercase "b"
                 }
            )
        ]


tries as Test =
    Test.Group
        "Try"
        [
        , codeTest
            """
            Multiline, formatted
            """
            """
            try a as
              , b:
                c
              , d:
                e
            """
            firstEvaluation
            (Test.isOkAndEqualTo
             << e
             << FA.Try
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
              , b: c
              , d: e
            """
            firstEvaluation
            (Test.isOkAndEqualTo
             << e
             << FA.Try
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
              , b:
                try c as
                    , q:
                        q
              , d:
                e
            """
            firstEvaluation
            (Test.isOkAndEqualTo
             << e
             << FA.Try
                 {
                 , patterns =
                     [
                     , lowercase "b"
                     & (e
                      << FA.Try
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
    Test.Group
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
                     << FA.List
                         False
                         [
                         , False & annotatedVariable "a" (uppercase "Int")
                         , False & lowercase "b"
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
             << FA.Fn
                 FA.Indented
                 [
                 , e
                 << FA.Record
                     {
                     , attrs =
                         [
                         , { maybeExpr = Nothing, name = lowercase "a" }
                         , { maybeExpr = Nothing, name = lowercase "b" }
                         ]
                     , isMultiline = False
                     , maybeExtension = Just Nothing
                     }
                 ]
                 (lowercase "x")
            )
        ]


binops as Test =
    sendBtoC =
        e << FA.BinopChain Op.precedence_pipe __ << lowercase "b" & [ faBinop Prelude.sendRight & lowercase "c" ]

    sendBtoCtoD =
        e << FA.BinopChain Op.precedence_pipe __ << lowercase "b" & [ faBinop Prelude.sendRight & lowercase "c", faBinop Prelude.sendRight & lowercase "d" ]

    Test.Group
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
    Test.Group
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
            (Test.isOkAndEqualTo (e << FA.UnopCall Op.UnopMinus (e << FA.Call (lowercase "a") [ lowercase "b" ])))
        , codeTest
            """
            Precedence 2
            """
            """
            a -b
            """
            firstEvaluation
            (Test.isOkAndEqualTo (e << FA.Call (lowercase "a") [ e << FA.UnopCall Op.UnopMinus (lowercase "b") ]))
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
                  << FA.Call
                      (lowercase "a")
                      [
                      , e << FA.UnopCall Op.UnopMinus (lowercase "b")
                      , lowercase "c"
                      ]
                 )
            )
        ]


comments as Test =
    Test.Group
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
