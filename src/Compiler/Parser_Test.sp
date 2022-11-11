
tests as Test =
    Test.Group "Parser"
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
#        , patterns
#        , binops
        ]


p as Pos =
    Pos.T


codeTest =
    Test.codeTest toHuman


asDefinition as FA.Statement: Result Text { pattern as FA.Expression, nonFn as [At Token.Word], body as FA.Expression } =
    s:
    try s as
        FA.ValueDef a:
            Ok a

        _:
            Err "Test says: no def"


asEvaluation as FA.Statement: Result Text FA.Expression =
    s:
    try s as
        FA.Evaluation a:
            Ok a

        _:
            Err "Test says: no eval"


firstStatement as Text: Result Text FA.Statement =
    code:

    grabFirst =
        stats:
        try stats as
            []: Err "Test says: no statements"
            head :: tail: Ok head

    code
        >> Compiler/Parser.textToFormattableModule {
            , stripLocations = True
            , moduleName = "Test"
            }
        >> TH.resErrorToStrippedText code
        >> onOk grabFirst


firstEvaluation as Text: Result Text FA.Expression =
    code:
    code >> firstStatement >> onOk asEvaluation


firstDefinition as Text: Result Text { pattern as FA.Expression, nonFn as [At Token.Word], body as FA.Expression } =
    code:
    code >> firstStatement >> onOk asDefinition


#firstEvaluationOfDefinition as Text: Result Text FA.Expression =
#    code:
#
#    grabFirst = def:
#        try def.body as
#           []: Err "Test says: empty def body"
#           head :: tail: Ok head
#
#    code
#        >> firstDefinition
#        >> onOk grabFirst
#        >> onOk asEvaluation
#
#
#firstAnnotation as Text: Result Text FA.Expression =
#    code:
#
#    grabAnnotation = #as FA.ValueDef: Result Text FA.Type =
#        def:
#        try def.pattern as
#            #FA.PatternAny pos name mutable (Just ty): Ok ty
#            _: Err "no annotation"
#
#    code
#        >> firstStatement
#        >> onOk asDefinition
#        # TODO test also the nonFn field!!!
#        >> onOk grabAnnotation
#
#
#typeConstant as Text: FA.Expression =
#    name:
#    FA.TypeConstant p Nothing name []
#
#
#
#
#


values as Test =
    Test.Group "Values"
        [
        , codeTest "[reg] Unop"
            "a = f -n"
            firstDefinition
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
        ]


#
#
#


parens as Test =
    Test.Group "Parens"
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

e as FA.Expr_: FA.Expression =
    FA.Expression p


tuple as FA.Expression: FA.Expression: FA.Expression =
    a: b:

    e << FA.Binop Op.Tuple (a & [Prelude.tuple & b])


word as Name: Token.Word =
    name:
    {
    , modifier = Token.NameNoModifier
    , isUpper = False
    , maybeModule = Nothing
    , name
    , attrPath = []
    }


variable as Name: FA.Expression =
    name:
    {
    , maybeType = Nothing
    , word = word name
    }
    >> FA.Variable
    >> e


annotatedVariable as Name: FA.Expression: FA.Expression =
    name: type:
    {
    , maybeType = Just type
    , word = word name
    }
    >> FA.Variable
    >> e


functions as Test =
    Test.Group "functions"
        [
        , codeTest
            """
            Inline body
            """
            """
            fn a, b: 3
            """
            firstEvaluation
            (Test.isOkAndEqualTo <<
                e << FA.Fn
                    [
                    , variable "a"
                    , variable "b"
                    ]
                    ( e << FA.LiteralNumber "3" )
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
            (Test.isOkAndEqualTo <<
                e << FA.Fn
                    [ variable "a" ]
                    ( e << FA.LiteralNumber "3" )
            )
        , codeTest
            """
            Sibling nesting
            """
            """
            fn a:
            fn b:
            3
            """
            firstEvaluation
            (Test.isOkAndEqualTo <<
                e << FA.Fn
                    [ variable "a" ]
                    (e << FA.Fn
                        [ variable "b" ]
                        ( e << FA.LiteralNumber "3" )
                    )
            )
        , codeTest
            """
            Tuple vs lambda precedence
            """
            """
            fn a & b: a
            """
            firstEvaluation
            (Test.isOkAndEqualTo <<
                e << FA.Fn
                    [ tuple (variable "a") (variable "b") ]
                    ( variable "a" )
            )
        , codeTest
            """
            [reg] Pass to function without parens
            """
            """
            xxx fn y: y
            """
            firstEvaluation
            (Test.isOkAndEqualTo <<
                e << FA.Call
                    ( variable "xxx" )
                    [ e << FA.Fn [ variable "y" ] (variable "y") ]
            )
        , codeTest
            """
            Pass to function without parens, below
            """
            """
            xxx fn y:
            y
            """
            firstEvaluation
            (Test.isOkAndEqualTo <<
                e << FA.Call
                    ( variable "xxx" )
                    [ e << FA.Fn [ variable "y" ] (variable "y") ]
            )
        ]


annotations as Test =
    Test.Group "Annotations"
        [
        , codeTest "Trivial case"
            """
            a as b =
                z
            """
            firstDefinition
            (Test.isOkAndEqualTo
                {
                , pattern = annotatedVariable "a" (variable "b")
                , nonFn = []
                , body = variable "z"
                }
            )
        , codeTest "Tuple precedence"
            """
            a as fn int & int: bool =
                b
            """
            firstDefinition
            (Test.isOkAndEqualTo
                {
                , pattern =
                    annotatedVariable "a"
                        (e << FA.Fn
                            [tuple (variable "int") (variable "int")]
                            (variable "bool")
                        )
                , nonFn = []
                , body = variable "b"
                }
            )
        ]


unionDefs as Test =

    asTypeDef = s:
        try s as
            FA.UnionDef a:
                Ok a

            _:
                Err "no type def"

    firstTypeDef =
        x: x >> firstStatement >> onOk asTypeDef

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
            union a b c = v1 b, v2 c, v3, v4 b c
            """
            firstTypeDef
            (Test.isOkAndEqualTo
                {
                , name = At p (word "a")
                , args = [ At p (word "b"), At p (word "c") ]
                , constructors =
                    [
                    , e << FA.Call (variable "v1") [ variable "b" ]
                    , e << FA.Call (variable "v2") [ variable "c" ]
                    , variable "v3"
                    , e << FA.Call (variable "v4") [ variable "b", variable "c" ]
                    ]
                }
            )
        , codeTest
            """
            Parse multiline def
            """
            """
            union a b c =
                , v1 b
                , v2 c
                , v3
                , v4 b c
            """
            firstTypeDef
            (Test.isOkAndEqualTo
                {
                , name = At p (word "a")
                , args = [ At p (word "b"), At p (word "c") ]
                , constructors =
                    [
                    , e << FA.Call (variable "v1") [ variable "b" ]
                    , e << FA.Call (variable "v2") [ variable "c" ]
                    , variable "v3"
                    , e << FA.Call (variable "v4") [ variable "b", variable "c" ]
                    ]
                }
            )
        , codeTest "list argument"
            "union a = a [int]"
            firstTypeDef
            (Test.isOkAndEqualTo
                {
                , name = At p (word "a")
                , args = []
                , constructors = [ e << FA.Call (variable "a") [ e << FA.List [ False & variable "int" ] ]]
                }
            )
        ]


lists as Test =
    Test.Group "Lists"
        [
        , codeTest
            """
            Inline
            """
            "[a, b]"
            firstEvaluation
            (Test.isOkAndEqualTo
              (e << FA.List [ False & variable "a", False & variable "b" ])
            )
        , codeTest
            """
            Multiline
            """
            """
            [
            , a
            , b
            ]
            """
            firstEvaluation
            (Test.isOkAndEqualTo
              (e << FA.List [ False & variable "a", False & variable "b" ])
            )
        , codeTest
            """
            Ancient egyptian
            """
            """
            blah [
            , a
            , b
            ]
            """
            firstEvaluation
            (Test.isOkAndEqualTo
              (e << FA.Call (variable "blah") [e << FA.List [ False & variable "a", False & variable "b" ]])
            )
        , codeTest
            """
            Dots
            """
            "[...a, b, ...c]"
            firstEvaluation
            (Test.isOkAndEqualTo
              (e << FA.List [ True & variable "a", False & variable "b", True & variable "c" ])
            )
        ]


records as Test =
    Test.Group "Records"
        [
        , codeTest "Inline"
            "{ x = b }"
            firstEvaluation
            (Test.isOkAndEqualTo
              (e << FA.Record
                  {
                  , maybeExtension = Nothing
                  , attrs =
                      [{
                      , name = variable "x"
                      , maybeExpr = Just << variable "b"
                      }]
                  }
              )
            )
        , codeTest "Multiline"
            """
            {
            , x = a
            , y = b
            }
            """
            firstEvaluation
            (Test.isOkAndEqualTo
              (e << FA.Record
                  {
                  , maybeExtension = Nothing
                  , attrs =
                      [
                      , {
                        , name = variable "x"
                        , maybeExpr = Just << variable "a"
                        }
                      , {
                        , name = variable "y"
                        , maybeExpr = Just << variable "b"
                        }
                      ]
                  }
              )
            )
        , codeTest "Pattern extension"
            """
            { with
            , x = a
            }
            """
            firstEvaluation
            (Test.isOkAndEqualTo
              (e << FA.Record
                  {
                  , maybeExtension = Just Nothing
                  , attrs = [ { name = variable "x", maybeExpr = Just << variable "a" } ]
                  }
              )
            )
        , codeTest "Expression extension"
            """
            { z with
            , x = a
            }
            """
            firstEvaluation
            (Test.isOkAndEqualTo
              (e << FA.Record
                  {
                  , maybeExtension = Just (Just (variable "z"))
                  , attrs = [ { name = variable "x", maybeExpr = Just << variable "a" } ]
                  }
              )
            )
        , codeTest
            """
            Type or annotated implicit value
            """
            """
            { x as bool }
            """
            firstEvaluation
            (Test.isOkAndEqualTo
              (e << FA.Record
                  {
                  , maybeExtension = Nothing
                  , attrs = [ { name = annotatedVariable "x" (variable "bool"), maybeExpr = Nothing } ]
                  }
              )
            )
        , codeTest
            """
            Type or annotated explicit value
            """
            """
            { x as bool = y }
            """
            firstEvaluation
            (Test.isOkAndEqualTo
              (e << FA.Record
                  {
                  , maybeExtension = Nothing
                  , attrs = [ { name = annotatedVariable "x" (variable "bool"), maybeExpr = Just << variable "y" } ]
                  }
              )
            )
        , codeTest "[reg] real-world use"
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
    Test.Group "Ifs"
        [ codeTest
            """
            Inline
            """
            """
            if a then b else c
            """
            firstEvaluation
            (Test.isOkAndEqualTo << e <<
                FA.If
                    {
                    , condition = variable "a"
                    , true = variable "b"
                    , false = variable "c"
                    }
            )
        , codeTest
            """
            Multiline, formatted
            """
            """
            if a then
                b
            else
                c
            """
            firstEvaluation
            (Test.isOkAndEqualTo << e <<
                FA.If
                    {
                    , condition = variable "a"
                    , true = variable "b"
                    , false = variable "c"
                    }
            )
        , codeTest
            """
            Multiline, compact
            """
            """
            if a then b
            else c
            """
            firstEvaluation
            (Test.isOkAndEqualTo << e <<
                FA.If
                    {
                    , condition = variable "a"
                    , true = variable "b"
                    , false = variable "c"
                    }
            )
        ]


tries as Test =
    Test.Group "Try"
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
            (Test.isOkAndEqualTo << e <<
                FA.Try
                    {
                    , value = variable "a"
                    , patterns =
                        [
                        , variable "b" & variable "c"
                        , variable "d" & variable "e"
                        ]
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
            (Test.isOkAndEqualTo << e <<
                FA.Try
                    {
                    , value = variable "a"
                    , patterns =
                        [
                        , variable "b" & variable "c"
                        , variable "d" & variable "e"
                        ]
                    }
            )
        ]


[#
patterns as Test =
    Test.Group "Patterns"
        [ codeTest "list unpacking"
            "[a, b] = x"
            (x: x >> firstDefinition >> Result.map y: y.pattern)
            ([ FA.PatternAny p False "a" Nothing, FA.PatternAny p False "b" Nothing ]
                >> FA.PatternList p
                >> Test.isOkAndEqualTo
            )
        , codeTest "list unpacking, inner block"
            """
x =
   [ a, b ] = c
                    """
            (x: x >> firstDefinition >> Result.map y: y.pattern)
            Test.isOk
        , codeTest "record unpacking"
            "{ a, b } = x"
            (x: x >> firstDefinition >> Result.map y: y.pattern)
            ({ extends = Nothing , attrs = [ At p "a" & Nothing, At p "b" & Nothing  ] } >> FA.PatternRecord p >> Test.isOkAndEqualTo)
        , codeTest "record unpacking, inner block"
            """
x =
  { a, b } = c
                    """
            (x: x >> firstDefinition >> Result.map y: y.pattern)
            Test.isOk
        ]


binops as Test =

    sendBtoC = b: c:
        FA.Binop p
            Op.Pipe
            ( FA.Variable p Nothing "b" [] & [ ( Prelude.sendRight & FA.Variable p Nothing "c" []) ])

    sendBtoCtoD = b: c: d:
        FA.Binop p
            Op.Pipe
            ( FA.Variable p Nothing "b" [] & [ ( Prelude.sendRight & FA.Variable p Nothing "c" []) , ( Prelude.sendRight & FA.Variable p Nothing "d" []) ])

    Test.Group "Binops"
        [ codeTest "no indent"
            """
            a = b >> c
            """
            firstEvaluationOfDefinition
            (Test.isOkAndEqualTo << sendBtoC 5 10)
        , codeTest "assignment indent"
            """
            a =
                b >> c
            """
            firstEvaluationOfDefinition
            (Test.isOkAndEqualTo << sendBtoC 9 14)
        , codeTest "pipe indent"
            """
            a =
                b
                  >> c
            """
            firstEvaluationOfDefinition
            (Test.isOkAndEqualTo << sendBtoC 9 20)
        , codeTest "pipe indent"
            """
            a =
                b
                  >> c
                  >> d
            """
            firstEvaluationOfDefinition
            (Test.isOkAndEqualTo << sendBtoCtoD 9 20 31)
        , codeTest "pyramid indent"
            """
            a =
                b
                  >> c
                    >> d
            """
            firstEvaluationOfDefinition
            (Test.isOkAndEqualTo << sendBtoCtoD 9 20 33)
        ]

#]
