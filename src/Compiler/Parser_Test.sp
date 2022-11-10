
tests as Test =
    Test.Group "Parser"
        [
#        , values
#        , parens
        , functions
#        , annotations
#        , unionDefs
#        , lists
#        , records
#        , ifs
#        , tries
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


variable as Name: FA.Expression =
    name:

    {
    , maybeType = Nothing
    , word =
        {
        , modifier = Token.NameNoModifier
        , isUpper = False
        , maybeModule = Nothing
        , name
        , attrPath = []
        }
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

[#

annotations as Test =
    Test.Group "Annotations"
        [ codeTest "Mutability 1"
            """
            a as Number:- Int: None =
              1
            """
            firstAnnotation
            (Test.isOkAndEqualTo
                (FA.TypeFunction p
                    (typeConstant "Number")
                    LambdaConsuming
                    (FA.TypeFunction p
                        (typeConstant "Int")
                        LambdaNormal
                        (typeConstant "None")
                    )
                )
            )
        , codeTest "Mutability 2"
            """
            a as Number: Int:- None =
              1
            """
            firstAnnotation
            (Test.isOkAndEqualTo
                (FA.TypeFunction p
                    (typeConstant "Number")
                    LambdaNormal
                    (FA.TypeFunction p
                        (typeConstant "Int")
                        LambdaConsuming
                        (typeConstant "None")
                    )
                )
            )
        , codeTest "Tuple precedence"
            """
            a as Int & Int: Bool =
              1
            """
            firstAnnotation
            (Test.isOkAndEqualTo <<
                FA.TypeFunction p
                    (FA.TypeTuple p
                        [ typeConstant "Int"
                        , typeConstant "Int"
                        ]
                    )
                    LambdaNormal
                    (typeConstant "Bool")
            )
        ]


unionDefs as Test =

    asTypeDef = s:
        try s as
            FA.UnionDef _ a:
                Ok a

            _:
                Err "no type def"

    firstTypeDef =
        x: x >> firstStatement >> onOk asTypeDef

    Test.Group
        """
        Type Definitions
        """
        [ codeTest
            """
            Parse inline def
            """
            """
            union A b c = V1 b, V2 c, V3, V4 b c
            """
            firstTypeDef
            (Test.isOkAndEqualTo
                { args = [ "b", "c" ]
                , constructors =
                    [ At p "V1" & [ FA.TypeVariable p "b" ]
                    , At p "V2" & [ FA.TypeVariable p "c" ]
                    , At p "V3" & []
                    , At p "V4" & [ FA.TypeVariable p "b", FA.TypeVariable p "c" ]
                    ]
                , name = "A"
                }
            )
        , codeTest
            """
            Parse multiline def
            """
            """
            union A =
               , V1
               , V2
            """
            firstTypeDef
            (Test.isOkAndEqualTo
                { name = "A"
                , args = []
                , constructors =
                    [ At p "V1" & []
                    , At p "V2" & []
                    ]
                }
            )
        , codeTest "list argument"
            "union A = A [Int]"
            firstTypeDef
            (Test.isOkAndEqualTo
                { args = []
                , name = "A"
                , constructors =
                    [ At p "A" & [ FA.TypeList p ( typeConstant "Int") ]
                    ]
                }
            )
        ]


lists as Test =
    Test.Group "Lists"
        [ codeTest
            """
            Inline
            """
            "[1, 2]"
            firstEvaluation
            ([ FA.LiteralNumber p "1", FA.LiteralNumber p "2" ] >> FA.List p >> Test.isOkAndEqualTo)
        , codeTest
            """
            Multiline canonical
            """
            """
                 a =
                   [
                   , 1
                   , 2
                   ]
                 """
            firstEvaluationOfDefinition
            ([ FA.LiteralNumber p "1", FA.LiteralNumber p "2" ] >> FA.List p >> Test.isOkAndEqualTo)
        , codeTest
            """
            Multiline compact
            """
            """
                 a = [
                   , 1
                   , 2
                   ]
                 """
            firstEvaluationOfDefinition
            ([ FA.LiteralNumber p "1", FA.LiteralNumber p "2" ] >> FA.List p >> Test.isOkAndEqualTo)
        ]


records as Test =
    Test.Group "Records"
        [ codeTest "inline"
            "a = { x = 1 }"
            firstEvaluationOfDefinition
            ({ attrs = [ At p "x" & (Just (FA.LiteralNumber p "1") ) ] , extends = Nothing } >> FA.Record p >> Test.isOkAndEqualTo)
        , codeTest "multiline"
            """
            a =
              {
              , x = 1
              , y = 2
              }
            """
            firstEvaluationOfDefinition
            ({ attrs =
                [ ( At p "x") & Just (FA.LiteralNumber p "1")
                , ( At p "y") & Just (FA.LiteralNumber p "2")
                ]
            , extends = Nothing
            }
                >> FA.Record p
                >> Test.isOkAndEqualTo
            )
        , codeTest "multiline compact"
            """
            a = {
              , x = 1
              , y = 2
              }
            """
            firstEvaluationOfDefinition
            ({ attrs =
                [ (At p "x") & Just (FA.LiteralNumber p "1")
                , (At p "y") & Just (FA.LiteralNumber p "2")
                ]
            , extends = Nothing
            }
                >> FA.Record p
                >> Test.isOkAndEqualTo
            )
        , codeTest
            """
            Annotation, inline
            """
            """
            a as { x as Bool } =
              a
            """
            firstAnnotation
            ({ extends = Nothing , attrs = [ (At p "x") & (Just << typeConstant "Bool") ] } >> FA.TypeRecord p >> Test.isOkAndEqualTo)
        , codeTest
            """
            SKIP Annotation, own line
            """
            """
            a as
               { x as Bool }
               =
               1
            """
            firstAnnotation
            ({ extends = Nothing , attrs = [ (At p "x") & (Just << typeConstant "Bool") ] } >> FA.TypeRecord p >> Test.isOkAndEqualTo)
        , codeTest
            """
            SKIP Annotation, multiline
            """
            """
            a as {
               , x as Bool
               }
                  =
                  a
            """
            firstAnnotation
            ({ extends = Nothing, attrs = [ (At p "x") & (Just << typeConstant "Bool") ] } >> FA.TypeRecord p >> Test.isOkAndEqualTo)
        , codeTest
            """
            [reg] simple assignment, inline
            """
            """
            a = { b with c }
            """
            firstDefinition
            Test.isOk
        , codeTest
            """
            [reg] simple assignment, as block
            """
            """
            a =
              { b with c }
            """
            firstDefinition
            Test.isOk
        , codeTest "[reg] simple assignment, as block"
            """
            a =
              { b with c = 1 }
            """
            firstDefinition
            Test.isOk
        , codeTest "[reg] real-world use"
            """
            a =
              { state with
                  , pos = endPos
                  , code = rest
                  , accum =
                      { kind = Token.Comment
                      , start = startPos
                      , end = endPos
                      }
                          :: state.accum
              }
            """
            firstDefinition
            Test.isOk
        ]


ifs as Test =
    Test.Group "Ifs"
        [ codeTest "inline"
            "a = if a then b else c"
            firstEvaluationOfDefinition
            Test.isOk
        , codeTest
            """
            SKIP multiline, formatted
            """
            """
            x =
                if a then
                    b
                else
                    c
            """
            firstEvaluationOfDefinition
            Test.isOk
        , codeTest "multiline, compact"
            """
            x =
              if a then b
              else c
            """
            firstEvaluationOfDefinition
            Test.isOk
        ]


tries as Test =
    Test.Group "Try"
        [
        , codeTest "multiline, formatted"
            """
            x =
              try a as
                b:
                  c
                d:
                  e
            """
            firstEvaluationOfDefinition
            Test.isOk
        , codeTest "multiline, compact"
            """
            x =
              try a as
                b: c
                d: e
            """
            firstEvaluationOfDefinition
            Test.isOk
        ]


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
