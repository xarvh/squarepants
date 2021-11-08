
tests =
    as Test

    Test.Group "Parser"
        [ errors
        , values
        , parens
        , lambdas
        , annotations
        , unionDefs
        , lists
        , records
        , ifs
        , tries
        , patterns
        , binops
        ]


p =
    Pos.T


codeTest =
    Test.codeTest Debug.toHuman


asDefinition s =
    as FA.Statement: Result Text FA.ValueDef
    try s as
        FA.Definition _ a:
            Ok a

        _:
            Err "Test says: no def"


asEvaluation s =
    as FA.Statement: Result Text FA.Expression
    try s as
        FA.Evaluation _ a:
            Ok a

        _:
            Err "Test says: no eval"


firstStatement code =
    grabFirst stats =
        try stats as
            []: Err "Test says: no statements"
            head :: tail: Ok head
    code
        >> Compiler/TestHelpers.textToFormattableModule
        >> Result.map (List.map (FA.posMap_statement fn _: p))
        >> Compiler/TestHelpers.resErrorToText code
        >> Result.andThen grabFirst


firstEvaluation code =
    as Text: Result Text FA.Expression
    code >> firstStatement >> Result.andThen asEvaluation


firstDefinition code =
    as Text: Result Text FA.ValueDef
    code >> firstStatement >> Result.andThen asDefinition


firstEvaluationOfDefinition code =
    as Text: Result Text FA.Expression

    grabFirst def =
        try def.body as
           []: Err "Test says: empty def body"
           head :: tail: Ok head

    code
        >> firstDefinition
        >> Result.andThen grabFirst
        >> Result.andThen asEvaluation


firstAnnotation code =
    as Text: Result Text FA.Type

    grabAnnotation def =
        as FA.ValueDef: Result Text FA.Type
        try def.maybeAnnotation as
            Nothing: Err "no annotation"
            Just ann: Ok ann.ty

    code
        >> firstStatement
        >> Result.andThen asDefinition
        # TODO test also the nonFn field!!!
        >> Result.andThen grabAnnotation


#
#
#


errors =
    as Test
    Test.Group "Errors"
        [
        , codeTest
            """
            [reg] error marker is completely off
            """
            """
            tests =
                as Test

                blah "StringToTokens"
                    [
                    , meh "keywords"
                        [
                        , codeTest
                            {
                            , name = "[reg] `fn` as a keyword"
                            , run = lexTokens "fn = 1"
                            , expected = Ok
                                    , { end = 0, kind = Token.NewSiblingLine, start = 0 }
                                    , { end = 2, kind = Token.Fn, start = 0 }
                                    , { end = 4, kind = Token.Defop { mutable = False }, start = 3 }
                                    , { end = 6, kind = Token.NumberLiteral "1", start = 5 }
                                    ]
                            }
                        ]
                    ]
            """
            firstDefinition
            (Test.errorContains
                [ """
  14 |                         , { end = 0, kind = Token.NewSiblingLine, start = 0 }
                                 ^
  """ ]
            )
        ]



#
#
#


values =
    as Test
    Test.Group "Values"
        [ codeTest "[reg] Unop"
            """
            r = f -n
            """
            firstDefinition
            Test.isOk
        ]



#
#
#


parens =
    as Test
    Test.Group "Parens"
        [ codeTest
            """
            Can exist on multiple lines
            """
            """
            tests =
                (Ok
                )
            """
            firstDefinition
            Test.isOk
        ]



#
#
#


lambdas =
    as Test
    Test.Group "lambdas"
        [ codeTest
            """
            Inline nesting
            """
            """
            fn a: fn b: 3
            """
            firstEvaluation
            (Test.isOkAndEqualTo <<
                FA.Lambda p
                    [ FA.PatternAny p False "a" ]
                    [ FA.Evaluation p <<
                        FA.Lambda p
                            [ FA.PatternAny p False "b" ]
                            [ FA.Evaluation p << FA.LiteralNumber p "3" ]
                    ]
            )
        , codeTest
            """
            Block nesting
            """
            """
            fn a:
              fn b:
                3
            """
            firstEvaluation
            (Test.isOkAndEqualTo <<
                FA.Lambda p
                    [ FA.PatternAny p False "a" ]
                    [ FA.Evaluation p <<
                        FA.Lambda p
                            [ FA.PatternAny p False "b" ]
                            [ FA.Evaluation p << FA.LiteralNumber p "3" ]
                    ]
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
                FA.Lambda p
                    [ FA.PatternAny p False "a" ]
                    [ FA.Evaluation p <<
                        FA.Lambda p
                            [ FA.PatternAny p False "b" ]
                            [ FA.Evaluation p << FA.LiteralNumber p "3"
                            ]
                    ]
            )
        , codeTest
            """
            Single tuple args can be unpacked without parens
            """
            """
            x =
              fn a & b: a
            """
            firstDefinition
            Test.isOk
        ]


annotations =
    as Test
    Test.Group "Annotations"
        [ codeTest "Mutability 1"
            """
            a =
              as Number @: Int: None
              1
            """
            firstAnnotation
            (Test.isOkAndEqualTo
                (FA.TypeFunction p
                    (FA.TypeName p "Number")
                    True
                    (FA.TypeFunction p
                        (FA.TypeName p "Int")
                        False
                        (FA.TypeName p "None")
                    )
                )
            )
        , codeTest "Mutability 2"
            """
                        a =
                          as Number: Int @: None
                          1
                        """
            firstAnnotation
            (Test.isOkAndEqualTo
                (FA.TypeFunction p
                    (FA.TypeName p "Number")
                    False
                    (FA.TypeFunction p
                        (FA.TypeName p "Int")
                        True
                        (FA.TypeName p "None")
                    )
                )
            )
        , codeTest "Tuple precedence"
            """
                        a =
                          as Int & Int: Bool
                          a
                        """
            firstAnnotation
            (Test.isOkAndEqualTo <<
                FA.TypeFunction p
                    (FA.TypeTuple p
                        [ FA.TypeName p "Int"
                        , FA.TypeName p "Int"
                        ]
                    )
                    False
                    (FA.TypeName p "Bool")
            )
        ]


unionDefs =
    as Test

    asTypeDef s =
        try s as
            FA.UnionDef _ a:
                Ok a

            _:
                Err "no type def"

    firstTypeDef =
        fn x: x >> firstStatement >> Result.andThen asTypeDef

    Test.Group "Type Definitions"
        [ codeTest "Parse inline def"
            "union A b c = V1 b, V2 c, V3, V4 b c"
            firstTypeDef
            (Test.isOkAndEqualTo
                { args = [ "b", "c" ]
                , constructors =
                    [ FA.TypePolymorphic p "V1" [ FA.TypeName p "b" ]
                    , FA.TypePolymorphic p "V2" [ FA.TypeName p "c" ]
                    , FA.TypeName p "V3"
                    , FA.TypePolymorphic p "V4" [ FA.TypeName p "b", FA.TypeName p "c" ]
                    ]
                , name = "A"
                }
            )
        , codeTest "Parse multiline def"
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
                    [ FA.TypeName p "V1"
                    , FA.TypeName p "V2"
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
                    [ FA.TypePolymorphic p
                        "A"
                        [ FA.TypePolymorphic p
                            "List"
                            [ FA.TypeName p "Int" ]
                        ]
                    ]
                }
            )
        ]


lists =
    as Test
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
        , codeTest "multiline compact"
            """
                 a = [
                   , 1
                   , 2
                   ]
                 """
            firstEvaluationOfDefinition
            ([ FA.LiteralNumber p "1", FA.LiteralNumber p "2" ] >> FA.List p >> Test.isOkAndEqualTo)
        ]


records =
    as Test
    Test.Group "Records"
        [ codeTest "inline"
            "a = { x = 1 }"
            firstEvaluationOfDefinition
            ({ attrs = [  "x" & (Just (FA.LiteralNumber p "1") ) ] , extends = Nothing } >> FA.Record p >> Test.isOkAndEqualTo)
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
                [ ( "x" & Just (FA.LiteralNumber p "1") )
                , ( "y" & Just (FA.LiteralNumber p "2") )
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
                [ ( "x" & Just (FA.LiteralNumber p "1") )
                , ( "y" & Just (FA.LiteralNumber p "2") )
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
            a =
              as { x as Bool }
              a
            """
            firstAnnotation
            ({ extends = Nothing , attrs = [  "x" & (Just << FA.TypeName p "Bool") ] } >> FA.TypeRecord p >> Test.isOkAndEqualTo)
        , codeTest
            """
            annotation, multiline
            """
            """
            a =
              as
               {
               , x as Bool
               }
              a
            """
            firstAnnotation
            ({ extends = Nothing , attrs = [  "x" & (Just << FA.TypeName p "Bool") ] } >> FA.TypeRecord p >> Test.isOkAndEqualTo)
        , codeTest
            """
            Annotation, multiline compact
            """
            """
            a =
              as {
               , x as Bool
               }
              a
            """
            firstAnnotation
            ({ extends = Nothing , attrs = [ "x" & (Just << FA.TypeName p "Bool") ] } >> FA.TypeRecord p >> Test.isOkAndEqualTo)
        , codeTest
            """
            [reg] simple assignment, inline
            """
            """
            a = { b with c }
            """
            firstDefinition
            Test.isOk
        , codeTest "[reg] simple assignment, as block"
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


ifs =
    as Test
    Test.Group "Ifs"
        [ codeTest "inline"
            "a = if a: b else c"
            firstEvaluationOfDefinition
            Test.isOk
        , codeTest "multiline, formatted"
            """
            x =
                if a:
                    b
                else
                    c
            """
            firstEvaluationOfDefinition
            Test.isOk
        , codeTest "multiline, compact"
            """
            x =
              if a: b
              else c
            """
            firstEvaluationOfDefinition
            Test.isOk
        ]


tries =
    as Test
    Test.Group "Try"
        [ codeTest "inline"
            "x = try a as b: c else d"
            firstEvaluationOfDefinition
            Test.isOk
        , codeTest "multiline, formatted"
            """
            x =
              try a as
                b:
                  c
                d:
                  e
                else
                  f
            """
            firstEvaluationOfDefinition
            Test.isOk
        , codeTest "multiline, compact"
            """
            x =
              try a as
                b: c
                d: e
                else f
            """
            firstEvaluationOfDefinition
            Test.isOk
        ]


patterns =
    as Test
    Test.Group "Patterns"
        [ codeTest "list unpacking"
            "[a, b] = x"
            (fn x: x >> firstDefinition >> Result.map fn y: y.pattern)
            ([ FA.PatternAny p False "a", FA.PatternAny p False "b" ]
                >> FA.PatternList p
                >> Test.isOkAndEqualTo
            )
        , codeTest "list unpacking, inner block"
            """
x =
   [ a, b ] = c
                    """
            (fn x: x >> firstDefinition >> Result.map fn y: y.pattern)
            Test.isOk
        , codeTest "record unpacking"
            "{ a, b } = x"
            (fn x: x >> firstDefinition >> Result.map fn y: y.pattern)
            ({ extends = Nothing , attrs = [ ( "a" & Nothing ) , ( "b" & Nothing ) ] } >> FA.PatternRecord p >> Test.isOkAndEqualTo)
        , codeTest "record unpacking, inner block"
            """
x =
  { a, b } = c
                    """
            (fn x: x >> firstDefinition >> Result.map fn y: y.pattern)
            Test.isOk
        ]


binops =
    as Test

    sendBtoC b c =
        FA.Binop p
            Op.Pipe
            ( FA.Variable p { isBinop = False } "b" & [ ( Prelude.sendRight & FA.Variable p { isBinop = False } "c") ])

    sendBtoCtoD b c d =
        FA.Binop p
            Op.Pipe
            ( FA.Variable p { isBinop = False } "b" & [ ( Prelude.sendRight & FA.Variable p { isBinop = False } "c") , ( Prelude.sendRight & FA.Variable p { isBinop = False } "d") ])

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

