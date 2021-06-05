module Compiler.TokensToFormattableAst_Test exposing (..)

import Compiler.TestHelpers
import Compiler.TokensToFormattableAst as Syntax
import Parser
import Prelude
import Test exposing (Test)
import Types.Binop as Binop
import Types.FormattableAst as FA
import Types.Literal
import Types.Token as Token exposing (Token)


tests : Test
tests =
    Test.Group "TokensToFormattableAst"
        [ errors
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


p : FA.Pos
p =
    ( -1, -1 )


applyDummyPos : FA.Pos -> FA.Pos
applyDummyPos =
    always p



----
---
--


codeTest =
    Test.codeTest Debug.toString


isOk =
    Test.isOk Debug.toString


simpleTest =
    Test.simple Debug.toString


kindToToken : Int -> Token.Kind -> Token
kindToToken index kind =
    { start = index
    , end = index + 1
    , kind = kind
    }


asDefinition : FA.Statement -> Result String FA.ValueDef
asDefinition s =
    case s of
        FA.Definition a ->
            Ok a

        _ ->
            Err "no def"


asEvaluation : FA.Statement -> Result String FA.Expression
asEvaluation s =
    case s of
        FA.Evaluation a ->
            Ok a

        _ ->
            Err "no eval"


runParser : Syntax.Parser a -> List Token -> Result String a
runParser parser ts =
    let
        ( failStates, outcome ) =
            Parser.parse (Syntax.discardSecond parser Parser.end) Syntax.unconsIgnoreComments ts
    in
    outcome
        |> Syntax.outcomeToResult "Test" ts failStates
        |> Compiler.TestHelpers.resErrorToString ""


firstStatement : String -> Result String FA.Statement
firstStatement code =
    code
        |> Compiler.TestHelpers.stringToFormattableModule
        |> Result.map (List.map (FA.posMap_statement applyDummyPos))
        |> Compiler.TestHelpers.resErrorToString code
        |> Result.andThen (List.head >> Result.fromMaybe "no head")


firstDefinition : String -> Result String FA.ValueDef
firstDefinition =
    firstStatement >> Result.andThen asDefinition


firstEvaluation : String -> Result String FA.Expression
firstEvaluation code =
    code
        |> firstDefinition
        |> Result.andThen (.body >> List.head >> Result.fromMaybe "empty body")
        |> Result.andThen asEvaluation


firstAnnotation : String -> Result String FA.Type
firstAnnotation code =
    code
        |> firstStatement
        |> Result.andThen asDefinition
        |> Result.andThen (.maybeAnnotation >> Result.fromMaybe "no annotation")



----
---
--


errors : Test
errors =
    Test.Group "Errors"
        [ codeTest "[reg] simple assignment, inline"
            """
            tests =
                as Test

                blah "StringToTokens"
                    [
                    , meh "keywords"
                        [
                        , codeTest
                            {
                            , name = "[reg] `fn` is a keyword"
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
            (Test.errContain
                """
  14 |                         , { end = 0, kind = Token.NewSiblingLine, start = 0 }
                                 ^
  """
            )
        ]



----
---
--


parens : Test
parens =
    Test.Group "Parens"
        [ codeTest "Can exist on multiple lines"
            """
          tests =
              (Ok
              )
          """
            firstDefinition
            Test.justOk
        ]



----
---
--


lambdas : Test
lambdas =
    Test.Group "lambdas"
        [ simpleTest
            { name = "inline nesting"
            , run =
                \_ ->
                    [ Token.Fn
                    , Token.Name { mutable = False } "a"
                    , Token.Colon
                    , Token.Fn
                    , Token.Name { mutable = False } "b"
                    , Token.Colon
                    , Token.NumberLiteral "3"
                    ]
                        |> List.indexedMap kindToToken
                        |> runParser Syntax.expr
                        |> Result.map (FA.posMap_expression applyDummyPos)
            , expected =
                Ok <|
                    FA.Lambda p
                        [ FA.PatternAny p False "a" ]
                        [ FA.Evaluation <|
                            FA.Lambda p
                                [ FA.PatternAny p False "b" ]
                                [ FA.Evaluation <| FA.Literal p <| Types.Literal.Number "3" ]
                        ]
            }
        , simpleTest
            { name = "block nesting"
            , run =
                \_ ->
                    [ Token.Fn
                    , Token.Name { mutable = False } "a"
                    , Token.Colon
                    , Token.BlockStart
                    , Token.Fn
                    , Token.Name { mutable = False } "b"
                    , Token.Colon
                    , Token.BlockStart
                    , Token.NumberLiteral "3"
                    , Token.BlockEnd
                    , Token.BlockEnd
                    ]
                        |> List.indexedMap kindToToken
                        |> runParser Syntax.expr
                        |> Result.map (FA.posMap_expression applyDummyPos)
            , expected =
                Ok <|
                    FA.Lambda p
                        [ FA.PatternAny p False "a" ]
                        [ FA.Evaluation <|
                            FA.Lambda p
                                [ FA.PatternAny p False "b" ]
                                [ FA.Evaluation <| FA.Literal p <| Types.Literal.Number "3" ]
                        ]
            }
        , simpleTest
            { name = "sibling nesting"
            , run =
                \_ ->
                    [ Token.Fn
                    , Token.Name { mutable = False } "a"
                    , Token.Colon
                    , Token.NewSiblingLine
                    , Token.Fn
                    , Token.Name { mutable = False } "b"
                    , Token.Colon
                    , Token.NewSiblingLine
                    , Token.NumberLiteral "3"
                    ]
                        |> List.indexedMap kindToToken
                        |> runParser Syntax.expr
                        |> Result.map (FA.posMap_expression applyDummyPos)
            , expected =
                Ok <|
                    FA.Lambda p
                        [ FA.PatternAny p False "a" ]
                        [ FA.Evaluation <|
                            FA.Lambda p
                                [ FA.PatternAny p False "b" ]
                                [ FA.Evaluation <|
                                    FA.Literal p (Types.Literal.Number "3")
                                ]
                        ]
            }
        ]


annotations : Test
annotations =
    Test.Group "Annotations"
        [ simpleTest
            { name = "Mutability 1"
            , run =
                \_ ->
                    firstAnnotation
                        """
                        a =
                          as Number @> Int -> None
                          1
                        """
            , expected =
                Ok
                    (FA.TypeFunction p
                        (FA.TypeName p "Number")
                        True
                        (FA.TypeFunction p
                            (FA.TypeName p "Int")
                            False
                            (FA.TypeName p "None")
                        )
                    )
            }
        , simpleTest
            { name = "Mutability 2"
            , run =
                \_ ->
                    firstAnnotation
                        """
                        a =
                          as Number -> Int @> None
                          1
                        """
            , expected =
                Ok
                    (FA.TypeFunction p
                        (FA.TypeName p "Number")
                        False
                        (FA.TypeFunction p
                            (FA.TypeName p "Int")
                            True
                            (FA.TypeName p "None")
                        )
                    )
            }
        , simpleTest
            { name = "Tuple precedence"
            , run =
                \_ ->
                    firstAnnotation
                        """
                        a =
                          as Int & Int -> Bool
                          a
                        """
            , expected =
                Ok
                    (FA.TypeFunction p
                        (FA.TypeTuple p
                            [ FA.TypeName p "Int"
                            , FA.TypeName p "Int"
                            ]
                        )
                        False
                        (FA.TypeName p "Bool")
                    )
            }
        ]


unionDefs : Test
unionDefs =
    let
        asTypeDef s =
            case s of
                FA.UnionDef a ->
                    Ok a

                _ ->
                    Err "no type def"

        firstTypeDef =
            firstStatement >> Result.andThen asTypeDef
    in
    Test.Group "Type Definitions"
        [ simpleTest
            { name = "Parse inline def"
            , run = \_ -> firstTypeDef "union A b c = V1 b, V2 c, V3, V4 b c"
            , expected =
                Ok
                    { args = [ "b", "c" ]
                    , constructors =
                        [ FA.TypePolymorphic p "V1" [ FA.TypeName p "b" ]
                        , FA.TypePolymorphic p "V2" [ FA.TypeName p "c" ]
                        , FA.TypeName p "V3"
                        , FA.TypePolymorphic p "V4" [ FA.TypeName p "b", FA.TypeName p "c" ]
                        ]
                    , name = "A"
                    }
            }
        , simpleTest
            { name = "Parse multiline def"
            , run =
                \_ ->
                    firstTypeDef
                        """
                        union A =
                           , V1
                           , V2
                        """
            , expected =
                { name = "A"
                , args = []
                , constructors =
                    [ FA.TypeName p "V1"
                    , FA.TypeName p "V2"
                    ]
                }
                    |> Ok
            }
        , simpleTest
            { name = "list argument"
            , run = \_ -> firstTypeDef "union A = A [Int]"
            , expected =
                Ok
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
            }
        ]


lists : Test
lists =
    Test.Group "Lists"
        [ simpleTest
            { name = "inline"
            , run = \_ -> firstEvaluation "a = [1, 2]"
            , expected =
                [ FA.Literal p <| Types.Literal.Number "1"
                , FA.Literal p <| Types.Literal.Number "2"
                ]
                    |> FA.List p
                    |> Ok
            }
        , simpleTest
            { name = "multiline canonical"
            , run = \_ -> firstEvaluation """
                 a =
                   [
                   , 1
                   , 2
                   ]
                 """
            , expected =
                [ FA.Literal p <| Types.Literal.Number "1"
                , FA.Literal p <| Types.Literal.Number "2"
                ]
                    |> FA.List p
                    |> Ok
            }
        , simpleTest
            { name = "multiline compact"
            , run = \_ -> firstEvaluation """
                 a = [
                   , 1
                   , 2
                   ]
                 """
            , expected =
                [ FA.Literal p <| Types.Literal.Number "1"
                , FA.Literal p <| Types.Literal.Number "2"
                ]
                    |> FA.List p
                    |> Ok
            }
        ]


records : Test
records =
    Test.Group "Records"
        [ simpleTest
            { name = "inline"
            , run = \_ -> firstEvaluation "a = { x = 1 }"
            , expected =
                Ok
                    (FA.Record p
                        { attrs = [ ( "x", Just (FA.Literal p <| Types.Literal.Number "1") ) ]
                        , extends = Nothing
                        }
                    )
            }
        , simpleTest
            { name = "multiline"
            , run =
                \_ ->
                    """
                    a =
                      {
                      , x = 1
                      , y = 2
                      }
                    """
                        |> firstEvaluation
            , expected =
                Ok
                    (FA.Record p
                        { attrs =
                            [ ( "x", Just (FA.Literal p <| Types.Literal.Number "1") )
                            , ( "y", Just (FA.Literal p <| Types.Literal.Number "2") )
                            ]
                        , extends = Nothing
                        }
                    )
            }
        , simpleTest
            { name = "multiline compact"
            , run =
                \_ ->
                    """
                    a = {
                      , x = 1
                      , y = 2
                      }
                    """
                        |> firstEvaluation
            , expected =
                Ok
                    (FA.Record p
                        { attrs =
                            [ ( "x", Just (FA.Literal p <| Types.Literal.Number "1") )
                            , ( "y", Just (FA.Literal p <| Types.Literal.Number "2") )
                            ]
                        , extends = Nothing
                        }
                    )
            }
        , simpleTest
            { name = "annotation, inline"
            , run =
                \_ ->
                    """
                    a =
                      as { x as Bool }
                      a
                    """
                        |> firstAnnotation
            , expected =
                Ok
                    (FA.TypeRecord p
                        { extends = Nothing
                        , attrs = [ ( "x", Just <| FA.TypeName p "Bool" ) ]
                        }
                    )
            }
        , simpleTest
            { name = "annotation, multiline"
            , run =
                \_ ->
                    """
                    a =
                      as
                       {
                       , x as Bool
                       }
                      a
                    """
                        |> firstAnnotation
            , expected =
                Ok
                    (FA.TypeRecord p
                        { extends = Nothing
                        , attrs = [ ( "x", Just <| FA.TypeName p "Bool" ) ]
                        }
                    )
            }
        , simpleTest
            { name = "annotation, multiline compact"
            , run =
                \_ ->
                    """
                    a =
                      as {
                       , x as Bool
                       }
                      a
                    """
                        |> firstAnnotation
            , expected =
                Ok
                    (FA.TypeRecord p
                        { extends = Nothing
                        , attrs = [ ( "x", Just <| FA.TypeName p "Bool" ) ]
                        }
                    )
            }
        , codeTest "[reg] simple assignment, inline"
            """
            a = { b with c }
            """
            firstDefinition
            Test.justOk
        , codeTest "[reg] simple assignment, as block"
            """
            a =
              { b with c }
            """
            firstDefinition
            Test.justOk
        , codeTest "[reg] simple assignment, as block"
            """
            a =
              { b with c = 1 }
            """
            firstDefinition
            Test.justOk
        ]


ifs : Test
ifs =
    Test.Group "Ifs"
        [ isOk
            { name = "inline"
            , run = \_ -> firstEvaluation "a = if a then b else c"
            }
        , isOk
            { name = "multiline, formatted"
            , run =
                \_ ->
                    """
                    x =
                      if a then
                        b
                      else
                        c
                    """
                        |> firstEvaluation
            }
        , isOk
            { name = "multiline, compact"
            , run =
                \_ ->
                    """
                    x =
                      if a then b
                      else c
                    """
                        |> firstEvaluation
            }
        ]


tries : Test
tries =
    Test.Group "Try"
        [ isOk
            { name = "inline"
            , run = \_ -> firstEvaluation "x = try a as b: c else d"
            }
        , isOk
            { name = "multiline, formatted"
            , run =
                \_ ->
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
                        |> firstEvaluation
            }
        , isOk
            { name = "multiline, compact"
            , run =
                \_ ->
                    """
                    x =
                      try a as
                        b: c
                        d: e
                        else f
                    """
                        |> firstEvaluation
            }
        ]


patterns : Test
patterns =
    Test.Group "Patterns"
        [ simpleTest
            { name = "list unpacking"
            , run = \_ -> firstDefinition "[a, b] = x" |> Result.map .pattern
            , expected = Ok <| FA.PatternList p [ FA.PatternAny p False "a", FA.PatternAny p False "b" ]
            }
        , isOk
            { name = "list unpacking, inner block"
            , run =
                \_ ->
                    """
x =
   [ a, b ] = c
                    """
                        |> firstDefinition
                        |> Result.map .pattern
            }
        , simpleTest
            { name = "record unpacking"
            , run = \_ -> firstDefinition "{ a, b } = x" |> Result.map .pattern
            , expected =
                Ok <|
                    FA.PatternRecord p
                        { extends = Nothing
                        , attrs =
                            [ ( "a", Nothing )
                            , ( "b", Nothing )
                            ]
                        }
            }
        , isOk
            { name = "record unpacking, inner block"
            , run =
                \_ ->
                    """
x =
  { a, b } = c
                    """
                        |> firstDefinition
                        |> Result.map .pattern
            }
        ]


binops : Test
binops =
    let
        sendBtoC b c =
            FA.Binop p
                Binop.Pipe
                ( FA.Variable p { isBinop = False } "b"
                , [ ( Prelude.sendRight
                    , FA.Variable p { isBinop = False } "c"
                    )
                  ]
                )

        sendBtoCtoD b c d =
            FA.Binop p
                Binop.Pipe
                ( FA.Variable p { isBinop = False } "b"
                , [ ( Prelude.sendRight
                    , FA.Variable p { isBinop = False } "c"
                    )
                  , ( Prelude.sendRight
                    , FA.Variable p { isBinop = False } "d"
                    )
                  ]
                )
    in
    Test.Group "Binops"
        [ codeTest "no indent"
            """
            a = b >> c
            """
            firstEvaluation
            (Test.okEqual <| sendBtoC 5 10)
        , codeTest "assignment indent"
            """
            a =
                b >> c
            """
            firstEvaluation
            (Test.okEqual <| sendBtoC 9 14)
        , codeTest "pipe indent"
            """
            a =
                b
                  >> c
            """
            firstEvaluation
            (Test.okEqual <| sendBtoC 9 20)
        , codeTest "pipe indent"
            """
            a =
                b
                  >> c
                  >> d
            """
            firstEvaluation
            (Test.okEqual <| sendBtoCtoD 9 20 31)
        , codeTest "pyramid indent"
            """
            a =
                b
                  >> c
                    >> d
            """
            firstEvaluation
            (Test.okEqual <| sendBtoCtoD 9 20 33)
        ]
