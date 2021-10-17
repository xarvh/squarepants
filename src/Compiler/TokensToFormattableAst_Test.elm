module Compiler.TokensToFormattableAst_Test exposing (..)

import Compiler.TestHelpers
import Compiler.TokensToFormattableAst as Syntax
import Parser
import Prelude
import Test exposing (Test)
import Types.FormattableAst as FA
import Types.Literal
import Types.Op as Op
import Types.Token as Token exposing (Token)


tests : Test
tests =
    Test.Group "TokensToFormattableAst"
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


nonCodeTest { name, run, code, expected } =
    -- TODO clean up this HACK
    Test.codeTest Debug.toString name (Debug.toString code) (\_ -> run code) (Test.okEqual expected)


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
        -- TODO test also the nonFn field!!!
        |> Result.andThen (.maybeAnnotation >> Maybe.map .ty >> Result.fromMaybe "no annotation")



----
---
--


errors : Test
errors =
    Test.Group "Errors"
        [ codeTest "[reg] simple assignment, inline"
            """
            tests =
                is Test

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


values : Test
values =
    Test.Group "Values"
        [ codeTest "[reg] Unop"
            """
            r = f -n
            """
            firstDefinition
            Test.isOk
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
            Test.isOk
        ]



----
---
--


lambdas : Test
lambdas =
    Test.Group "lambdas"
        [ nonCodeTest
            { name = "inline nesting"
            , code =
                [ Token.Fn
                , Token.Name { mutable = False } "a"
                , Token.Colon
                , Token.Fn
                , Token.Name { mutable = False } "b"
                , Token.Colon
                , Token.NumberLiteral "3"
                ]
            , run =
                List.indexedMap kindToToken
                    >> runParser Syntax.expr
                    >> Result.map (FA.posMap_expression applyDummyPos)
            , expected =
                FA.Lambda p
                    [ FA.PatternAny p False "a" ]
                    [ FA.Evaluation <|
                        FA.Lambda p
                            [ FA.PatternAny p False "b" ]
                            [ FA.Evaluation <| FA.Literal p <| Types.Literal.Number "3" ]
                    ]
            }
        , nonCodeTest
            { name = "block nesting"
            , code =
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
            , run =
                \s ->
                    s
                        |> List.indexedMap kindToToken
                        |> runParser Syntax.expr
                        |> Result.map (FA.posMap_expression applyDummyPos)
            , expected =
                FA.Lambda p
                    [ FA.PatternAny p False "a" ]
                    [ FA.Evaluation <|
                        FA.Lambda p
                            [ FA.PatternAny p False "b" ]
                            [ FA.Evaluation <| FA.Literal p <| Types.Literal.Number "3" ]
                    ]
            }
        , nonCodeTest
            { name = "sibling nesting"
            , code =
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
            , run =
                \s ->
                    s
                        |> List.indexedMap kindToToken
                        |> runParser Syntax.expr
                        |> Result.map (FA.posMap_expression applyDummyPos)
            , expected =
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


annotations : Test
annotations =
    Test.Group "Annotations"
        [ codeTest "Mutability 1"
            """
            a =
              is Number @> Int -> None
              1
            """
            firstAnnotation
            (Test.okEqual
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
                          is Number -> Int @> None
                          1
                        """
            firstAnnotation
            (Test.okEqual
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
                          is Int & Int -> Bool
                          a
                        """
            firstAnnotation
            (Test.okEqual <|
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
        [ codeTest "Parse inline def"
            "union A b c = V1 b, V2 c, V3, V4 b c"
            firstTypeDef
            (Test.okEqual
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
            (Test.okEqual
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
            (Test.okEqual
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


lists : Test
lists =
    Test.Group "Lists"
        [ codeTest "inline"
            "a = [1, 2]"
            firstEvaluation
            ([ FA.Literal p <| Types.Literal.Number "1"
             , FA.Literal p <| Types.Literal.Number "2"
             ]
                |> FA.List p
                |> Test.okEqual
            )
        , codeTest "multiline canonical"
            """
                 a =
                   [
                   , 1
                   , 2
                   ]
                 """
            firstEvaluation
            ([ FA.Literal p <| Types.Literal.Number "1"
             , FA.Literal p <| Types.Literal.Number "2"
             ]
                |> FA.List p
                |> Test.okEqual
            )
        , codeTest "multiline compact"
            """
                 a = [
                   , 1
                   , 2
                   ]
                 """
            firstEvaluation
            ([ FA.Literal p <| Types.Literal.Number "1"
             , FA.Literal p <| Types.Literal.Number "2"
             ]
                |> FA.List p
                |> Test.okEqual
            )
        ]


records : Test
records =
    Test.Group "Records"
        [ codeTest "inline"
            "a = { x = 1 }"
            firstEvaluation
            ({ attrs = [ ( "x", Just (FA.Literal p <| Types.Literal.Number "1") ) ]
             , extends = Nothing
             }
                |> FA.Record p
                |> Test.okEqual
            )
        , codeTest "multiline"
            """
                    a =
                      {
                      , x = 1
                      , y = 2
                      }
                    """
            firstEvaluation
            ({ attrs =
                [ ( "x", Just (FA.Literal p <| Types.Literal.Number "1") )
                , ( "y", Just (FA.Literal p <| Types.Literal.Number "2") )
                ]
             , extends = Nothing
             }
                |> FA.Record p
                |> Test.okEqual
            )
        , codeTest "multiline compact"
            """
                    a = {
                      , x = 1
                      , y = 2
                      }
                    """
            firstEvaluation
            ({ attrs =
                [ ( "x", Just (FA.Literal p <| Types.Literal.Number "1") )
                , ( "y", Just (FA.Literal p <| Types.Literal.Number "2") )
                ]
             , extends = Nothing
             }
                |> FA.Record p
                |> Test.okEqual
            )
        , codeTest "annotation, inline"
            """
                    a =
                      is { x is Bool }
                      a
                    """
            firstAnnotation
            ({ extends = Nothing
             , attrs = [ ( "x", Just <| FA.TypeName p "Bool" ) ]
             }
                |> FA.TypeRecord p
                |> Test.okEqual
            )
        , codeTest "annotation, multiline"
            """
                    a =
                      is
                       {
                       , x is Bool
                       }
                      a
                    """
            firstAnnotation
            ({ extends = Nothing
             , attrs = [ ( "x", Just <| FA.TypeName p "Bool" ) ]
             }
                |> FA.TypeRecord p
                |> Test.okEqual
            )
        , codeTest "annotation, multiline compact"
            """
                    a =
                      is {
                       , x is Bool
                       }
                      a
                    """
            firstAnnotation
            ({ extends = Nothing
             , attrs = [ ( "x", Just <| FA.TypeName p "Bool" ) ]
             }
                |> FA.TypeRecord p
                |> Test.okEqual
            )
        , codeTest "[reg] simple assignment, inline"
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


ifs : Test
ifs =
    Test.Group "Ifs"
        [ codeTest "inline"
            "a = if a: b else c"
            firstEvaluation
            Test.isOk
        , codeTest "multiline, formatted"
            """
                    x =
                      if a:
                        b
                      else
                        c
                    """
            firstEvaluation
            Test.isOk
        , codeTest "multiline, compact"
            """
                    x =
                      if a: b
                      else c
                    """
            firstEvaluation
            Test.isOk
        ]


tries : Test
tries =
    Test.Group "Try"
        [ codeTest "inline"
            "x = try a as b: c else d"
            firstEvaluation
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
            firstEvaluation
            Test.isOk
        , codeTest "multiline, compact"
            """
            x =
              try a as
                b: c
                d: e
                else f
            """
            firstEvaluation
            Test.isOk
        ]


patterns : Test
patterns =
    Test.Group "Patterns"
        [ codeTest "list unpacking"
            "[a, b] = x"
            (firstDefinition >> Result.map .pattern)
            ([ FA.PatternAny p False "a", FA.PatternAny p False "b" ]
                |> FA.PatternList p
                |> Test.okEqual
            )
        , codeTest "list unpacking, inner block"
            """
x =
   [ a, b ] = c
                    """
            (firstDefinition >> Result.map .pattern)
            Test.isOk
        , codeTest "record unpacking"
            "{ a, b } = x"
            (firstDefinition >> Result.map .pattern)
            ({ extends = Nothing
             , attrs =
                [ ( "a", Nothing )
                , ( "b", Nothing )
                ]
             }
                |> FA.PatternRecord p
                |> Test.okEqual
            )
        , codeTest "record unpacking, inner block"
            """
x =
  { a, b } = c
                    """
            (firstDefinition >> Result.map .pattern)
            Test.isOk
        ]


binops : Test
binops =
    let
        sendBtoC b c =
            FA.Binop p
                Op.Pipe
                ( FA.Variable p { isBinop = False } "b"
                , [ ( Prelude.sendRight
                    , FA.Variable p { isBinop = False } "c"
                    )
                  ]
                )

        sendBtoCtoD b c d =
            FA.Binop p
                Op.Pipe
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
