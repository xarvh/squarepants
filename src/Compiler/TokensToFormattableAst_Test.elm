module Compiler.TokensToFormattableAst_Test exposing (..)

import Compiler.TestHelpers
import Compiler.TokensToFormattableAst as Syntax
import Parser
import Test exposing (Test)
import Types.FormattableAst as FA
import Types.Literal
import Types.Token as Token exposing (Token)


tests : Test
tests =
    Test.Group "TokensToFormattableAst"
        [ lambdas
        , annotations
        , unionDefs
        , lists
        , records
        , ifs
        , tries
        , patterns
        , binops
        ]



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
    ts
        |> Parser.parse (Syntax.discardSecond parser Parser.end) Syntax.unconsIgnoreComments
        |> Syntax.outcomeToResult "Test" "" ts
        |> Compiler.TestHelpers.resErrorToString


firstStatement : String -> Result String FA.Statement
firstStatement code =
    code
        |> Compiler.TestHelpers.stringToFormattableModule
        |> Compiler.TestHelpers.resErrorToString
        |> Result.andThen (List.head >> Result.fromMaybe "no head")


firstDefinition =
    firstStatement >> Result.andThen asDefinition


firstEvaluation : String -> Result String FA.Expression
firstEvaluation code =
    code
        |> firstDefinition
        |> Result.map (.body >> Tuple.first)
        |> Result.andThen asEvaluation


firstAnnotation : String -> Result String FA.Annotation
firstAnnotation code =
    code
        |> firstStatement
        |> Result.andThen asDefinition
        |> Result.andThen (.maybeAnnotation >> Result.fromMaybe "no annotation")



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
                    , Token.Defop { mutable = False }
                    , Token.Fn
                    , Token.Name { mutable = False } "b"
                    , Token.Defop { mutable = False }
                    , Token.NumberLiteral "3"
                    ]
                        |> List.indexedMap kindToToken
                        |> runParser Syntax.expr
            , expected =
                Ok <|
                    FA.Lambda
                        { start = 0
                        , parameters = ( FA.PatternAny ( 1, 2 ) "a", [] )
                        , body =
                            ( FA.Evaluation <|
                                FA.Lambda
                                    { start = 3
                                    , parameters = ( FA.PatternAny ( 4, 5 ) "b", [] )
                                    , body = ( FA.Evaluation <| FA.Literal { start = 6, end = 7, value = Types.Literal.Number "3" }, [] )
                                    }
                            , []
                            )
                        }
            }
        , simpleTest
            { name = "block nesting"
            , run =
                \_ ->
                    [ Token.Fn
                    , Token.Name { mutable = False } "a"
                    , Token.Defop { mutable = False }
                    , Token.BlockStart
                    , Token.Fn
                    , Token.Name { mutable = False } "b"
                    , Token.Defop { mutable = False }
                    , Token.BlockStart
                    , Token.NumberLiteral "3"
                    , Token.BlockEnd
                    , Token.BlockEnd
                    ]
                        |> List.indexedMap kindToToken
                        |> runParser Syntax.expr
            , expected =
                Ok
                    (FA.Lambda
                        { start = 0
                        , parameters = ( FA.PatternAny ( 1, 2 ) "a", [] )
                        , body =
                            ( FA.Evaluation <|
                                FA.Lambda
                                    { start = 4
                                    , parameters = ( FA.PatternAny ( 5, 6 ) "b", [] )
                                    , body = ( FA.Evaluation <| FA.Literal { start = 8, end = 9, value = Types.Literal.Number "3" }, [] )
                                    }
                            , []
                            )
                        }
                    )
            }
        , simpleTest
            { name = "sibling nesting"
            , run =
                \_ ->
                    [ Token.Fn
                    , Token.Name { mutable = False } "a"
                    , Token.Defop { mutable = False }
                    , Token.NewSiblingLine
                    , Token.Fn
                    , Token.Name { mutable = False } "b"
                    , Token.Defop { mutable = False }
                    , Token.NewSiblingLine
                    , Token.NumberLiteral "3"
                    ]
                        |> List.indexedMap kindToToken
                        |> runParser Syntax.expr
            , expected =
                Ok
                    (FA.Lambda
                        { start = 0
                        , parameters = ( FA.PatternAny ( 1, 2 ) "a", [] )
                        , body =
                            ( FA.Evaluation <|
                                FA.Lambda
                                    { start = 4
                                    , parameters = ( FA.PatternAny ( 5, 6 ) "b", [] )
                                    , body =
                                        ( FA.Evaluation <|
                                            FA.Literal
                                                { start = 8
                                                , end = 9
                                                , value = Types.Literal.Number "3"
                                                }
                                        , []
                                        )
                                    }
                            , []
                            )
                        }
                    )
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
                        a : Number @> Int -> None
                        a = 1
                        """
            , expected =
                Ok
                    { mutable = False
                    , name = "a"
                    , type_ =
                        FA.TypeFunction
                            { from = FA.TypeName { name = "Number" }
                            , fromIsMutable = True
                            , to =
                                FA.TypeFunction
                                    { from = FA.TypeName { name = "Int" }
                                    , fromIsMutable = False
                                    , to = FA.TypeName { name = "None" }
                                    }
                            }
                    }
            }
        , simpleTest
            { name = "Mutability 2"
            , run =
                \_ ->
                    firstAnnotation
                        """
                        a : Number -> Int @> None
                        a = 1
                        """
            , expected =
                Ok
                    { mutable = False
                    , name = "a"
                    , type_ =
                        FA.TypeFunction
                            { from = FA.TypeName { name = "Number" }
                            , fromIsMutable = False
                            , to =
                                FA.TypeFunction
                                    { from = FA.TypeName { name = "Int" }
                                    , fromIsMutable = True
                                    , to = FA.TypeName { name = "None" }
                                    }
                            }
                    }
            }
        , simpleTest
            { name = "Tuple precedence"
            , run =
                \_ ->
                    firstAnnotation
                        """
                        a : Int & Int -> Bool
                        a = a
                        """
            , expected =
                Ok
                    { mutable = False
                    , name = "a"
                    , type_ =
                        FA.TypeFunction
                            { from =
                                FA.TypeTuple
                                    [ FA.TypeName
                                        { name = "Int"
                                        }
                                    , FA.TypeName { name = "Int" }
                                    ]
                            , fromIsMutable = False
                            , to = FA.TypeName { name = "Bool" }
                            }
                    }
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
                        [ FA.TypePolymorphic { args = [ FA.TypeName { name = "b" } ], name = "V1" }
                        , FA.TypePolymorphic { args = [ FA.TypeName { name = "c" } ], name = "V2" }
                        , FA.TypeName { name = "V3" }
                        , FA.TypePolymorphic { args = [ FA.TypeName { name = "b" }, FA.TypeName { name = "c" } ], name = "V4" }
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
                    [ FA.TypeName { name = "V1" }
                    , FA.TypeName { name = "V2" }
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
                        [ FA.TypePolymorphic
                            { name = "A"
                            , args =
                                [ FA.TypePolymorphic
                                    { name = "List"
                                    , args = [ FA.TypeName { name = "Int" } ]
                                    }
                                ]
                            }
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
                [ FA.Literal { end = 6, value = Types.Literal.Number "1", start = 5 }
                , FA.Literal { end = 9, value = Types.Literal.Number "2", start = 8 }
                ]
                    |> FA.List
                    |> Ok
            }
        , simpleTest
            { name = "multiline"
            , run = \_ -> firstEvaluation """
                 a = [
                   , 1
                   , 2
                   ]
                 """
            , expected =
                [ FA.Literal { end = 12, value = Types.Literal.Number "1", start = 11 }
                , FA.Literal { end = 18, value = Types.Literal.Number "2", start = 17 }
                ]
                    |> FA.List
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
                    (FA.Record ( 4, 12 )
                        { attrs = [ ( "x", Just (FA.Literal { end = 11, value = Types.Literal.Number "1", start = 10 }) ) ]
                        , extends = Nothing
                        }
                    )
            }
        , simpleTest
            { name = "multiline"
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
                    (FA.Record ( 5, 27 )
                        { attrs =
                            [ ( "x", Just (FA.Literal { end = 16, value = Types.Literal.Number "1", start = 15 }) )
                            , ( "y", Just (FA.Literal { end = 26, value = Types.Literal.Number "2", start = 25 }) )
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
                    a : { x : Bool }
                    a = a
                    """
                        |> firstAnnotation
                        |> Result.map .type_
            , expected =
                Ok
                    (FA.TypeRecord ( 5, 16 )
                        { extends = Nothing
                        , attrs = [ ( "x", Just <| FA.TypeName { name = "Bool" } ) ]
                        }
                    )
            }
        , simpleTest
            { name = "annotation, multiline"
            , run =
                \_ ->
                    """
                    a : {
                       , x : Bool
                       }
                    a = a
                    """
                        |> firstAnnotation
                        |> Result.map .type_
            , expected =
                Ok
                    (FA.TypeRecord ( 5, 21 )
                        { extends = Nothing
                        , attrs = [ ( "x", Just <| FA.TypeName { name = "Bool" } ) ]
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
            , run = \_ -> firstEvaluation "x = try a as b then c else d"
            }
        , isOk
            { name = "multiline, formatted"
            , run =
                \_ ->
                    """
                    x =
                      try a as
                        b then
                          c
                        d then
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
                        b then c
                        d then e
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
            , expected = Ok <| FA.PatternList ( 0, 7 ) [ FA.PatternAny ( 1, 2 ) "a", FA.PatternAny ( 4, 5 ) "b" ]
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
                    FA.PatternRecord ( 0, 7 )
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
            FA.Binop
                { group = Token.Pipe
                , sepList =
                    ( FA.Variable { start = b, end = b + 1, name = "b", binop = False }
                    , [ ( ">>"
                        , FA.Variable { start = c, end = c + 1, name = "c", binop = False }
                        )
                      ]
                    )
                }

        sendBtoCtoD b c d =
            FA.Binop
                { group = Token.Pipe
                , sepList =
                    ( FA.Variable { start = b, end = b + 1, name = "b", binop = False }
                    , [ ( ">>"
                        , FA.Variable { start = c, end = c + 1, name = "c", binop = False }
                        )
                      , ( ">>"
                        , FA.Variable { start = d, end = d + 1, name = "d", binop = False }
                        )
                      ]
                    )
                }
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
