module Compiler.TokensToFormattableAst_Test exposing (..)

import Compiler.TestHelpers
import Compiler.TokensToFormattableAst as Syntax
import Test exposing (Test)
import Types.FormattableAst as FA
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


faBinop : Token.PrecedenceGroup -> { left : FA.Expression, op : String, right : FA.Expression } -> FA.Expression
faBinop group { left, op, right } =
    FA.Binop
        { group = group
        , sepList = ( left, [ ( op, right ) ] )
        }


firstStatement : String -> Result String FA.Statement
firstStatement code =
    code
        |> Compiler.TestHelpers.stringToFormattableModule
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
                        |> Syntax.runParser (Syntax.end Syntax.expr)
            , expected =
                Ok <|
                    FA.Lambda
                        { start = 0
                        , parameters = ( FA.PatternAny "a", [] )
                        , body =
                            ( FA.Evaluation <|
                                FA.Lambda
                                    { start = 3
                                    , parameters = ( FA.PatternAny "b", [] )
                                    , body = ( FA.Evaluation <| FA.NumberLiteral { start = 6, end = 7, number = "3" }, [] )
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
                        |> Syntax.runParser (Syntax.end Syntax.expr)
            , expected =
                Ok
                    (FA.Lambda
                        { start = 0
                        , parameters = ( FA.PatternAny "a", [] )
                        , body =
                            ( FA.Evaluation <|
                                FA.Lambda
                                    { start = 4
                                    , parameters = ( FA.PatternAny "b", [] )
                                    , body = ( FA.Evaluation <| FA.NumberLiteral { start = 8, end = 9, number = "3" }, [] )
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
                        |> Syntax.runParser (Syntax.end Syntax.expr)
            , expected =
                Ok
                    (FA.Lambda
                        { start = 0
                        , parameters = ( FA.PatternAny "a", [] )
                        , body =
                            ( FA.Evaluation <|
                                FA.Lambda
                                    { start = 4
                                    , parameters = ( FA.PatternAny "b", [] )
                                    , body =
                                        ( FA.Evaluation <|
                                            FA.NumberLiteral
                                                { start = 8
                                                , end = 9
                                                , number = "3"
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
            , run = \_ -> firstTypeDef "type A b c = V1 b, V2 c, V3, V4 b c"
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
                        type A =
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
            , run = \_ -> firstTypeDef "type A = A [Int]"
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
                [ FA.NumberLiteral { end = 6, number = "1", start = 5 }
                , FA.NumberLiteral { end = 9, number = "2", start = 8 }
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
                [ FA.NumberLiteral { end = 12, number = "1", start = 11 }
                , FA.NumberLiteral { end = 18, number = "2", start = 17 }
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
            , expected = Ok (FA.Record { attrs = [ ( "x", Just (FA.NumberLiteral { end = 11, number = "1", start = 10 }) ) ], maybeUpdateTarget = Nothing })
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
            , expected = Ok (FA.Record { attrs = [ ( "x", Just (FA.NumberLiteral { end = 16, number = "1", start = 15 }) ), ( "y", Just (FA.NumberLiteral { end = 26, number = "2", start = 25 }) ) ], maybeUpdateTarget = Nothing })
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
            , expected = Ok (FA.TypeRecord [ ( "x", FA.TypeName { name = "Bool" } ) ])
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
            , expected = Ok (FA.TypeRecord [ ( "x", FA.TypeName { name = "Bool" } ) ])
            }
        , codeTest "annotation, extensible"
            """
            a : { b with x : Bool }
            a = a
            """
            (firstAnnotation >> Result.map .type_)
            (Test.errContain "not supported")
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
            , expected = Ok <| FA.PatternList [ FA.PatternAny "a", FA.PatternAny "b" ]
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
            , expected = Ok <| FA.PatternRecord [ ( "a", Nothing ), ( "b", Nothing ) ]
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
