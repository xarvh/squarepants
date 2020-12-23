module Compiler.TokensToFormattableAst_Test exposing (..)

import Compiler.TestHelpers
import Compiler.TokensToFormattableAst as Syntax
import Test exposing (Test)
import Types.FormattableAst as FA
import Types.Token as Token exposing (Token)


simpleTest =
    Test.simple Debug.toString


kindToToken : Int -> Token.Kind -> Token
kindToToken index kind =
    { start = index
    , end = index + 1
    , kind = kind
    }


asDefinition : FA.Statement -> Maybe FA.ValueDefinition
asDefinition s =
    case s of
        FA.Definition a ->
            Just a

        _ ->
            Nothing


tests : Test
tests =
    Test.Group "TokensToFormattableAst"
        [ binops
        , lambdas
        , annotations
        , typeDefinitions
        ]


binops : Test
binops =
    Test.Group "Binops"
        [ simpleTest
            { name = "left-association"
            , run =
                \_ ->
                    [ Token.NumberLiteral "1"
                    , Token.Binop Token.Addittive "+"
                    , Token.NumberLiteral "2"
                    , Token.Binop Token.Addittive "+"
                    , Token.NumberLiteral "3"
                    ]
                        |> List.indexedMap kindToToken
                        |> Syntax.runParser (Syntax.end Syntax.expr)
            , expected =
                Ok
                    (FA.Binop
                        { left =
                            FA.Binop
                                { left = FA.NumberLiteral { start = 0, end = 1, number = "1" }
                                , op = "+"
                                , right = FA.NumberLiteral { start = 2, end = 3, number = "2" }
                                }
                        , op = "+"
                        , right = FA.NumberLiteral { start = 4, end = 5, number = "3" }
                        }
                    )
            }
        , simpleTest
            { name = "precedence"
            , run =
                \_ ->
                    [ Token.NumberLiteral "1"
                    , Token.Binop Token.Addittive "+"
                    , Token.NumberLiteral "2"
                    , Token.Binop Token.Multiplicative "*"
                    , Token.NumberLiteral "3"
                    ]
                        |> List.indexedMap kindToToken
                        |> Syntax.runParser (Syntax.end Syntax.expr)
            , expected =
                Ok
                    (FA.Binop
                        { left = FA.NumberLiteral { start = 0, end = 1, number = "1" }
                        , op = "+"
                        , right =
                            FA.Binop
                                { left = FA.NumberLiteral { start = 2, end = 3, number = "2" }
                                , op = "*"
                                , right = FA.NumberLiteral { start = 4, end = 5, number = "3" }
                                }
                        }
                    )
            }
        ]


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
    let
        firstAnnotation code =
            code
                |> Compiler.TestHelpers.stringToFormattableModule
                |> Result.toMaybe
                |> Maybe.andThen List.head
                |> Maybe.andThen asDefinition
                |> Maybe.andThen .maybeAnnotation
    in
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
                Just
                    { mutable = False
                    , name = "a"
                    , type_ =
                        FA.TypeFunction
                            { from = FA.TypeConstantOrVariable { name = "Number" }
                            , fromIsMutable = True
                            , to =
                                FA.TypeFunction
                                    { from = FA.TypeConstantOrVariable { name = "Int" }
                                    , fromIsMutable = False
                                    , to = FA.TypeConstantOrVariable { name = "None" }
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
                Just
                    { mutable = False
                    , name = "a"
                    , type_ =
                        FA.TypeFunction
                            { from = FA.TypeConstantOrVariable { name = "Number" }
                            , fromIsMutable = False
                            , to =
                                FA.TypeFunction
                                    { from = FA.TypeConstantOrVariable { name = "Int" }
                                    , fromIsMutable = True
                                    , to = FA.TypeConstantOrVariable { name = "None" }
                                    }
                            }
                    }
            }
        ]


typeDefinitions : Test
typeDefinitions =
    let
        firstTypeDef code =
            code
                |> Compiler.TestHelpers.stringToFormattableModule
                |> Result.toMaybe
                |> Maybe.andThen List.head
                |> Maybe.andThen asTypeDef

        asTypeDef s =
            case s of
                FA.TypeDefinition a ->
                    Just a

                _ ->
                    Nothing
    in
    Test.Group "Type Definitions"
        [ simpleTest
            { name = "Parse inline def"
            , run = \_ -> firstTypeDef "type A b c = V1 b | V2 c | V3 | V4 b c"
            , expected =
                Just
                    { args = [ "b", "c" ]
                    , constructors =
                        [ { args = [ FA.TypeConstantOrVariable { name = "b" } ], name = "V1" }
                        , { args = [ FA.TypeConstantOrVariable { name = "c" } ], name = "V2" }
                        , { args = [], name = "V3" }
                        , { args = [ FA.TypeConstantOrVariable { name = "b" }, FA.TypeConstantOrVariable { name = "c" } ], name = "V4" }
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
                           | V1
                           | V2
                        """
            , expected =
                Just
                    { args = []
                    , constructors =
                        [ { args = [], name = "V1" }
                        , { args = [], name = "V2" }
                        ]
                    , name = "A"
                    }
            }
        ]
