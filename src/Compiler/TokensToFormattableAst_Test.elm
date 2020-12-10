module Compiler.TokensToFormattableAst_Test exposing (..)

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



-- TODO add tests for type annotations
-- TODO add helpers to declare tests input via text code


tests : List Test
tests =
    [ ----
      --- Binops
      --
      simpleTest
        { name =
            "Binops: left-association"
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
        { name =
            "Binops: precedence"
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

    ----
    --- Lambdas
    --
    , simpleTest
        { name =
            "Lambdas: inline nesting"
        , run =
            \_ ->
                [ Token.Fn
                , Token.Symbol "a"
                , Token.Defop
                , Token.Fn
                , Token.Symbol "b"
                , Token.Defop
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
                        ( FA.Evaluate <|
                            FA.Lambda
                                { start = 3
                                , parameters = ( FA.PatternAny "b", [] )
                                , body = ( FA.Evaluate <| FA.NumberLiteral { start = 6, end = 7, number = "3" }, [] )
                                }
                        , []
                        )
                    }
        }
    , simpleTest
        { name =
            "Lambdas: block nesting"
        , run =
            \_ ->
                [ Token.Fn
                , Token.Symbol "a"
                , Token.Defop
                , Token.BlockStart
                , Token.Fn
                , Token.Symbol "b"
                , Token.Defop
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
                        ( FA.Evaluate <|
                            FA.Lambda
                                { start = 4
                                , parameters = ( FA.PatternAny "b", [] )
                                , body = ( FA.Evaluate <| FA.NumberLiteral { start = 8, end = 9, number = "3" }, [] )
                                }
                        , []
                        )
                    }
                )
        }
    , simpleTest
        { name =
            "Lambdas: sibling nesting"
        , run =
            \_ ->
                [ Token.Fn
                , Token.Symbol "a"
                , Token.Defop
                , Token.NewSiblingLine
                , Token.Fn
                , Token.Symbol "b"
                , Token.Defop
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
                        ( FA.Evaluate <|
                            FA.Lambda
                                { start = 4
                                , parameters = ( FA.PatternAny "b", [] )
                                , body =
                                    ( FA.Evaluate <|
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
