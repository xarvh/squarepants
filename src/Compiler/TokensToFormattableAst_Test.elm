module Compiler.TokensToFormattableAst_Test exposing (..)

import Compiler.TokensToFormattableAst as Syntax
import Test exposing (Test)
import Types.FormattableAst as FA
import Types.Token as Token exposing (Token)


simpleTest =
    Test.simple Debug.toString


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
                Syntax.runParser (Syntax.end Syntax.expr)
                    [ Token.NumberLiteral "1"
                    , Token.Binop Token.Addittive "+"
                    , Token.NumberLiteral "2"
                    , Token.Binop Token.Addittive "+"
                    , Token.NumberLiteral "3"
                    ]
        , expected =
            Ok
                (FA.Binop
                    (FA.Binop
                        (FA.NumberLiteral "1")
                        "+"
                        (FA.NumberLiteral "2")
                    )
                    "+"
                    (FA.NumberLiteral "3")
                )
        }
    , simpleTest
        { name =
            "Binops: precedence"
        , run =
            \_ ->
                Syntax.runParser (Syntax.end Syntax.expr)
                    [ Token.NumberLiteral "1"
                    , Token.Binop Token.Addittive "+"
                    , Token.NumberLiteral "2"
                    , Token.Binop Token.Multiplicative "*"
                    , Token.NumberLiteral "3"
                    ]
        , expected =
            Ok
                (FA.Binop
                    (FA.NumberLiteral "1")
                    "+"
                    (FA.Binop
                        (FA.NumberLiteral "2")
                        "*"
                        (FA.NumberLiteral "3")
                    )
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
                Syntax.runParser (Syntax.end Syntax.expr)
                    [ Token.Fn
                    , Token.Symbol "a"
                    , Token.Defop
                    , Token.Fn
                    , Token.Symbol "b"
                    , Token.Defop
                    , Token.NumberLiteral "3"
                    ]
        , expected =
            Ok <|
                FA.Lambda
                    { parameters = ( "a", [] )
                    , body =
                        ( FA.Evaluate <|
                            FA.Lambda
                                { parameters = ( "b", [] )
                                , body = ( FA.Evaluate <| FA.NumberLiteral "3", [] )
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
                Syntax.runParser (Syntax.end Syntax.expr)
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
        , expected =
            Ok
                (FA.Lambda
                    { parameters = ( "a", [] )
                    , body =
                        ( FA.Evaluate <|
                            FA.Lambda
                                { parameters = ( "b", [] )
                                , body = ( FA.Evaluate <| FA.NumberLiteral "3", [] )
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
                Syntax.runParser (Syntax.end Syntax.expr)
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
        , expected =
            Ok
                (FA.Lambda
                    { parameters = ( "a", [] )
                    , body =
                        ( FA.Evaluate <|
                            FA.Lambda
                                { parameters = ( "b", [] )
                                , body = ( FA.Evaluate <| FA.NumberLiteral "3", [] )
                                }
                        , []
                        )
                    }
                )
        }
    ]
