module Vier.Syntax_Test exposing (..)

import Test exposing (Test)
import Vier.Syntax as Syntax exposing (Expression(..), Statement(..), runParser)
import Vier.Token as Token exposing (Token)


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
                runParser (Syntax.end Syntax.expr)
                    [ Token.NumberLiteral "1"
                    , Token.Binop Token.Addittive "+"
                    , Token.NumberLiteral "2"
                    , Token.Binop Token.Addittive "+"
                    , Token.NumberLiteral "3"
                    ]
        , expected =
            Ok
                (Binop
                    (Binop
                        (Literal "1")
                        "+"
                        (Literal "2")
                    )
                    "+"
                    (Literal "3")
                )
        }
    , simpleTest
        { name =
            "Binops: precedence"
        , run =
            \_ ->
                runParser (Syntax.end Syntax.expr)
                    [ Token.NumberLiteral "1"
                    , Token.Binop Token.Addittive "+"
                    , Token.NumberLiteral "2"
                    , Token.Binop Token.Multiplicative "*"
                    , Token.NumberLiteral "3"
                    ]
        , expected =
            Ok
                (Binop
                    (Literal "1")
                    "+"
                    (Binop
                        (Literal "2")
                        "*"
                        (Literal "3")
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
                runParser (Syntax.end Syntax.expr)
                    [ Token.Fn
                    , Token.Symbol "a"
                    , Token.Binop Token.Assignment "="
                    , Token.Fn
                    , Token.Symbol "b"
                    , Token.Binop Token.Assignment "="
                    , Token.NumberLiteral "3"
                    ]
        , expected =
            Ok
                (Lambda [ "a" ]
                    ( Evaluate <|
                        Lambda [ "b" ]
                            ( Evaluate <| Literal "3"
                            , []
                            )
                    , []
                    )
                )
        }
    , simpleTest
        { name =
            "Lambdas: block nesting"
        , run =
            \_ ->
                runParser (Syntax.end Syntax.expr)
                    [ Token.Fn
                    , Token.Symbol "a"
                    , Token.Binop Token.Assignment "="
                    , Token.BlockStart
                    , Token.Fn
                    , Token.Symbol "b"
                    , Token.Binop Token.Assignment "="
                    , Token.BlockStart
                    , Token.NumberLiteral "3"
                    , Token.BlockEnd
                    , Token.BlockEnd
                    ]
        , expected =
            Ok
                (Lambda [ "a" ]
                    ( Evaluate <|
                        Lambda [ "b" ]
                            ( Evaluate <| Literal "3"
                            , []
                            )
                    , []
                    )
                )
        }
    , simpleTest
        { name =
            "Lambdas: sibling nesting"
        , run =
            \_ ->
                runParser (Syntax.end Syntax.expr)
                    [ Token.Fn
                    , Token.Symbol "a"
                    , Token.Binop Token.Assignment "="
                    , Token.NewSiblingLine
                    , Token.Fn
                    , Token.Symbol "b"
                    , Token.Binop Token.Assignment "="
                    , Token.NewSiblingLine
                    , Token.NumberLiteral "3"
                    ]
        , expected =
            Ok
                (Lambda [ "a" ]
                    ( Evaluate <|
                        Lambda [ "b" ]
                            ( Evaluate <| Literal "3"
                            , []
                            )
                    , []
                    )
                )
        }
    ]
