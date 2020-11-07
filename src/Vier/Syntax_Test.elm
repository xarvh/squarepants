module Vier.Syntax_Test exposing (..)

import Test exposing (Test)
import Vier.Syntax as Syntax exposing (Expression(..), runParser)
import Vier.Token as Token exposing (Token)


simpleTest =
    Test.simple Debug.toString


tests : List Test
tests =
    [ ----
      --- Expr
      --
      simpleTest
        { name =
            "expr: left-associates binops"
        , run =
            \_ ->
                runParser (Syntax.end Syntax.expr)
                    [ Token.NumberLiteral "1"
                    , Token.Binop Token.AddittiveSpaced "+"
                    , Token.NumberLiteral "2"
                    , Token.Binop Token.AddittiveSpaced "+"
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
            "expr: binops precedence"
        , run =
            \_ ->
                runParser (Syntax.end Syntax.expr)
                    [ Token.NumberLiteral "1"
                    , Token.Binop Token.AddittiveSpaced "+"
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
    ]
