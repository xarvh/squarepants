module Vier.Lexer_Test exposing (..)

import Test exposing (Test)
import Vier.Lexer as Lexer
import Vier.Token as Token exposing (TokenKind)
import Vier.Error exposing (Error)


simpleTest =
    Test.simple Debug.toString


lexTokens : String -> () -> Result Error (List TokenKind)
lexTokens s _ =
  s
  |> Lexer.lexer
  |> Result.map (List.map .kind)




tests : List Test
tests =
    [ ----
      --- Unary additive ops
      --
      simpleTest
        { name = "Unary +/-: -a"
        , run = lexTokens " -a"
        , expected =
            Ok
                [ Token.BlockStart
                , Token.Unop "-"
                , Token.Symbol "a"
                , Token.BlockEnd
                ]
        }
      , simpleTest
        { name = "Unary +/-: a - -a"
        , run = lexTokens "a - -a"
        , expected =
            Ok
                [ Token.NewSiblingLine
                , Token.Symbol "a"
                , Token.Binop Token.Addittive "-"
                , Token.Unop "-"
                , Token.Symbol "a"
                ]
        }
    ]
