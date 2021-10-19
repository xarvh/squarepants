module SPTests.Lexer_Test exposing (..)

import Compiler.Lexer
import Compiler.TestHelpers
import Prelude
import Test exposing (Test)
import Types.Error exposing (Error)
import Types.Token as Token exposing (Token)


codeTest =
    Test.codeTest Debug.toString


lexTokens : String -> Result String (List Token)
lexTokens s =
    Compiler.Lexer.lexer "Test" s
        |> Compiler.TestHelpers.resErrorToString s


lexTokensAndDrop : Int -> String -> Result String (List Token)
lexTokensAndDrop n s =
    s |> lexTokens |> Result.map (List.drop n)


non_mut_name =
    Token.Name { mutable = False }
