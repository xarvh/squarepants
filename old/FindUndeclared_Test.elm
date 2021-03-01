module Compiler.FindUndeclared_Test exposing (..)

import Compiler.FindUndeclared as F
import Compiler.TestHelpers
import Dict exposing (Dict)
import Test exposing (Test)
import Types.CanonicalAst as CA


codeTest =
    Test.codeTest Debug.toString


undeclared : String -> Result String (Result (List F.Error) F.EnvUn)
undeclared code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Result.map F.moduleUndeclared
        |> Compiler.TestHelpers.resultErrorToString code



----
---
--


tests : Test
tests =
    Test.Group "FindUndeclared"
        [ codeTest "unordered definitions in the same scope"
            """
            a =
               b = c + 1
               c = 1
            """
            undeclared
            (Test.okEqual <|
                Err <|
                    [ F.ErrorValueUsedBeforeDeclaration "c" [ 111 ] ]
            )
        , codeTest "[reg] unordered definitions in root scope"
            """
            a =
              b = c

            c =
              1
            """
            undeclared
            (Test.okEqual <|
                Err <|
                    [ F.ErrorValueUsedBeforeDeclaration "c" [ 111 ] ]
            )
        , codeTest "reject aliases with undeclared var types"
            """
            alias K a = List b
            """
            undeclared
            (Test.okEqual <|
                Err <|
                    [ F.ErrorUndeclaredTypeVariable "b" [ 111 ] ]
            )
        , codeTest "reject union constructors with undeclared var types"
            """
            type Q b c = Q d
            """
            undeclared
            (Test.okEqual <|
                Err <|
                    [ F.ErrorUndeclaredTypeVariable "d" [ 111 ] ]
            )
        , codeTest "[reg] lambda params should count as declared!"
            """
            x q = q
            """
            undeclared
            (Test.okEqual <|
                Ok
                    { types = Dict.empty
                    , values = Dict.empty
                    , valuesUsedBeforeDeclaration = Dict.empty
                    }
            )
        ]
