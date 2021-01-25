module Compiler.FindUndeclared_Test exposing (..)

import Compiler.FindUndeclared exposing (EnvUndeclared, Undeclared)
import Compiler.TestHelpers
import Dict exposing (Dict)
import Test exposing (Test)
import Types.CanonicalAst as CA


simpleTest =
    Test.simple Debug.toString


undeclared : String -> Result String (Result Undeclared EnvUndeclared)
undeclared code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Result.map Compiler.FindUndeclared.moduleUndeclared
        |> Compiler.TestHelpers.resultErrorToString code



----
---
--


tests : Test
tests =
    Test.Group "FindUndeclared"
        [ simpleTest
            { name = "unordered definitions"
            , run =
                \_ ->
                    undeclared
                        """
                        a =
                           b = c + 1
                           c = 1
                        """
            , expected =
                Ok <|
                    Ok
                        { types = Dict.empty
                        , values = Dict.fromList [ ( "+", [ 111 ] ) ]
                        }
            }
        , simpleTest
            { name = "reject aliases with undeclared var types"
            , run = \_ -> undeclared "alias K a = List b"
            , expected = Ok <| Err <| Dict.singleton "b" [ 111 ]
            }
        , simpleTest
            { name = "reject union constructors with undeclared var types"
            , run = \_ -> undeclared "type Q b c = Q d"
            , expected = Ok <| Err <| Dict.singleton "d" [ 111 ]
            }
        ]
