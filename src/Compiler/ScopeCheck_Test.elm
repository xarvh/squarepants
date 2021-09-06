module Compiler.ScopeCheck_Test exposing (..)

import Compiler.CoreModule
import Compiler.TestHelpers as TH exposing (p)
import Dict exposing (Dict)
import Test exposing (Test)
import Types.Error exposing (Res)


tests : Test
tests =
    Test.Group "ScopeCheck"
        [ codeTest "Function args can't shadow other names"
            "a = fn a: 1"
            check
            (Test.errContain "already used")
        ]


codeTest =
    Test.codeTest Debug.toString


check : String -> Result String ()
check code =
    code
        |> TH.stringToCanonicalModuleWithPos
        |> Result.map (always ())
        |> TH.resErrorToString code
