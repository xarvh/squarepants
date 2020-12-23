module Compiler.FormattableToCanonicalAst_Test exposing (..)

import Compiler.FormattableToCanonicalAst
import Compiler.TestHelpers exposing (stringToCanonicalModule)
import Dict exposing (Dict)
import Set exposing (Set)
import Test exposing (Test)
import Types.CanonicalAst as CA exposing (Name)


simpleTest =
    Test.simple Debug.toString


isOk =
    Test.isOk Debug.toString


hasError =
    Test.hasError Debug.toString


tests : Test
tests =
    Test.Group "FormattableToCanonicalAst"
        [ unionTypes
        ]



----
--- Union Types
--


unionTypes : Test
unionTypes =
    Test.Group "unionTypes"
        [ hasError
            { name = "name starts with uppercase"
            , run = \_ -> stringToCanonicalModule "type a = A"
            , test = Test.errorShouldContain "uppercase"
            }
        , hasError
            { name = "constructor names start with uppercase"
            , run = \_ -> stringToCanonicalModule "type A = a"
            , test = Test.errorShouldContain "constructor"
            }
        ]
