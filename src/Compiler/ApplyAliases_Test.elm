module Compiler.ApplyAliases_Test exposing (..)

import Compiler.ApplyAliases
import Compiler.TestHelpers as TH exposing (p)
import Dict exposing (Dict)
import Set exposing (Set)
import Test exposing (Test)
import Types.CanonicalAst as CA


codeTest =
    Test.codeTest Debug.toString


applyAndGet : (CA.RootDef -> Maybe a) -> String -> String -> Result String a
applyAndGet getAs name code =
    code
        |> TH.stringToCanonicalModule
        |> TH.resErrorToString code
        |> Result.andThen (Dict.get ("Test." ++ name) >> Result.fromMaybe "dict error!")
        |> Result.andThen (getAs >> Result.fromMaybe "wrong variant")


applyAndGetValue : String -> String -> Result String CA.RootValueDef
applyAndGetValue name code =
    code
        |> TH.stringToCanonicalModule
        |> TH.resErrorToString code
        |> Result.andThen (CA.findValue ("Test." ++ name) >> Result.fromMaybe "findValue error!")



----
---
--


tests : Test
tests =
    Test.Group "ApplyAliases"
        [ Test.Group "annotations"
            [ codeTest "simple"
                """
                alias A b c = List b
                a =
                  as A Number Bool
                  a
                """
                (applyAndGetValue "a" >> Result.map .maybeAnnotation)
                ([ CA.TypeConstant p "SPCore.Number" []
                 ]
                    |> CA.TypeConstant p "SPCore.List"
                    |> CA.TypeAlias p "Test.A"
                    |> Just
                    |> Test.okEqual
                )
            , codeTest "Reject wrong number of args"
                """
                alias A b c = List b
                a =
                  as A Bool
                  a
                """
                (applyAndGetValue "a")
                (Test.errContain "alias Test.A needs 2 args, but was used with 1")
            , codeTest "record"
                """
                alias A b = { x as b, y as b }
                a =
                  as A Bool
                  a
                """
                (applyAndGetValue "a" >> Result.map .maybeAnnotation)
                (Dict.empty
                    |> Dict.insert "x" (CA.TypeConstant p "SPCore.Bool" [])
                    |> Dict.insert "y" (CA.TypeConstant p "SPCore.Bool" [])
                    |> CA.TypeRecord p Nothing
                    |> CA.TypeAlias p "Test.A"
                    |> Just
                    |> Test.okEqual
                )
            , codeTest "Reject mutable args that contain functions"
                """
                a =
                  as (Int -> Int) @> Int
                  a
                """
                (applyAndGetValue "a")
                (Test.errContain "utable")
            ]
        , Test.Group "unions"
            [ codeTest "simple"
                """
                alias A b c = List b
                union B x = B1 (A Bool x)
                """
                (applyAndGet CA.asUnion "B" >> Result.map .constructors)
                (Test.okEqual <|
                    Dict.singleton "Test.B1"
                        [ CA.TypeAlias p
                            "Test.A"
                            (CA.TypeConstant p
                                "SPCore.List"
                                [ CA.TypeConstant p "SPCore.Bool" []
                                ]
                            )
                        ]
                )
            ]
        , Test.Group "aliases"
            [ codeTest "Reject mutable args that contain functions"
                """
                alias X = { x as Text -> Text } @> Text
                """
                (applyAndGet CA.asAlias "X")
                (Test.errContain "contain function")
            ]

        {-
           [ simpleTest
               { name = "simple"
               , run =
                   \_ ->
                       """
                       alias A b c = List b
                       alias B x = A Bool x
                       """
                           |> applyAndGet CA.asAlias "B"
                           |> Result.map .ty
               , expected =
                   Ok <| CA.TypeAlias "Test.A" (CA.TypeConstant { ref = "SPCore.List", args = [ CA.TypeConstant { ref = "SPCore.Bool", args = [] } ] })
               }
           , hasError
               { name = "reject circular aliases"
               , run =
                   \_ ->
                       """
                       alias A = B -> B
                       alias B = [ A ]
                       """
                           |> applyAndGet CA.asAlias "B"
               , test =
                   Test.errorShouldContain "circular"
               }
           ]
        -}
        ]
