module Compiler.ApplyAliases_Test exposing (..)

import Compiler.ApplyAliases
import Compiler.TestHelpers
import Dict exposing (Dict)
import Set exposing (Set)
import Test exposing (Test)
import Types.CanonicalAst as CA


isOk =
    Test.isOk Debug.toString


simpleTest =
    Test.simple Debug.toString


hasError =
    Test.hasError Debug.toString


applyAndGet : (CA.RootDef () -> Maybe a) -> String -> String -> Result String a
applyAndGet getAs name code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Compiler.TestHelpers.resultErrorToString code
        |> Result.andThen (Dict.get ("Test." ++ name) >> Result.fromMaybe "dict error!")
        |> Result.andThen (getAs >> Result.fromMaybe "wrong variant")


applyAndGetValue : String -> String -> Result String (CA.ValueDef ())
applyAndGetValue name code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Compiler.TestHelpers.resultErrorToString code
        |> Result.andThen (CA.findValue ("Test." ++ name) >> Result.fromMaybe "findValue error!")



----
---
--


tests : Test
tests =
    Test.Group "ApplyAliases"
        [ Test.Group "annotations"
            [ simpleTest
                { name = "simple"
                , run =
                    \_ ->
                        """
                        alias A b c = List b
                        a : A Number Bool
                        a = a
                        """
                            |> applyAndGetValue "a"
                            |> Result.map .maybeAnnotation
                , expected =
                    { args =
                        [ CA.TypeConstant
                            { args = []
                            , ref = "SPCore.Number"
                            }
                        ]
                    , ref = "SPCore.List"
                    }
                        |> CA.TypeConstant
                        |> CA.TypeAlias "Test.A"
                        |> Just
                        |> Ok
                }
            , hasError
                { name = "Reject wrong number of args"
                , run =
                    \_ ->
                        """
                        alias A b c = List b
                        a : A Bool
                        a = a
                        """
                            |> applyAndGetValue "a"
                , test = Test.errorShouldContain "alias Test.A needs 2 args, but was used with 1"
                }
            , simpleTest
                { name = "record"
                , run =
                    \_ ->
                        """
                        alias A b = { x : b, y : b }
                        a : A Bool
                        a = a
                        """
                            |> applyAndGetValue "a"
                            |> Result.map .maybeAnnotation
                , expected =
                    { attrs =
                        Dict.fromList
                            [ ( "x", CA.TypeConstant { args = [], ref = "SPCore.Bool" } )
                            , ( "y", CA.TypeConstant { args = [], ref = "SPCore.Bool" } )
                            ]
                    , extensible = Nothing
                    }
                        |> CA.TypeRecord
                        |> CA.TypeAlias "Test.A"
                        |> Just
                        |> Ok
                }
            ]
        , Test.Group "unions"
            [ simpleTest
                { name = "simple"
                , run =
                    \_ ->
                        """
                        alias A b c = List b
                        union B x = B1 (A Bool x)
                        """
                            |> applyAndGet CA.asUnion "B"
                            |> Result.map .constructors
                , expected =
                    Ok <|
                        Dict.singleton "Test.B1"
                            [ CA.TypeAlias "Test.A"
                                (CA.TypeConstant
                                    { ref = "SPCore.List"
                                    , args =
                                        [ CA.TypeConstant
                                            { args = []
                                            , ref = "SPCore.Bool"
                                            }
                                        ]
                                    }
                                )
                            ]
                }
            ]

        {-
           , Test.Group "aliases"
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
