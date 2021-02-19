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


applyAndGet : (CA.Module () -> Dict String a) -> String -> String -> Result String a
applyAndGet dict name code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Compiler.TestHelpers.resultErrorToString code
        |> Result.map dict
        |> Result.andThen (Dict.get name >> Result.fromMaybe "dict error!")


applyAndGetValue : String -> String -> Result String (CA.ValueDef ())
applyAndGetValue name code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Compiler.TestHelpers.resultErrorToString code
        |> Result.andThen (CA.findValue name >> Result.fromMaybe "findValue error!")



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
                            , path = "Number"
                            }
                        ]
                    , path = "List"
                    }
                        |> CA.TypeConstant
                        |> CA.TypeAlias "A"
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
                , test = Test.errorShouldContain "alias A needs 2 args, but was used with 1"
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
                            [ ( "x", CA.TypeConstant { args = [], path = "Bool" } )
                            , ( "y", CA.TypeConstant { args = [], path = "Bool" } )
                            ]
                    , extensible = Nothing
                    }
                        |> CA.TypeRecord
                        |> CA.TypeAlias "A"
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
                        type B x = B1 (A Bool x)
                        """
                            |> applyAndGet .unions "B"
                            |> Result.map .constructors
                , expected =
                    Ok
                        [ { name = "B1"
                          , args =
                                [ CA.TypeAlias "A"
                                    (CA.TypeConstant
                                        { path = "List"
                                        , args =
                                            [ CA.TypeConstant
                                                { args = []
                                                , path = "Bool"
                                                }
                                            ]
                                        }
                                    )
                                ]
                          }
                        ]
                }
            ]
        , Test.Group "aliases"
            [ simpleTest
                { name = "simple"
                , run =
                    \_ ->
                        """
                        alias A b c = List b
                        alias B x = A Bool x
                        """
                            |> applyAndGet .aliases "B"
                            |> Result.map .ty
                , expected =
                    Ok <| CA.TypeAlias "A" (CA.TypeConstant { path = "List", args = [ CA.TypeConstant { path = "Bool", args = [] } ] })
                }
            , hasError
                { name = "reject circular aliases"
                , run =
                    \_ ->
                        """
                        alias A = B -> B
                        alias B = [ A ]
                        """
                            |> applyAndGet .aliases "B"
                , test =
                    Test.errorShouldContain "circular"
                }
            ]
        ]
