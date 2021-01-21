module Compiler.TypeInference_Test exposing (..)

import Compiler.TestHelpers
import Compiler.TypeInference as TI
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


constant n =
    CA.TypeConstant { path = n, args = [] }


function from to =
    CA.TypeFunction { from = from, fromIsMutable = Nothing, to = to }


infer : String -> String -> TI.Res TI.EnvEntry
infer name code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Result.andThen (TI.inspectModule preamble)
        |> Result.andThen (Dict.get name >> Result.fromMaybe "Dict fail")



-- preamble : TI.Env


preamble =
    let
        em x =
            { type_ = x
            , forall = Set.empty
            , mutable = Just False
            }
    in
    Dict.fromList
        [ ( "add", em <| function (constant "Number") (function (constant "Number") (constant "Number")) )
        , ( "+", em <| function (constant "Number") (function (constant "Number") (constant "Number")) )
        , ( "not", em <| function (constant "Bool") (constant "Bool") )
        , ( "True", em <| constant "Bool" )
        , ( "False", em <| constant "Bool" )
        , ( "reset", em <| CA.TypeFunction { from = constant "Number", fromIsMutable = Just True, to = constant "None" } )
        , ( "+="
          , em <|
                CA.TypeFunction
                    { from = CA.TypeConstant { path = "Number", args = [] }
                    , fromIsMutable = Just False
                    , to =
                        CA.TypeFunction
                            { from = CA.TypeConstant { path = "Number", args = [] }
                            , fromIsMutable = Just True
                            , to = constant "None"
                            }
                    }
          )
        ]


tests : Test
tests =
    Test.Group "TypeInference"
        [ functions
        , statements
        , variableTypes
        , findAllNestedSiblingReferences
        , mutability
        , higherOrderTypes
        , records
        ]



----
--- Functions
--


functions : Test
functions =
    Test.Group "functions"
        [ simpleTest
            { name = "Known function with correct params"
            , run = \_ -> infer "a" "a = add 3 1"
            , expected = Ok { type_ = constant "Number", forall = Set.empty, mutable = Just False }
            }
        , simpleTest
            { name = "Known function with wrong params"
            , run = \_ -> infer "a" "a = add False"
            , expected = Err """cannot unify Bool and Number"""
            }
        , simpleTest
            { name = "Function inference 1"
            , run = \_ -> infer "a" "a x = add x 1"
            , expected =
                Ok
                    { type_ = function (constant "Number") (constant "Number")
                    , forall = Set.empty
                    , mutable = Just False
                    }
            }
        , simpleTest
            { name = "Function inference 2: same as 1, but with swapped args"
            , run = \_ -> infer "a" "a x = add 1 x"
            , expected =
                Ok
                    { type_ = function (constant "Number") (constant "Number")
                    , forall = Set.empty
                    , mutable = Just False
                    }
            }
        , simpleTest
            { name = "Function args can't shadow other names"
            , run = \_ -> infer "a" "a = fn a = 1"
            , expected = Err "function parameter `a` shadows env variable"
            }
        , simpleTest
            { name = "[reg] fn has type None"
            , run = \_ -> infer "a" "a = fn x = 1"
            , expected =
                Ok
                    { forall = Set.fromList [ "t2" ]
                    , mutable = Just False
                    , type_ =
                        CA.TypeFunction
                            { from = CA.TypeVariable { name = "t2" }
                            , fromIsMutable = Nothing
                            , to = CA.TypeConstant { path = "Number", args = [] }
                            }
                    }
            }
        ]



----
--- Statements
--


statements : Test
statements =
    Test.Group "statements"
        [ simpleTest
            { name = "Statement blocks should return the last statement's type"
            , run =
                \_ ->
                    infer "a"
                        """
                a =
                  3
                  False
                """
            , expected = Ok { type_ = constant "Bool", forall = Set.empty, mutable = Just False }
            }
        , simpleTest
            { name = "Definition statement return type None"
            , run =
                \_ ->
                    infer "a"
                        """
                a =
                  f x = 3
                """
            , expected = Ok { type_ = constant "None", forall = Set.empty, mutable = Just False }
            }
        ]



----
--- Variable types
--


variableTypes : Test
variableTypes =
    Test.Group "variable types"
        [ simpleTest
            { name = "Identity"
            , run =
                \_ ->
                    infer "id"
                        """
                        id : a -> a
                        id a = a
                        """
            , expected =
                Ok
                    { type_ =
                        CA.TypeFunction
                            { from = CA.TypeVariable { name = "a" }
                            , fromIsMutable = Just False
                            , to = CA.TypeVariable { name = "a" }
                            }
                    , forall = Set.singleton "a"
                    , mutable = Just False
                    }
            }
        , simpleTest
            { name = "Identity, no annotation"
            , run =
                \_ ->
                    infer "id"
                        """
                        id a = a
                        """
            , expected =
                Ok
                    { type_ =
                        CA.TypeFunction
                            { from = CA.TypeVariable { name = "t2" }
                            , fromIsMutable = Nothing
                            , to = CA.TypeVariable { name = "t2" }
                            }
                    , forall = Set.singleton "t2"
                    , mutable = Just False
                    }
            }
        , hasError
            { name = "Reject disconnected forall var types?"
            , run =
                \_ ->
                    infer "id"
                        """
                        id : a -> b
                        id l = l
                        """
            , test = Test.errorShouldContain "too general"
            }
        , isOk
            { name = "TyVar definitions: lambda scope"
            , run =
                \_ ->
                    infer "a"
                        """
                a b =
                  f x = x
                  f 3
                  f False
                """
            }
        , isOk
            { name = "TyVar definitions: non-lambda scope"
            , run =
                \_ ->
                    infer "a"
                        """
                a =
                  f x = x
                  f 3
                  f False
                """
            }
        , isOk
            { name = "TyVar definitions: root scope"
            , run =
                \_ ->
                    infer "a"
                        """
                        a x = x
                        g =
                          a 3
                          a False
                        """
            }
        , simpleTest
            {-
               https://stackoverflow.com/questions/900585/why-are-functions-in-ocaml-f-not-recursive-by-default/904715#904715

               This error happens only when the identity function (`b`) follows alphabetically
               the definition that references it.
               Just to be sure, I've added another test below that is identical to this one
               with the only difference that `a` is renamed to `c`, and it passes.

               Ok, per test #3, the problem is in the statements order.
            -}
            { name = "[reg] `a` was variable type instead than number"
            , run =
                \_ ->
                    infer "a"
                        """
                        b x = x
                        a = b 1
                        """
            , expected = Ok { type_ = constant "Number", forall = Set.empty, mutable = Just False }
            }
        , simpleTest
            -- See note for the test above!
            { name = "[reg] make sure that `c` works"
            , run =
                \_ ->
                    infer "c"
                        """
                        b x = x
                        c = b 1
                        """
            , expected = Ok { type_ = constant "Number", forall = Set.empty, mutable = Just False }
            }
        , simpleTest
            -- See note for the test above!
            { name = "[reg] it's in the declaration order!"
            , run =
                \_ ->
                    infer "q"
                        """
                        q =
                          a = b 1
                          b x = x
                          a
                        """
            , expected = Ok { type_ = constant "Number", forall = Set.empty, mutable = Just False }
            }

        -- TODO Test self recursion and mutual recursion
        ]



----
--- Definitions reordering
--


referencedSiblingDefs : Dict Name (Set Name)
referencedSiblingDefs =
    [ ( "a", [ "b", "blah" ] )
    , ( "b", [ "a", "meh" ] )

    --
    , ( "c", [ "d" ] )
    , ( "d", [ "e" ] )
    , ( "e", [ "c" ] )

    --
    , ( "f", [ "f" ] )
    , ( "g", [ "h" ] )

    --
    , ( "cc", [ "dd" ] )
    , ( "dd", [ "ee" ] )
    , ( "ee", [ "ee" ] )
    ]
        |> List.map (Tuple.mapSecond Set.fromList)
        |> Dict.fromList


findAllNestedSiblingReferences : Test
findAllNestedSiblingReferences =
    Test.Group "findAllNestedSiblingReferences"
        [ simpleTest
            { name = "Two-way recursion"
            , run = \_ -> TI.findAllNestedSiblingReferences referencedSiblingDefs "a" Set.empty
            , expected = Set.fromList [ "a", "b", "blah", "meh" ]
            }
        , simpleTest
            { name = "Three way recursion"
            , run = \_ -> TI.findAllNestedSiblingReferences referencedSiblingDefs "c" Set.empty
            , expected = Set.fromList [ "c", "d", "e" ]
            }
        , simpleTest
            { name = "Self recursion"
            , run = \_ -> TI.findAllNestedSiblingReferences referencedSiblingDefs "f" Set.empty
            , expected = Set.fromList [ "f" ]
            }
        , simpleTest
            { name = "No recursion"
            , run = \_ -> TI.findAllNestedSiblingReferences referencedSiblingDefs "g" Set.empty
            , expected = Set.fromList [ "g", "h" ]
            }
        , simpleTest
            { name = "Single self recursion"
            , run = \_ -> TI.findAllNestedSiblingReferences referencedSiblingDefs "cc" Set.empty
            , expected = Set.fromList [ "cc", "dd", "ee" ]
            }
        ]



{-
   findMutualRecursions : List Test
   findMutualRecursions =
       let
       in
       [ simpleTest
           { name =
               "Two-way mutual recursion A"
           , run =
               \_ ->
                   TI.findMutualRecursions referencedSiblingDefs "a" Set.empty
           , expected =
               Set.fromList [ "a", "b" ]
           }
       , simpleTest
           { name =
               "Two-way mutual recursion B"
           , run =
               \_ ->
                   TI.findMutualRecursions referencedSiblingDefs "b" Set.empty
           , expected =
               Set.fromList [ "a", "b" ]
           }
       , simpleTest
           { name =
               "Three-way mutual recursion D"
           , run =
               \_ ->
                   TI.findMutualRecursions referencedSiblingDefs "d" Set.empty
           , expected =
               Set.fromList [ "c", "d", "e" ]
           }
       , simpleTest
           { name =
               "Self recursion"
           , run =
               \_ ->
                   TI.findMutualRecursions referencedSiblingDefs "f" Set.empty
           , expected =
               Set.fromList [ "f" ]
           }
       , simpleTest
           { name =
               "No recursion 1"
           , run =
               \_ ->
                   TI.findMutualRecursions referencedSiblingDefs "g" Set.empty
           , expected =
               Set.fromList [ "g" ]
           }
       , simpleTest
           { name =
               "No recursion 2"
           , run =
               \_ ->
                   TI.findMutualRecursions referencedSiblingDefs "cc" Set.empty
           , expected =
               Set.fromList [ "cc" ]
           }
       ]
-}
----
--- Mutability
--


mutability : Test
mutability =
    Test.Group "mutability"
        [ simpleTest
            { name = "Statement blocks that define mutables can't return functions"
            , run =
                \_ ->
                    infer "a"
                        """
                        a =
                          x @= 1
                          fn y = y
                        """
            , expected = Err "statement blocks that define mutables can't return functions"
            }
        , simpleTest
            { name = "Infer lambda arg mutability"
            , run =
                \_ ->
                    infer "a"
                        """
                        a =
                          q x =
                            reset @x
                          q
                        """
            , expected =
                Ok
                    { type_ = CA.TypeFunction { from = constant "Number", fromIsMutable = Just True, to = constant "None" }
                    , forall = Set.empty
                    , mutable = Just False
                    }
            }
        , hasError
            { name = "Detect mismatching annotations"
            , run =
                \_ ->
                    infer "a"
                        """
                        a : Number -> None
                        a =
                          reset
                        """
            , test = Test.errorShouldContain "mutability clash"
            }
        , simpleTest
            { name = "Correctly unify annotation's mutability"
            , run =
                \_ ->
                    infer "a"
                        """
                        a : Number @> None
                        a =
                          reset
                        """
            , expected =
                Ok
                    { type_ = CA.TypeFunction { from = constant "Number", fromIsMutable = Just True, to = constant "None" }
                    , forall = Set.empty
                    , mutable = Just False
                    }
            }
        , simpleTest
            { name = "Functions can't be mutable 1"
            , run = \_ -> infer "a" "a @= fn x = x"
            , expected = Err "these mutable values contain functions: a"
            }

        {- TODO, requires type vars
           , simpleTest
               { name = "Functions can't be mutable 2"
               , run = \_ -> infer "a" "a f = set f (fn x = x)"
               , expected = Err "these mutable values contain functions: f"
               }
        -}
        , hasError
            { name = "Lambda argument mutability is correctly inferred"
            , run = \_ -> infer "a" "a = fn x = reset x"
            , test = Test.errorShouldContain "mutability clash"
            }
        , hasError
            { name = "*Nested* lambda argument mutability is correctly inferred"
            , run = \_ -> infer "a" "a = fn x = (fn y = reset y) x"
            , test = Test.errorShouldContain "mutability clash"
            }
        , hasError
            { name = "Functions can't be mutable (annotation)"
            , run =
                \_ ->
                    infer "a"
                        """
                        f @: Int -> Int
                        f @= add 1
                        """
            , test = Test.errorShouldContain "mutable"
            }
        , hasError
            { name = "args that are functions can't be mutable (annotation)"
            , run =
                \_ ->
                    infer "a"
                        """
                        a : (Int -> Int) @> Int
                        a = a
                        """
            , test = Test.errorShouldContain "mutable"
            }
        ]



----
--- Higher order types
--


higherOrderTypes : Test
higherOrderTypes =
    Test.Group "higher order types"
        [ simpleTest
            { name = "Parse precedence"
            , run =
                \_ ->
                    infer "a"
                        """
                        a : List a -> List a
                        a l = l
                        """
            , expected =
                Ok
                    { type_ =
                        CA.TypeFunction
                            { from = CA.TypeConstant { args = [ CA.TypeVariable { name = "a" } ], path = "List" }
                            , fromIsMutable = Just False
                            , to = CA.TypeConstant { args = [ CA.TypeVariable { name = "a" } ], path = "List" }
                            }
                    , mutable = Just False
                    , forall = Set.singleton "a"
                    }
            }
        ]



----
--- Records
--


records : Test
records =
    Test.Group "Records"
        [ simpleTest
            { name = "Attribute access"
            , run =
                \_ ->
                    infer "a"
                        """
                        a b = b.meh.blah
                        """
            , expected =
                Ok
                    { forall = Set.fromList [ "t5", "t7", "t8" ]
                    , mutable = Just False
                    , type_ =
                        CA.TypeFunction
                            { from =
                                CA.TypeRecord
                                    { attrs =
                                        Dict.singleton "meh"
                                            (CA.TypeRecord
                                                { attrs = Dict.singleton "blah" (CA.TypeVariable { name = "t8" })
                                                , extensible = Just "t7"
                                                }
                                            )
                                    , extensible = Just "t5"
                                    }
                            , fromIsMutable = Nothing
                            , to = CA.TypeVariable { name = "t8" }
                            }
                    }
            }
        , simpleTest
            { name = "Attribute mutation"
            , run =
                \_ ->
                    infer "a"
                        """
                        a b = @b.meh.blah += 1
                        """
            , expected =
                Ok
                    { forall = Set.fromList [ "t6", "t8" ]
                    , mutable = Just False
                    , type_ =
                        CA.TypeFunction
                            { from =
                                CA.TypeRecord
                                    { attrs =
                                        Dict.singleton "meh"
                                            (CA.TypeRecord
                                                { attrs = Dict.singleton "blah" (CA.TypeConstant { path = "Number", args = [] })
                                                , extensible = Just "t8"
                                                }
                                            )
                                    , extensible = Just "t6"
                                    }
                            , fromIsMutable = Just True
                            , to = CA.TypeConstant { path = "None", args = [] }
                            }
                    }
            }
        , isOk
            { name = "Tuple3 direct item mutability"
            , run =
                \_ ->
                    infer "x"
                        """
                        x =
                          a @= 3 & False & 2

                          @a.third += 1
                        """
            }
        , isOk
            { name = "Tuple2 direct item mutability, annotated"
            , run =
                \_ ->
                    infer "x"
                        """
                        x =
                           a @: Number & Number
                           a @= 1 & 2

                           @a.first += 1
                        """
            }
        , simpleTest
            { name = "functional update"
            , run =
                \_ ->
                    infer "a" "a b = { b with x = 1 }"
            , expected =
                let
                    re =
                        CA.TypeRecord
                            { attrs = Dict.singleton "x" (CA.TypeConstant { args = [], path = "Number" })
                            , extensible = Just "t6"
                            }
                in
                Ok
                    { forall = Set.fromList [ "t6" ]
                    , mutable = Just False
                    , type_ =
                        CA.TypeFunction
                            { from = re
                            , fromIsMutable = Nothing
                            , to = re
                            }
                    }
            }
        , simpleTest
            { name = "instantiate and refine inferred records"
            , run =
                \_ ->
                    infer "c"
                        """
                        a t = { t with x = 1 }
                        c = a
                        """
            , expected =
                let
                    re =
                        CA.TypeRecord
                            { attrs = Dict.singleton "x" (CA.TypeConstant { args = [], path = "Number" })
                            , extensible = Just "t9"
                            }
                in
                Ok
                    { forall = Set.fromList [ "t9" ]
                    , mutable = Just False
                    , type_ =
                        CA.TypeFunction
                            { from = re
                            , fromIsMutable = Nothing
                            , to = re
                            }
                    }
            }
        ]
