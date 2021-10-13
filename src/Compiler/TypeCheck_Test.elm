module Compiler.TypeCheck_Test exposing (..)

import Compiler.CoreModule
import Compiler.TestHelpers as TH exposing (p)
import Compiler.TypeCheck as TI
import Dict exposing (Dict)
import Human.CanonicalAst as HCA
import Lib
import Prelude
import Set exposing (Set)
import Test exposing (Test)
import Types.CanonicalAst as CA exposing (Type)
import Types.Error exposing (Res)


tests : Test
tests =
    Test.Group "TypeCheck"
        [ functions
        , statements
        , variableTypes
        , mutability
        , higherOrderTypes
        , records
        , patterns
        , try_as
        , if_then
        , nonFunction
        ]



----
---
--


codeTest =
    Test.codeTest Debug.toString


simpleTest { name, code, run, expected } =
    Test.codeTest Debug.toString name code run (Test.okEqual expected)


hasError { name, code, run, test } =
    Test.codeTest Debug.toString name code run test


constant n =
    CA.TypeConstant p n []


function from to =
    CA.TypeFunction p from False to


tyNumber =
    constant "SPCore.Number"


tyNone =
    constant "SPCore.None"


ftv : String -> Dict String { nonFn : Bool }
ftv n =
    Dict.singleton n { nonFn = False }


forallTuple : List ( String, List CA.RejectFunction ) -> Dict String { nonFn : Bool }
forallTuple =
    List.foldl (\( n, rf ) -> Dict.insert n { nonFn = rf /= [] }) Dict.empty



----
--- These should be removed once we get rid of the old record declarations
--


typeFunction { from, fromIsMutable, to } =
    CA.TypeFunction p from fromIsMutable to


typeVariable { name } =
    CA.TypeVariable p name


typeConstant { ref, args } =
    CA.TypeConstant p ref args


typeRecord { extensible, attrs } =
    CA.TypeRecord p extensible attrs



----
---
--


testDefs : Dict String Type
testDefs =
    Dict.empty
        |> Dict.insert "Test.add" (function tyNumber (function tyNumber tyNumber))
        |> Dict.insert "Test.reset" (typeFunction { from = tyNumber, fromIsMutable = True, to = tyNone })


type alias Out =
    { freeTypeVariables : Dict String { nonFn : Bool }
    , ty : Type
    , isMutable : Bool
    }


cleanUpVar : TI.InstanceVariable -> Out
cleanUpVar var =
    let
        ( ty, tyvars ) =
            --(TH.removePos CA.posMap_type var.ty, var.freeTypeVariables)
            HCA.normalizeTypeAndTyvars (TH.removePos CA.posMap_type var.ty) var.freeTypeVariables
    in
    { ty = ty
    , freeTypeVariables = tyvars
    , isMutable = var.isMutable
    }


infer : String -> String -> Result String Out
infer name code =
    code
        |> TH.stringToCanonicalModuleWithPos
        |> Result.map (Dict.union Prelude.prelude >> TI.allDefsToEnvAndValues)
        |> Result.andThen (\( env, values ) -> TI.fromAllValueDefs (addTestDefs env) values)
        |> TH.resErrorToString code
        |> Result.andThen
            (\env ->
                env.instanceVariables
                    |> Dict.get ("Test." ++ name)
                    |> Result.fromMaybe "Dict fail"
                    |> Result.map cleanUpVar
            )


addTestDefs : TI.Env -> TI.Env
addTestDefs env =
    let
        add : String -> Type -> Dict String TI.InstanceVariable -> Dict String TI.InstanceVariable
        add name ty =
            Dict.insert name
                { definedAt = CA.T
                , ty = ty

                -- TODO populate nonFn
                , freeTypeVariables = Dict.map (\k p -> { nonFn = False }) (TI.typeTyvars ty)
                , isMutable = False
                }
    in
    { env | instanceVariables = Dict.foldl add env.instanceVariables testDefs }



----
--- Functions
--


functions : Test
functions =
    Test.Group "functions"
        [ codeTest "Known function with correct params"
            "a = add 3 1"
            (infer "a")
            (Test.okEqual
                { ty = tyNumber
                , freeTypeVariables = Dict.empty
                , isMutable = False
                }
            )
        , codeTest "Known function with wrong params"
            "a = add False"
            (infer "a")
            (Test.errContain "SPCore.Bool")
        , simpleTest
            { name = "Function inference 1"
            , code = "a x = add x 1"
            , run = infer "a"
            , expected =
                { ty = function tyNumber tyNumber
                , freeTypeVariables = Dict.empty
                , isMutable = False
                }
            }
        , simpleTest
            { name = "Function inference 2: same as 1, but with swapped args"
            , code = "a x = add 1 x"
            , run = infer "a"
            , expected =
                { ty = function tyNumber tyNumber
                , freeTypeVariables = Dict.empty
                , isMutable = False
                }
            }
        , simpleTest
            { name = "[reg] fn has type None"
            , code = "a = fn x: 1"
            , run = infer "a"
            , expected =
                { freeTypeVariables = ftv "a"
                , isMutable = False
                , ty =
                    typeFunction
                        { from = typeVariable { name = "a" }
                        , fromIsMutable = False
                        , to = typeConstant { ref = "SPCore.Number", args = [] }
                        }
                }
            }

        --
        , codeTest "[reg] Multiple arguments are correctly inferred"
            """
            a x y z = x + y + z
            """
            (infer "a")
            Test.isOk

        --
        , codeTest "Annotation should be consistent with mutability"
            """
            f a =
              is Number @> Number
              a
            """
            (infer "f")
            (Test.errContain "IncompatibleMutability")
        ]



----
--- Statements
--


statements : Test
statements =
    Test.Group "statements"
        [ codeTest
            """
            Statement blocks should return the last statement's type
            """
            """
            a =
              3
              False
            """
            (infer "a")
            (Test.okEqual { ty = constant "SPCore.Bool", freeTypeVariables = Dict.empty, isMutable = False })
        , codeTest
            """
            Definition statement return type None
            """
            """
                a =
                  f x = 3
                """
            (infer "a")
            (Test.okEqual { ty = tyNone, freeTypeVariables = Dict.empty, isMutable = False })
        , codeTest
            """
            Local values can't shadow root values
            """
            """
            a = 1
            b =
              a = 1
              a
            """
            (infer "b")
            (Test.errContain "already")
        , codeTest
            """
            Prevent local redeclarations
            """
            """
            b =
              a = 1
              a = 1
            """
            (infer "b")
            (Test.errContain "declar")
        , codeTest
            """
            Prevent root redeclarations
            """
            """
            a = 1
            a = 1
            """
            (infer "b")
            (Test.errContain "declar")
        ]



----
--- Variable types
--


variableTypes : Test
variableTypes =
    Test.Group "variable types"
        [ simpleTest
            { name = "Identity"
            , code =
                """
                id a =
                  is a -> a
                  a
                """
            , run = infer "id"
            , expected =
                { ty =
                    typeFunction
                        { from = typeVariable { name = "a" }
                        , fromIsMutable = False
                        , to = typeVariable { name = "a" }
                        }
                , freeTypeVariables = ftv "a"
                , isMutable = False
                }
            }

        {-
           , simpleTest
               { name = "Identity, no annotation"
               , code =
                   """
                           id a = a
                           """
               , run = infer "id"
               , expected =
                   { ty =
                       typeFunction
                           { from = typeVariable { name = "0" }
                           , fromIsMutable = False
                           , to = typeVariable { name = "0" }
                           }
                   , freeTypeVariables = freeTypeVariables "0" []
                   , isMutable = False
                   }
               }
           , codeTest "Reject disconnected freeTypeVariables var types?"
               """
               id l =
                 is a -> b
                 l
               """
               (infer "id")
               (Test.errContain "too general")
           , codeTest "TyVar definitions: lambda scope"
               """
                   a b =
                     f x = x
                     f 3
                     f False
                   """
               (infer "a")
               Test.isOk
           , codeTest "TyVar definitions: non-lambda scope"
               """
                   a =
                     f x = x
                     f 3
                     f False
                   """
               (infer "a")
               Test.isOk
           , codeTest "TyVar definitions: root scope"
               """
                           a x = x
                           g =
                             a 3
                             a False
                           """
               (infer "a")
               Test.isOk

           -- TODO Implement self recursion and mutual recursion
           , codeTest "[reg] statements, assignments, free vars"
               """
               id a = a

               x q =
                     s = id q
                     s
               """
               (infer "x")
               (Test.okEqual
                   { freeTypeVariables = freeTypeVariables "2" []
                   , isMutable = False
                   , ty =
                       typeFunction
                           { from = typeVariable { name = "2" }
                           , fromIsMutable = False
                           , to = typeVariable { name = "2" }
                           }
                   }
               )
           , codeTest "[reg] ??? TI failure"
               """
               rec acc =
                   if True then
                       acc
                   else
                       rec [] "" [] acc
               """
               (infer "rec")
               (Test.errContain "annotation")
        -}
        {- OBSOLETE

           Since now we allow mutual recursion only across root lambda definitions, these tests are obsolete.

           If at a later time we decide that non-root functions can be mutually recursive we'll possibly restore these.

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
               , expected = Ok { ty = tyNumber, freeTypeVariables = Dict.empty, isMutable = False }
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
               , expected = Ok { ty = tyNumber, freeTypeVariables = Dict.empty, isMutable = False }
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
               , expected = Ok { ty = tyNumber, freeTypeVariables = Dict.empty, isMutable = False }
               }
        -}
        ]



----
--- Definitions reordering
--


referencedSiblingDefs : Dict String (Set String)
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



----
--- Mutability
--


mutability : Test
mutability =
    Test.Group "mutability"
        [ hasError
            { name = "Statement blocks that define mutables can't return functions"
            , code =
                """
                a =
                  x @= 1
                  fn y: y
                """
            , run = infer "a"
            , test = Test.errContain "can't return functions"
            }
        , codeTest "Immutable variables can't be used as mutable"
            """
            a x =
              @x := 1
            """
            (infer "a")
            (Test.errContain "mutable")
        , hasError
            { name = "Detect mismatching annotations"
            , code =
                """
                a =
                  is Number -> None
                  reset
                """
            , run = infer "a"
            , test = Test.errContain "IncompatibleMutability"
            }
        , simpleTest
            { name = "Correctly unify annotation's mutability"
            , code =
                """
                        a =
                          is Number @> None
                          reset
                        """
            , run = infer "a"
            , expected =
                { ty = typeFunction { from = tyNumber, fromIsMutable = True, to = tyNone }
                , freeTypeVariables = Dict.empty
                , isMutable = False
                }
            }
        , hasError
            { name = "Functions can't be mutable 1"
            , code = "a @= fn x: x"
            , run = infer "a"
            , test = Test.errContain "mutable"
            }
        , codeTest
            "Functions can't be mutable 2"
            """
            a @f =
              @f := (fn x: x)
            """
            (infer "a")
            (Test.errContain "mutable args cannot be functions")
        , codeTest
            "Functions can't be mutable 3"
            """
            a @f =
              f 1
            """
            (infer "a")
            (Test.errContain "mutable args cannot be functions")
        , hasError
            { name = "Lambda argument mutability is correctly inferred"
            , code = "a = fn x: reset x"
            , run = infer "a"
            , test = Test.errContain "mutability clash"
            }
        , hasError
            { name = "*Nested* lambda argument mutability is correctly inferred"
            , code = "a = fn x: (fn y: reset y) x"
            , run = infer "a"
            , test = Test.errContain "mutability clash"
            }
        , hasError
            { name = "Functions can't be mutable (annotation)"
            , code =
                """
                a @=
                  is Number -> Number
                  add 1
                """
            , run = infer "a"
            , test = Test.errContain "mutable"
            }
        , codeTest
            "Mutables can contain functions via free tyvars"
            """
            a x =
              s @= x
              s

            z =
              a (fn x: x)
            """
            (infer "a")
            Test.isOk
        ]



----
--- Higher order types
--


higherOrderTypes : Test
higherOrderTypes =
    Test.Group "higher order types"
        [ simpleTest
            { name = "Parse precedence"
            , code =
                """
                        a l =
                          is List a -> List a
                          l
                        """
            , run = infer "a"
            , expected =
                { ty =
                    typeFunction
                        { from = typeConstant { args = [ typeVariable { name = "a" } ], ref = "SPCore.List" }
                        , fromIsMutable = False
                        , to = typeConstant { args = [ typeVariable { name = "a" } ], ref = "SPCore.List" }
                        }
                , isMutable = False
                , freeTypeVariables = ftv "a"
                }
            }
        , simpleTest
            { name = "Union type constructors"
            , code = "union X a = L"
            , run = infer "L"
            , expected =
                { ty = typeConstant { args = [ typeVariable { name = "a" } ], ref = "Test.X" }
                , isMutable = False
                , freeTypeVariables = ftv "a"
                }
            }
        ]



----
--- Records
--


records : Test
records =
    Test.Group "Records"
        [ codeTest "Attribute access"
            """
            a b = b.meh.blah
            """
            (infer "a")
            (Test.okEqual
                { freeTypeVariables =
                    forallTuple
                        [ ( "a", [] )
                        , ( "b", [] )
                        , ( "c", [] )
                        ]
                , isMutable = False
                , ty =
                    typeFunction
                        { from =
                            typeRecord
                                { attrs =
                                    Dict.singleton "meh"
                                        (typeRecord
                                            { attrs = Dict.singleton "blah" (typeVariable { name = "c" })
                                            , extensible = Just "b"
                                            }
                                        )
                                , extensible = Just "a"
                                }
                        , fromIsMutable = False
                        , to = typeVariable { name = "c" }
                        }
                }
            )
        , codeTest
            """
            Attribute mutation
            """
            """
            a @b = @b.meh.blah += 1
            """
            (infer "a")
            (Test.okEqual
                { freeTypeVariables =
                    forallTuple
                        [ ( "a", [] )
                        , ( "b", [] )
                        ]
                , isMutable = False
                , ty =
                    typeFunction
                        { from =
                            typeRecord
                                { attrs =
                                    Dict.singleton "meh"
                                        (typeRecord
                                            { attrs = Dict.singleton "blah" (typeConstant { ref = "SPCore.Number", args = [] })
                                            , extensible = Just "b"
                                            }
                                        )
                                , extensible = Just "a"
                                }
                        , fromIsMutable = True
                        , to = typeConstant { ref = "SPCore.None", args = [] }
                        }
                }
            )
        , codeTest "Tuple3 direct item mutability"
            """
            x =
              a @= 3 & False & 2

              @a.third += 1
            """
            (infer "x")
            Test.isOk
        , codeTest "Tuple2 direct item mutability, annotated"
            """
            x =
               a @=
                 is Number & Number
                 1 & 2

               @a.first += 1
            """
            (infer "x")
            Test.isOk
        , simpleTest
            { name = "functional update"
            , code = "a b = { b with x = 1 }"
            , run = infer "a"
            , expected =
                let
                    re =
                        typeRecord
                            { attrs = Dict.singleton "x" (typeConstant { args = [], ref = "SPCore.Number" })
                            , extensible = Just "a"
                            }
                in
                { freeTypeVariables =
                    forallTuple
                        [ ( "a", [] )
                        ]
                , isMutable = False
                , ty =
                    typeFunction
                        { from = re
                        , fromIsMutable = False
                        , to = re
                        }
                }
            }
        , codeTest "instantiate and refine inferred records"
            """
            a t = { t with x = 1 }
            c = a
            """
            (infer "c")
            (let
                re =
                    typeRecord
                        { attrs = Dict.singleton "x" (typeConstant { args = [], ref = "SPCore.Number" })
                        , extensible = Just "a"
                        }
             in
             Test.okEqual
                { freeTypeVariables =
                    forallTuple
                        [ ( "a", [] )
                        ]
                , isMutable = False
                , ty =
                    typeFunction
                        { from = re
                        , fromIsMutable = False
                        , to = re
                        }
                }
            )
        , codeTest "[reg] excessive forallness in records"
            """
            x q =
             a = q.first
             a
            """
            (infer "x")
            (Test.okEqual
                { freeTypeVariables =
                    forallTuple
                        [ ( "a", [] )
                        , ( "b", [] )
                        ]
                , isMutable = False
                , ty =
                    typeFunction
                        { from =
                            typeRecord
                                { attrs = Dict.fromList [ ( "first", typeVariable { name = "b" } ) ]
                                , extensible = Just "a"
                                }
                        , fromIsMutable = False
                        , to = typeVariable { name = "b" }
                        }
                }
            )
        , codeTest "[reg] refineType when the record has a non-extensible alias"
            """
            alias A = { c is Number, d is Number }

            upd a =
              is A -> A
              { a with c = .c + 1 }
            """
            (infer "upd")
            Test.isOk
        ]



----
--- Patterns
--


patterns : Test
patterns =
    Test.Group "Patterns"
        [ codeTest "List unpacking"
            """
            x q =
                   [ first, second ] = q
                   first
            """
            (infer "x")
            --
            (Test.okEqual
                { freeTypeVariables = ftv "a"
                , isMutable = False
                , ty =
                    typeFunction
                        { from =
                            typeConstant
                                { args = [ typeVariable { name = "a" } ]
                                , ref = "SPCore.List"
                                }
                        , fromIsMutable = False
                        , to = typeVariable { name = "a" }
                        }
                }
            )
        , codeTest "Records are correctly unpacked"
            """
            x q =
                { first } = q
                first
            """
            (infer "x")
            --
            (Test.okEqual
                { freeTypeVariables = forallTuple [ ( "a", [] ), ( "b", [] ) ]
                , isMutable = False
                , ty =
                    typeFunction
                        { from =
                            typeRecord
                                { attrs = Dict.fromList [ ( "first", typeVariable { name = "b" } ) ]
                                , extensible = Just "a"
                                }
                        , fromIsMutable = False
                        , to = typeVariable { name = "b" }
                        }
                }
            )
        , {- TODO
             I can't reproduce this error in the unit tests.
             Even if I copy all code verbatim here, the error does not appear.

             I can only reproduce it on the dev environment and not reliably.
             I don't fully understand what causes it.

             Still, the problem is caused at least in part by the fact that I'm not instantiating the type for type constructors when inferring patterns
             (In TypeCheck.fromPattern#CA.PatternConstructor) which is definitely something worth fixing.

             But still, I don't understand the problem enough to reproduce it reliably.
          -}
          codeTest "[rec] Constructors should instantiate their variable types"
            """
            each ls f =
                is List a -> (a -> b) -> None
                try ls as
                    SPCore.Nil:
                        None

            result =
                  1 :: SPCore.Nil = SPCore.Nil
            """
            (infer "result")
            --
            Test.isOk
        ]



----
--- Try..As
--


try_as : Test
try_as =
    Test.Group "try..as"
        [ codeTest "basic functionality"
            """
            x q =
             try q as
               True: 2
               else 3
            """
            (infer "x")
            (Test.okEqual
                { freeTypeVariables = Dict.empty
                , isMutable = False
                , ty =
                    typeFunction
                        { from = typeConstant { ref = "SPCore.Bool", args = [] }
                        , fromIsMutable = False
                        , to = typeConstant { ref = "SPCore.Number", args = [] }
                        }
                }
            )

        --
        , codeTest "rejects non-matching patterns"
            """
            x q =
             try q as
               True: 2
               []: 3
            """
            (infer "x")
            (Test.errContain "SPCore.List")

        --
        , codeTest "rejects non-matching blocks"
            """
            x q =
             try q as
               True: 2
               False: False
            """
            (infer "x")
            (Test.errContain "SPCore.Number")
        ]



----
--- if..then
--


if_then : Test
if_then =
    Test.Group "if..then"
        [ codeTest "basic functionality"
            """
            x q =
              if q then 1
              else 2
            """
            (infer "x")
            (Test.okEqual
                { freeTypeVariables = Dict.empty
                , isMutable = False
                , ty =
                    typeFunction
                        { from = typeConstant { ref = "SPCore.Bool", args = [] }
                        , fromIsMutable = False
                        , to = typeConstant { ref = "SPCore.Number", args = [] }
                        }
                }
            )

        --
        , codeTest "rejects non-bool conditions"
            """
            x q =
              if 1 then 1
              else 2
            """
            (infer "x")
            (Test.errContain "SPCore.Bool")

        --
        , codeTest "rejects non-matching blocks"
            """
            x q =
              if q then 2
              else False
            """
            (infer "x")
            (Test.errContain "SPCore.Number")
        ]



----
--- NonFunction
--


nonFunction : Test
nonFunction =
    Test.Group "NonFunction"
        [ codeTest
            """
            Basic functionality
            """
            """
            blah a =
              is List a -> List a
              with a NonFunction
              a

            meh =
              blah [fn x: x]
            """
            (infer "meh")
            (Test.errContain "should not contain functions")
        ]
