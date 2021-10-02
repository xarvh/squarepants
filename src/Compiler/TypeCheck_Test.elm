module Compiler.TypeCheck_Test exposing (..)

import Compiler.CoreModule
import Compiler.TestHelpers as TH exposing (p)
import Compiler.TypeCheck as TI
import Dict exposing (Dict)
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


forall : String -> List CA.RejectFunction -> Dict String TI.TypeVariable
forall n rf =
    Dict.singleton n { definedAt = CA.S, rf = rf }


forallMany : List String -> Dict String TI.TypeVariable
forallMany =
    List.map (\n -> ( n, { definedAt = CA.S, rf = [] } )) >> Dict.fromList


forallTuple : List ( String, List CA.RejectFunction ) -> Dict String TI.TypeVariable
forallTuple =
    List.foldl (\( n, rf ) -> Dict.insert n { definedAt = CA.S, rf = rf }) Dict.empty



----
--- These should be removed once we get rid of the old record declarations
--


typeFunction { from, fromIsMutable, to } =
    CA.TypeFunction p from fromIsMutable to


typeVariable { name } =
    CA.TypeVariable p [] name


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


infer : String -> String -> Result String { forall : Dict String TI.TypeVariable, type_ : Type, mutable : Bool }
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
                    --|> Maybe.map normalizeSchema
                    |> Result.fromMaybe "Dict fail"
                    |> Result.map
                        (\var ->
                            { type_ = TH.removePos CA.posMap_type var.ty
                            , forall = Dict.map stripPosFromTyvar var.freeTypeVariables
                            , mutable = var.isMutable
                            }
                        )
            )


stripPosFromTyvar : name -> TI.TypeVariable -> TI.TypeVariable
stripPosFromTyvar name tyvar =
    let
        stripRf rf =
            case rf of
                CA.Us _ ->
                    CA.Us CA.S

                CA.Pa _ ->
                    CA.Pa CA.S

                CA.Re _ ->
                    CA.Re CA.S
    in
    { definedAt = CA.S
    , rf = List.map stripRf tyvar.rf
    }


addTestDefs : TI.Env -> TI.Env
addTestDefs env =
    let
        add : String -> Type -> Dict String TI.InstanceVariable -> Dict String TI.InstanceVariable
        add name ty =
            Dict.insert name
                { definedAt = CA.T
                , ty = ty
                , freeTypeVariables = TI.typeTyvars ty
                , isMutable = False
                }
    in
    { env | instanceVariables = Dict.foldl add env.instanceVariables testDefs }



----
--- "t2" -> "a"
--
-- TODO move to Human/?
-- normalizeSchema : TI.EnvEntry -> TI.EnvEntry
-- normalizeSchema schema =
--     let
--         ( ty, dict ) =
--             normalizeType Dict.empty schema.type_
--
--         replaceName name =
--             Dict.get name dict |> Maybe.withDefault name
--     in
--     { schema
--         | type_ = ty
--         , forall = Set.map replaceName schema.forall
--     }
--
--
-- normalizeName : Dict String String -> String -> ( String, Dict String String )
-- normalizeName dict name =
--     case Dict.get name dict of
--         Just new ->
--             ( new, dict )
--
--         Nothing ->
--             if String.toInt name == Nothing then
--                 ( name, dict )
--
--             else
--                 let
--                     n =
--                         Dict.size dict + 1 |> String.fromInt
--                 in
--                 ( n, Dict.insert name n dict )
--
--
-- normalizeType : Dict String String -> Type -> ( Type, Dict String String )
-- normalizeType dict ty =
--     case ty of
--         CA.TypeConstant pos name args ->
--             let
--                 fold arg ( ars, d ) =
--                     normalizeType d arg
--                         |> Tuple.mapFirst (\na -> na :: ars)
--
--                 ( reversedArgs, dict1 ) =
--                     List.foldl fold ( [], dict ) args
--             in
--             ( CA.TypeConstant pos name (List.reverse reversedArgs)
--             , dict1
--             )
--
--         CA.TypeVariable pos name ->
--             normalizeName dict name
--                 |> Tuple.mapFirst (CA.TypeVariable pos)
--
--         CA.TypeFunction pos from0 fromIsMut to0 ->
--             let
--                 ( from, d1 ) =
--                     normalizeType dict from0
--
--                 ( to, d2 ) =
--                     normalizeType d1 to0
--             in
--             ( CA.TypeFunction pos from fromIsMut to
--             , d2
--             )
--
--         CA.TypeRecord pos ext0 attrs0 ->
--             let
--                 ( et, d1 ) =
--                     case ext0 of
--                         Nothing ->
--                             ( Nothing, dict )
--
--                         Just e ->
--                             normalizeName dict e |> Tuple.mapFirst Just
--
--                 fold name attr ( accum, d ) =
--                     normalizeType d attr
--                         |> Tuple.mapFirst (\na -> Dict.insert name na accum)
--
--                 ( attrs, d2 ) =
--                     Dict.foldl fold ( Dict.empty, d1 ) attrs0
--             in
--             ( CA.TypeRecord pos et attrs
--             , d2
--             )
--
--         CA.TypeAlias pos path t ->
--             normalizeType dict t
--                 |> Tuple.mapFirst (CA.TypeAlias pos path)
--
--
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
                { type_ = tyNumber
                , forall = Dict.empty
                , mutable = False
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
                { type_ = function tyNumber tyNumber
                , forall = Dict.empty
                , mutable = False
                }
            }
        , simpleTest
            { name = "Function inference 2: same as 1, but with swapped args"
            , code = "a x = add 1 x"
            , run = infer "a"
            , expected =
                { type_ = function tyNumber tyNumber
                , forall = Dict.empty
                , mutable = False
                }
            }
        , simpleTest
            { name = "[reg] fn has type None"
            , code = "a = fn x: 1"
            , run = infer "a"
            , expected =
                { forall = forall "0" []
                , mutable = False
                , type_ =
                    typeFunction
                        { from = typeVariable { name = "0" }
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
              as Number @> Number
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
        [ simpleTest
            { name = "Statement blocks should return the last statement's type"
            , code =
                """
                a =
                  3
                  False
                """
            , run = infer "a"
            , expected = { type_ = constant "SPCore.Bool", forall = Dict.empty, mutable = False }
            }
        , simpleTest
            { name = "Definition statement return type None"
            , code =
                """
                a =
                  f x = 3
                """
            , run = infer "a"
            , expected = { type_ = tyNone, forall = Dict.empty, mutable = False }
            }
        , codeTest "Local values can't shadow root values"
            """
            a = 1
            b =
              a = 1
              a
            """
            (infer "b")
            (Test.errContain "already")
        , codeTest "Prevent local redeclarations"
            """
            b =
              a = 1
              a = 1
            """
            (infer "b")
            (Test.errContain "declar")
        , codeTest "Prevent root redeclarations"
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
                  as a -> a
                  a
                """
            , run = infer "id"
            , expected =
                { type_ =
                    typeFunction
                        { from = typeVariable { name = "a" }
                        , fromIsMutable = False
                        , to = typeVariable { name = "a" }
                        }
                , forall = forall "a" []
                , mutable = False
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
                   { type_ =
                       typeFunction
                           { from = typeVariable { name = "0" }
                           , fromIsMutable = False
                           , to = typeVariable { name = "0" }
                           }
                   , forall = forall "0" []
                   , mutable = False
                   }
               }
           , codeTest "Reject disconnected forall var types?"
               """
               id l =
                 as a -> b
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
                   { forall = forall "2" []
                   , mutable = False
                   , type_ =
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
               , expected = Ok { type_ = tyNumber, forall = Dict.empty, mutable = False }
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
               , expected = Ok { type_ = tyNumber, forall = Dict.empty, mutable = False }
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
               , expected = Ok { type_ = tyNumber, forall = Dict.empty, mutable = False }
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
                  as Number -> None
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
                          as Number @> None
                          reset
                        """
            , run = infer "a"
            , expected =
                { type_ = typeFunction { from = tyNumber, fromIsMutable = True, to = tyNone }
                , forall = Dict.empty
                , mutable = False
                }
            }
        , hasError
            { name = "Functions can't be mutable 1"
            , code = "a @= fn x: x"
            , run = infer "a"
            , test = Test.errContain "mutable"
            }
        , codeTest "Functions can't be mutable 2"
            """
            a @f =
              @f := (fn x: x)
            """
            (infer "a")
            (Test.errContain "these mutable values contain functions: f")
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
                  as Number -> Number
                  add 1
                """
            , run = infer "a"
            , test = Test.errContain "mutable"
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
            , code =
                """
                        a l =
                          as List a -> List a
                          l
                        """
            , run = infer "a"
            , expected =
                { type_ =
                    typeFunction
                        { from = typeConstant { args = [ typeVariable { name = "a" } ], ref = "SPCore.List" }
                        , fromIsMutable = False
                        , to = typeConstant { args = [ typeVariable { name = "a" } ], ref = "SPCore.List" }
                        }
                , mutable = False
                , forall = forall "a" []
                }
            }
        , simpleTest
            { name = "Union type constructors"
            , code = "union X a = L"
            , run = infer "L"
            , expected =
                { type_ = typeConstant { args = [ typeVariable { name = "a" } ], ref = "Test.X" }
                , mutable = False
                , forall = forall "a" []
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
                { forall =
                    forallTuple
                        [ ( "1", [ CA.Re CA.S ] )
                        , ( "3", [ CA.Re CA.S ] )
                        , ( "4", [] )
                        ]
                , mutable = False
                , type_ =
                    typeFunction
                        { from =
                            typeRecord
                                { attrs =
                                    Dict.singleton "meh"
                                        (typeRecord
                                            { attrs = Dict.singleton "blah" (typeVariable { name = "4" })
                                            , extensible = Just "3"
                                            }
                                        )
                                , extensible = Just "1"
                                }
                        , fromIsMutable = False
                        , to = typeVariable { name = "4" }
                        }
                }
            )
        , simpleTest
            { name = "Attribute mutation"
            , code =
                """
                        a @b = @b.meh.blah += 1
                        """
            , run = infer "a"
            , expected =
                { forall =
                    forallTuple
                        [ ( "1", [ CA.Re CA.S ] )
                        , ( "3", [ CA.Re CA.S ] )
                        ]
                , mutable = False
                , type_ =
                    typeFunction
                        { from =
                            typeRecord
                                { attrs =
                                    Dict.singleton "meh"
                                        (typeRecord
                                            { attrs = Dict.singleton "blah" (typeConstant { ref = "SPCore.Number", args = [] })
                                            , extensible = Just "3"
                                            }
                                        )
                                , extensible = Just "1"
                                }
                        , fromIsMutable = True
                        , to = typeConstant { ref = "SPCore.None", args = [] }
                        }
                }
            }
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
                 as Number & Number
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
                            , extensible = Just "1"
                            }
                in
                { forall =
                    forallTuple
                        [ ( "1", [ CA.Re CA.S ] )
                        ]
                , mutable = False
                , type_ =
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
                        , extensible = Just "3"
                        }
             in
             Test.okEqual
                { forall =
                    forallTuple
                        [ ( "3", [ CA.Re CA.S ] )
                        ]
                , mutable = False
                , type_ =
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
                { forall =
                    forallTuple
                        [ ( "1", [ CA.Re CA.S ] )
                        , ( "2", [] )
                        ]
                , mutable = False
                , type_ =
                    typeFunction
                        { from =
                            typeRecord
                                { attrs = Dict.fromList [ ( "first", typeVariable { name = "2" } ) ]
                                , extensible = Just "1"
                                }
                        , fromIsMutable = False
                        , to = typeVariable { name = "2" }
                        }
                }
            )
        , codeTest "[reg] refineType when the record has a non-extensible alias"
            """
            alias A = { c as Number, d as Number }

            upd a =
              as A -> A
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
                { forall = forall "1" []
                , mutable = False
                , type_ =
                    typeFunction
                        { from =
                            typeConstant
                                { args = [ typeVariable { name = "1" } ]
                                , ref = "SPCore.List"
                                }
                        , fromIsMutable = False
                        , to = typeVariable { name = "1" }
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
                { forall = forallTuple [ ( "1", [] ), ( "2", [ CA.Re CA.S ] ) ]
                , mutable = False
                , type_ =
                    typeFunction
                        { from =
                            typeRecord
                                { attrs = Dict.fromList [ ( "first", typeVariable { name = "1" } ) ]
                                , extensible = Just "2"
                                }
                        , fromIsMutable = False
                        , to = typeVariable { name = "1" }
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
                as List a -> (a -> b) -> None
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
                { forall = Dict.empty
                , mutable = False
                , type_ =
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
                { forall = Dict.empty
                , mutable = False
                , type_ =
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
