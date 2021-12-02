

tests =
    as Test
    Test.Group "TypeCheck"
        [ functions
        , statements
        , variableTypes
        , mu
        , higherOrderTypes
        , records
        , patterns
        , try_as
        , if_else
        , nonFunction
        ]



#
#
#


codeTest =
    Test.codeTest outToHuman


outToHuman out =
    as Out: Text

    freeVars =
        # TODO humanize these too
        out.freeTypeVariables

    nf =
        freeVars
            >> Dict.filter (fn k v: v.nonFn)
            >> Dict.keys
            >> Text.join ", "

    [
    , "  freeTypeVariables = [ " .. Text.join ", " (Dict.keys freeVars) .. " ]"
    , "  (NonFunction = [" .. nf .. "])"
    , "  isMutable = " .. Debug.toHuman out.isMutable
    , "  ty = " .. Human/CanonicalAst.typeToText TH.moduleUmr TH.defaultMeta out.ty
    , "  pos = " .. Debug.toHuman out.ty
    ]
        >> Text.join "\n"



simpleTest { name, code, run, expected } =
    Test.codeTest Debug.toHuman name code run (Test.isOkAndEqualTo expected)


hasError { name, code, run, test } =
    Test.codeTest Debug.toHuman name code run test


constant n =
    CA.TypeConstant Pos.T n []


function from to =
    CA.TypeFunction Pos.T from False to


tyNumber =
    Compiler/TestHelpers.numberType


tyNone =
    Compiler/TestHelpers.noneType


ftv n =
    as Text: Dict Text { nonFn as Bool }
    Dict.singleton n { nonFn = False }


forall vars =
    as List Text: Dict Text { nonFn as Bool }

    List.foldl (fn n: Dict.insert n { nonFn = False }) vars Dict.empty



#
# These should be removed once we get rid of the old record declarations
#


typeFunction { from, fromIsMutable, to } =
    CA.TypeFunction Pos.T from fromIsMutable to


typeVariable { name } =
    CA.TypeVariable Pos.T name


typeConstant { ref, args } =
    CA.TypeConstant Pos.T ref args


#
#
#

alias Type =
    CA.Type


testEnv =
    as Compiler/TypeCheck.Env

    e =
        Compiler/TypeCheck.initEnv TH.moduleUmr TH.defaultMeta

    { e with
    , instanceVariables =
        .instanceVariables
            >> Dict.insert
                (CA.RefRoot << Meta.USR TH.moduleUmr "add")
                { definedAt = Pos.T
                , ty = function tyNumber (function tyNumber tyNumber)
                , freeTypeVariables = Dict.empty
                , isMutable = False
                }
            >> Dict.insert
                (CA.RefRoot << Meta.USR TH.moduleUmr "reset")
                { definedAt = Pos.T
                , ty = typeFunction { from = tyNumber, fromIsMutable = True, to = tyNone }
                , freeTypeVariables = Dict.empty
                , isMutable = False
                }
    }







alias Out =
    { freeTypeVariables as Dict Text { nonFn as Bool }
    , ty as Type
    , isMutable as Bool
    }


cleanUpVar var =
    as Compiler/TypeCheck.InstanceVariable: Out

    ty & tyvars =
        HCA.normalizeTypeAndTyvars var.ty var.freeTypeVariables

    { ty = ty
    , freeTypeVariables = var.freeTypeVariables
    , isMutable = var.isMutable
    }


infer name code =
    as Text: Text: Result Text Out

    moduleResult =
        as Res CA.Module
        Compiler/TestHelpers.textToCanonicalModule code

    getInstanceVar env =
        as Compiler/TypeCheck.Env: Result Text Out
        try Dict.get (TH.rootLocal name) env.instanceVariables as
            Nothing:
                Err "Dict fail"
            Just var:
                Ok << cleanUpVar var

    typeCheckModule m =
        as CA.Module: Res Compiler/TypeCheck.Env
        Compiler/TypeCheck.fromModule (Compiler/TypeCheck.addModuleToEnv m testEnv) m

    moduleResult
        >> Result.andThen typeCheckModule
        >> Compiler/TestHelpers.resErrorToStrippedText code
        >> Result.andThen getInstanceVar



#
# Functions
#


functions =
    as Test
    Test.Group "functions"
        [ codeTest "Known function with correct params"
            "a = add 3 1"
            (infer "a")
            (Test.isOkAndEqualTo
                { ty = tyNumber
                , freeTypeVariables = Dict.empty
                , isMutable = False
                }
            )
        , codeTest "Known function with wrong params"
            "a = add False"
            (infer "a")
            (Test.errorContains [ "Bool"])
        , simpleTest
            { name = "Function inference 1"
            , code = "a = x: add x 1"
            , run = infer "a"
            , expected =
                { ty = function tyNumber tyNumber
                , freeTypeVariables = Dict.empty
                , isMutable = False
                }
            }
        , simpleTest
            { name = "Function inference 2: same as 1, but with swapped args"
            , code = "a = x: add 1 x"
            , run = infer "a"
            , expected =
                { ty = function tyNumber tyNumber
                , freeTypeVariables = Dict.empty
                , isMutable = False
                }
            }
        , simpleTest
            { name = "[reg] fn has type None"
            , code = "a = x: 1"
            , run = infer "a"
            , expected =
                { freeTypeVariables = ftv "1"
                , isMutable = False
                , ty =
                    typeFunction
                        { from = typeVariable { name = "a" }
                        , fromIsMutable = False
                        , to = CoreTypes.number
                        }
                }
            }

        #
        , codeTest "[reg] Multiple arguments are correctly inferred"
            """
            a = x: y: z: x + y + z
            """
            (infer "a")
            Test.isOk

        #
        , codeTest "Annotation should be consistent with mutability"
            """
            f as Number @: Number = a:
              a
            """
            (infer "f")
            (Test.errorContains [ "mutability"])
        ]



#
# Statements
#


statements =
    as Test
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
            (Test.isOkAndEqualTo { ty = CoreTypes.bool, freeTypeVariables = Dict.empty, isMutable = False })
        , codeTest
            """
            Definition statement return type None
            """
            """
            a =
              f = x: 3
            """
            (infer "a")
            (Test.isOkAndEqualTo { ty = CoreTypes.none, freeTypeVariables = Dict.empty, isMutable = False })
        , codeTest
            """
            SKIP Local values can't shadow root values
            """
            """
            a = 1
            b as Number =
                a = 1
                a
            """
            (infer "b")
            (Test.errorContains [ "already"])
        , codeTest
            """
            SKIP Prevent local redeclarations
            """
            """
            b =
              a = 1
              a = 1
            """
            (infer "b")
            (Test.errorContains [ "declar"])
        , codeTest
            """
            SKIP Prevent root redeclarations
            """
            """
            a = 1
            a = 1
            """
            (infer "b")
            (Test.errorContains [ "declar"])
        ]



#
# Variable types
#


variableTypes =
    as Test
    Test.Group "Variable types"
        [ simpleTest
            { name = "Identity"
            , code =
                """
                id as a: a =
                  a: a
                """
            , run = infer "id"
            , expected =
                { ty =
                    typeFunction
                        { from = typeVariable { name = "0a" }
                        , fromIsMutable = False
                        , to = typeVariable { name = "0a" }
                        }
                , freeTypeVariables = ftv "0a"
                , isMutable = False
                }
            }
        ]


mu =
    as Test
    Test.Group "mutability"
        [ hasError
            { name = "Statement blocks that define mutables can't return functions"
            , code =
                """
                a =
                  x @= 1
                  y: y
                """
            , run = infer "a"
            , test = Test.errorContains [ "can't return functions" ]
            }
        , codeTest "Immutable variables can't be used as mutable"
            """
            a = x:
              @x := 1
            """
            (infer "a")
            (Test.errorContains [ "mutable"])
        , hasError
            { name = "Detect mismatching annotations"
            , code =
                """
                a as Number: None =
                  reset
                """
            , run = infer "a"
            , test = Test.errorContains [ "utability"]
            }
        , simpleTest
            { name = "Correctly unify annotation's mutability"
            , code =
                """
                a as Number @: None =
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
            , code = "a @= x: x"
            , run = infer "a"
            , test = Test.errorContains ["utable"]
            }
        , codeTest
            "Functions can't be mutable 2"
            """
            a = @f:
                @f := (x: x)
            """
            (infer "a")
            (Test.errorContains [ "mutable args cannot be functions"])
        , codeTest
            "Functions can't be mutable 3"
            """
            a = @f:
              f 1
            """
            (infer "a")
            (Test.errorContains [ "mutable args cannot be functions"])
        , hasError
            { name = "Lambda argument mutability is correctly inferred"
            , code = "a = x: reset x"
            , run = infer "a"
            , test = Test.errorContains [ "mutability clash" ]
            }
        , hasError
            { name = "*Nested* lambda argument mutability is correctly inferred"
            , code = "a = x: (y: reset y) x"
            , run = infer "a"
            , test = Test.errorContains [ "mutability clash"]
            }
        , hasError
            { name = "Functions can't be mutable (annotation)"
            , code =
                """
                a as Number: Number @=
                  add 1
                """
            , run = infer "a"
            , test = Test.errorContains [ "utable" ]
            }
        , codeTest
            "Mutables can contain functions via free tyvars"
            """
            a = x:
              s @= x
              s

            z as x: x =
              a (x: x)
            """
            (infer "a")
            Test.isOk
        ]



#
# Higher order types
#


higherOrderTypes =
    as Test
    Test.Group "higher order types"
        [ simpleTest
            { name = "Parse precedence"
            , code =
                """
                union T a = T a

                a as T a: T a =
                    l: l
                """
            , run = infer "a"
            , expected =
                { ty =
                    typeFunction
                        { from = CA.TypeConstant Pos.T (TH.rootLocal "T") [ typeVariable { name = "0a" } ]
                        , fromIsMutable = False
                        , to = CA.TypeConstant Pos.T (TH.rootLocal "T") [ typeVariable { name = "0a" } ]
                        }
                , isMutable = False
                , freeTypeVariables = ftv "0a"
                }
            }
        , simpleTest
            { name = "Union type constructors"
            , code = "union X a = L"
            , run = infer "L"
            , expected =
                { ty = typeConstant { args = [ typeVariable { name = "a" } ], ref = TH.rootLocal "X" }
                , isMutable = False
                , freeTypeVariables = ftv "a"
                }
            }
        , codeTest
            "SKIP [reg] type check mistakes a union type with free tyvars for a free tyvar?"
            """
            union O r e o = O r e o

            run as (r: O r e o): r: O r e o = rToOreo: r:
                rToOreo r
            """
            (infer "run")
            Test.isOk
        , codeTest
            """
            [reg] Wrong should be Text
            """
            """
            union O o = O Text o

            fun as Number: Text: O wrong = _: a:
                O a a
            """
            (infer "fun")
            (Test.errorContains [ "wrong"])
        , codeTest
            # TODO this should be MakeCanonical's responsibility?
            """
            SKIP [reg] Should complain about undefined type argument
            """
            """
            union O a = O Text output
            x = 1
            """
            (infer "x")
            (Test.errorContains [ "undefined"])
        ]



#
# Records
#


records =
    as Test
    Test.Group "Records"
        [ codeTest "Attribute access"
            """
            a = b: b.meh.blah
            """
            (infer "a")
            (Test.isOkAndEqualTo
                { freeTypeVariables = forall [ "2", "4", "5" ]
                , isMutable = False
                , ty =
                    typeFunction
                        { from =
                            CA.TypeRecord (Pos.I 2)
                                (Just "a")
                                (Dict.singleton "meh"
                                    (CA.TypeRecord (Pos.I 2)
                                        (Just "b")
                                        (Dict.singleton "blah" (typeVariable { name = "c" }))
                                    )
                                )
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
            a = @b: @b.meh.blah += 1
            """
            (infer "a")
            (Test.isOkAndEqualTo
                { freeTypeVariables = forall [ "2", "4" ]
                , isMutable = False
                , ty =
                    typeFunction
                        { from =
                            CA.TypeRecord (Pos.I 2)
                                (Just "a")
                                (Dict.singleton "meh"
                                    (CA.TypeRecord (Pos.I 2)
                                        (Just "b")
                                        (Dict.singleton "blah" CoreTypes.number)
                                    )
                                )
                        , fromIsMutable = True
                        , to = CoreTypes.none
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
            x = y:
               a as Number & Number @=
                 1 & 2

               @a.first += 1
            """
            (infer "x")
            Test.isOk
        , simpleTest
            { name = "functional update"
            , code = "a = b: { b with x = 1 }"
            , run = infer "a"
            , expected =
                CA.TypeRecord Pos.T (Just "a") (Dict.singleton "x" CoreTypes.number) >> fn re:
                    { freeTypeVariables = forall [ "2" ]
                    , isMutable = False
                    , ty = typeFunction { from = re , fromIsMutable = False , to = re }
                    }
            }
        , codeTest "SKIP instantiate and refine inferred records"
            """
            a = t: { t with x = 1 }
            c = a
            """
            (infer "c")
            (CA.TypeRecord Pos.T (Just "a") (Dict.singleton "x" CoreTypes.number) >> fn re:
                 Test.isOkAndEqualTo
                    { freeTypeVariables = forall [ "a" ]
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
            x = q:
             a = q.first
             a
            """
            (infer "x")
            (Test.isOkAndEqualTo
                { freeTypeVariables = forall [ "3", "4" ]
                , isMutable = False
                , ty =
                    typeFunction
                        { from =
                            CA.TypeRecord (Pos.I 2)
                                (Just "a")
                                (Dict.fromList [ ( "first" & typeVariable { name = "b" } ) ])
                        , fromIsMutable = False
                        , to = typeVariable { name = "b" }
                        }
                }
            )
        , codeTest "[reg] refineType when the record has a non-extensible alias"
            """
            alias A = { c as Number, d as Number }

            upd as A: A = a:
              { a with c = .c + 1 }
            """
            (infer "upd")
            Test.isOk
        , codeTest "[reg] infinite recursion on addSubstitution/unify_"
            """
            alias B = { l as [Text] }

            readOne as B: (Text & B) = b:
                try b.l as
                    []: "" & b
                    h :: t: h & { b with l = t }
            """
            (infer "readOne")
            Test.isOk
        , codeTest "[reg] unifyToNonExtensibleRecord correctly substitutes the record extension"
            """
            alias R = { x as Number, y as Number }

            rec as R: R =
                s:
                    if True then
                        { s with y = .y }
                    else
                        rec { s with y = .y }
            """
            (infer "rec")
            Test.isOk
        ]



#
# Patterns
#


patterns =
    as Test
    Test.Group "Patterns"
        [ codeTest "List unpacking"
            """
            x = q:
               [ first, second ] = q
               first
            """
            (infer "x")
            #
            (Test.isOkAndEqualTo
                { freeTypeVariables = ftv "2"
                , isMutable = False
                , ty =
                    typeFunction
                        { from = CoreTypes.list ( CA.TypeVariable (Pos.I 11) "a" )
                        , fromIsMutable = False
                        , to = CA.TypeVariable (Pos.I 11) "a"
                        }
                }
            )
        , codeTest "Records are correctly unpacked"
            """
            x = q:
                { first } = q
                first
            """
            (infer "x")
            #
            (Test.isOkAndEqualTo
                { freeTypeVariables = forall [ "2", "3" ]
                , isMutable = False
                , ty =
                    typeFunction
                        { from =
                            CA.TypeRecord Pos.T
                                (Just "a")
                                (Dict.fromList [ ( "first" & typeVariable { name = "b" } ) ])
                        , fromIsMutable = False
                        , to = typeVariable { name = "b" }
                        }
                }
            )
         [# TODO
             I can't reproduce this error in the unit tests.
             Even if I copy all code verbatim here, the error does not appear.

             I can only reproduce it on the dev environment and not reliably.
             I don't fully understand what causes it.

             Still, the problem is caused at least in part by the fact that I'm not instantiating the type for type constructors when inferring patterns
             (In TypeCheck.fromPattern#CA.PatternConstructor) which is definitely something worth fixing.

             But still, I don't understand the problem enough to reproduce it reliably.
          #]
        , codeTest "[reg] Constructors should instantiate their variable types"
            """
            each as [a]: (a: b): None =
                ls: f:
                try ls as
                    SPCore.Nil:
                        None

            result =
                  1 :: SPCore.Nil = SPCore.Nil
            """
            (infer "result")
            #
            Test.isOk
        ]



#
# Try..As
#


try_as =
    as Test
    Test.Group "try..as"
        [ codeTest "basic functionality"
            """
            x = q:
                try q as
                    True: 2
                    _: 3
            """
            (infer "x")
            (Test.isOkAndEqualTo
                { freeTypeVariables = Dict.empty
                , isMutable = False
                , ty =
                    typeFunction
                        { from = CoreTypes.bool
                        , fromIsMutable = False
                        , to = CoreTypes.number
                        }
                }
            )

        #
        , codeTest "rejects non-matching patterns"
            """
            x = q:
                try q as
                    True: 2
                    []: 3
            """
            (infer "x")
            (Test.errorContains [ "List", "Bool" ])

        #
        , codeTest "rejects non-matching blocks"
            """
            x = q:
             try q as
               True: 2
               False: False
            """
            (infer "x")
            (Test.errorContains [ "Number", "Bool" ])
        ]



#
# if..else
#


if_else =
    as Test
    Test.Group "if..else"
        [ codeTest "basic functionality"
            """
            x = q:
              if q then 1
              else 2
            """
            (infer "x")
            (Test.isOkAndEqualTo
                { freeTypeVariables = Dict.empty
                , isMutable = False
                , ty =
                    typeFunction
                        { from = CoreTypes.bool
                        , fromIsMutable = False
                        , to = CoreTypes.number
                        }
                }
            )

        #
        , codeTest "rejects non-bool conditions"
            """
            x = q:
              if 1 then 1
              else 2
            """
            (infer "x")
            (Test.errorContains [ "Bool"])

        #
        , codeTest "rejects non-matching blocks"
            """
            x = q:
              if q then 2
              else False
            """
            (infer "x")
            (Test.errorContains [ "Number"])
        ]



#
# NonFunction
#


nonFunction =
    as Test
    Test.Group "NonFunction"
        [
        codeTest
            """
            SKIP Basic functionality
            """
            """
            blah as List a: List a =
              with a NonFunction
              a:
              a

            meh =
              blah [x: x]
            """
            (infer "meh")
            (Test.errorContains [ "should not contain functions"])
        ]
