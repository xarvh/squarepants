
tests as Test =
    Test.Group "TypeCheck"
        [
        , functions
        , statements
        , variableTypes
        , higherOrderTypes
        , records
        , patterns
        , try_as
        , if_else
        , nonFunction
        , misc
        #, zzzz
        ]


# TODO test rejection of circular aliases
# TODO test rejection of arguments with the same name


#
#
#


codeTest =
    Test.codeTest outToHuman __ __ __ __


alias Out =
    {
    , type as TA.RawType
    , freeTyvars as Dict TA.TyvarId TA.Tyvar
    }


outToHuman as fn Out: Text =
    fn out:

    [
    , "  tyvars = " .. Debug.toHuman (Dict.toList out.freeTyvars)
    , "  type = " .. Human/Type.display "" (Human/Type.doRawType {} out.type)
    ]
    >> Text.join "\n" __


unionCons as fn Int, Text: TA.RawType =
    fn id, consName:
    TA.TypeUnion (Just id) (Dict.ofOne consName [])


tyvar as fn Int: TA.RawType =
    TH.taTyvar


freeTyvars as fn [TA.TyvarId]: Dict TA.TyvarId TA.Tyvar =
    fn ids:

    List.for Dict.empty ids fn id, d:
        Dict.insert id {
            , originalName = ""
            , allowFunctions = True
            , generalizedAt = Pos.G
            , generalizedFor = RefLocal ""
            }
            d


freeTyvarsAnnotated as fn [TA.TyvarId & Name]: Dict TA.TyvarId TA.Tyvar =
    fn ids:
    Dict.empty
    >> List.for __ ids fn (id & originalName), d:
        Dict.insert id
              {
              , originalName
              , allowFunctions = True
              , generalizedAt = Pos.G
              , generalizedFor = RefLocal ""
              }
              d


#
#
#
infer as fn Text: fn Text: Result Text Out =
    fn targetName: fn code:

    params as Compiler/MakeCanonical.ReadOnly =
        {
        , meta = TH.meta
        , umr = TH.moduleUmr
        , errorModule = TH.errorModule code
        }

    params
    >> Compiler/MakeCanonical.textToCanonicalModule True __
    >> TH.resErrorToStrippedText
    >> onOk fn caModule:

    [caModule]
    >> Compiler/TypeCheck.initStateAndGlobalEnv [] __
    >> TH.resErrorToStrippedText
    >> onOk fn (luv & typeCheckGlobalEnv_):

    typeCheckGlobalEnv as Compiler/TypeCheck.Env =
        { typeCheckGlobalEnv_ with
        , variables = .variables
            >> Dict.insert
                (RefGlobal << USR TH.moduleUmr "add")
                {
                , definedAt = Pos.T
                , type = toImm << TH.taFunction [TH.taNumber, TH.taNumber] TH.taNumber
                , freeTyvars = Dict.empty
                , freeUnivars = Dict.empty
                }
                __
            >> Dict.insert
                (RefGlobal << USR TH.moduleUmr "reset")
                {
                , definedAt = Pos.T
                , type = toImm << TH.taFunction [TH.taNumber] TH.taNone
                , freeTyvars = Dict.empty
                , freeUnivars = Dict.empty
                }
                __
        }

    !lastUnificationVarId =
        cloneImm luv

    caModule
    >> Compiler/TypeCheck.doModule @lastUnificationVarId typeCheckGlobalEnv __
    >> TH.resErrorToStrippedText
    >> onOk fn taModule:

    taModule
    >> Compiler/UniquenessCheck.doModule
    >> TH.resErrorToStrippedText
    >> onOk fn moduleWithDestroy:

    toMatch as fn (CA.Pattern & TA.ValueDef): Maybe TA.ValueDef =
        fn (pattern & def):

        try pattern as
            , CA.PatternAny _ { maybeAnnotation, maybeName = Just name }:
                if name == targetName then Just def else Nothing
            , _:
                Nothing

    matches =
        moduleWithDestroy.valueDefs
        >> Dict.toList
        >> List.filterMap toMatch __

    try matches as
        , []:
            Err "dict fail"

        , def :: tail:
            {
            , type = def.type.raw
            , freeTyvars = def.freeTyvars
            }
            >> normalizeOut
            >> Ok


normalizeOut as fn Out: Out =
    fn out:

    !hash = Hash.fromList []

    {
    , type = TA.normalizeType @hash out.type
    , freeTyvars = Dict.for Dict.empty out.freeTyvars fn id, tc, d: Dict.insert (TA.normalizeTyvarId @hash id) tc d
    }




#
# Functions
#


functions as Test =
    Test.Group "functions"
        [
        , codeTest "Known function with correct params"
            "a = add 3 1"
            (infer "a")
            (Test.isOkAndEqualTo
                {
                , type = TH.taNumber
                , freeTyvars = Dict.empty
                }
            )
        , codeTest "Known function with wrong *number* of args"
            "a = add False"
            (infer "a")
            (Test.errorContains [ "Number", "Arguments" ])
        , codeTest "Known function with wrong params"
            "a = add False 1"
            (infer "a")
            (Test.errorContains [ "False" ])
        , codeTest
            "Function inference 1"
            "a = fn x: add x 1"
            (infer "a")
            (Test.isOkAndEqualTo
                {
                , type = TH.taFunction [TH.taNumber] TH.taNumber
                , freeTyvars = Dict.empty
                }
            )
        , codeTest
            "Function inference 2: same as 1, but with swapped args"
            "a = fn x: add 1 x"
            (infer "a")
            (Test.isOkAndEqualTo
                {
                , type = TH.taFunction [TH.taNumber] TH.taNumber
                , freeTyvars = Dict.empty
                }
            )
        , codeTest
            "[reg] fn had type None"
            "a = fn x: 1"
            (infer "a")
            (Test.isOkAndEqualTo
                {
                , type = TA.TypeFn [tyvar 1 >> toImm >> TA.ParSp] (toUni TH.taNumber)
                , freeTyvars = freeTyvars [1]
                }
            )
        , codeTest "[reg] Multiple arguments are correctly inferred"
            """
            a = fn x, y, z: x + y + z
            """
            (infer "a")
            Test.isOk
        , codeTest "Annotation should be consistent with mutability"
            """
            f as fn @Number: Number = fn a:
              a
            """
            (infer "f")
            (Test.errorContains ["RecyclingDoesNotMatch"])

        , codeTest
            """
            [reg] on is missing tyvars?
            """
            """
            andThen as [a] = []

            on = andThen
            """
            (infer "on")
            (Test.isOkAndEqualTo
                {
                , freeTyvars = freeTyvars [1]
                , type = TH.taList (tyvar 1)
                }
            )
        , codeTest
            # I don't know what's wrong here, but it's very wrong
            """
            [reg] Something's wrong with tyvars
            """
            """
            listCons as fn item: item =
                fn item:
                []
            """
            (infer "listCons")
            (Test.errorContains ["item"])
        ]



#
# Statements
#


statements as Test =
    Test.Group "statements"
        [
        , codeTest
            """
            Statement blocks should return the last statement's type
            """
            """
            a =
              3
              False
            """
            (infer "a")
            (Test.isOkAndEqualTo { type = unionCons 1 "False", freeTyvars = Dict.empty })
        , codeTest
            """
            Definition statement return type None
            """
            """
            a =
              f = fn x: 3
            """
            (infer "a")
            (Test.isOkAndEqualTo { type = unionCons 1 "None", freeTyvars = Dict.empty })
        , codeTest
            """
            [reg] Definition statement with annotation return type None
            """
            """
            a as None =
              f = 3
            """
            (infer "a")
            Test.isOk
        [# TODO move to MakeCanonical?
        , codeTest
            """
            Local values can't shadow root values
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
            Prevent local redeclarations
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
            Prevent root redeclarations
            """
            """
            a = 1
            a = 1
            """
            (infer "b")
            (Test.errorContains [ "declar"])
        #]
        , codeTest
            """
            [reg] Annotated declarations are actually typechecked
            """
            """
            x as None =
                q = 1 + ""
            """
            (infer "x")
            (Test.errorContains [])
        ]



#
# Variable types
#


variableTypes as Test =
    Test.Group "Variable types"
        [
        , codeTest
            "Identity"
            """
            id as fn a: a =
              fn a: a
            """
            (infer "id")
            (Test.isOkAndEqualTo
                { type = TH.taFunction [tyvar 1] (tyvar 1)
                , freeTyvars = freeTyvarsAnnotated [1 & "a"]
                }
            )

        , codeTest
            "Annotated vars are instantiated when referenced"
            """
            q as [item] =
              Nil

            r as [Text] =
                  q
            """
            (infer "r")
            Test.isOk
        ]


#
# Higher order types
#


higherOrderTypes as Test =
    Test.Group "higher order types"
        [
        , codeTest
            """
            Parse precedence
            """
            """
            union T a = T a

            a as fn T a: T a =
                fn l: l
            """
            (infer "a")
            (Test.isOkAndEqualTo
                { type =
                    TH.taFunction
                        [ TA.TypeUnion Nothing (Dict.ofOne "T" [ tyvar 1 ])]
                        ( TA.TypeUnion Nothing (Dict.ofOne "T" [ tyvar 1 ]))
                , freeTyvars = freeTyvarsAnnotated [1 & "a"]
                }
            )
        , codeTest
            """
            Union type constructors
            """
            """
            union X a = L
            l as X a = L
            """
            (infer "l")
            (Test.isOkAndEqualTo
                {
                , freeTyvars = freeTyvarsAnnotated [1 & "a"]
                , type = TA.TypeUnion Nothing (Dict.ofOne "L" [])
                }
            )
        , codeTest
            """
            [reg] type check mistakes a union type with free tyvars for a free tyvar?
            """
            """
            union O r e o = O r e o

            run as fn (fn r: O r e o), r: O r e o =
               fn rToOreo, r:
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
            """
            [reg] Should complain about undefined type argument
            """
            """
            union O a = O Text output
            x = 1
            """
            (infer "x")
            (Test.errorContains [ "output" ])
        , codeTest
            """
            [reg] Named vars can't be refined?
            """
            """
            union Wrap a = Zot a

            f as fn a: Wrap a =
                fn a: a
            """
            (infer "f")
            (Test.errorContains [ "Zot" ])
        ]



#
# Records
#


records as Test =
    Test.Group "Records"
        [
        , codeTest
            """
            Attribute access
            """
            """
            a = fn b: b.meh.blah
            """
            (infer "a")
            (Test.isOkAndEqualTo
                {
                , freeTyvars = freeTyvars [3]
                , type =
                    TH.taFunction
                        [ TA.TypeRecord (Just 1)
                            (Dict.ofOne "meh" ( TA.TypeRecord (Just 2) (Dict.ofOne "blah" (tyvar 3))))
                        ]
                        (tyvar 3)
                }
            )
        , codeTest
            """
            Attribute mutation
            """
            """
            a = fn @b: @b.meh.blah += 1
            """
            (infer "a")
            (Test.isOkAndEqualTo
                {
                # TODO Should I have free tyvars 1 and 2 here?
                , freeTyvars = Dict.empty
                , type =
                    TA.TypeFn
                        [ TA.ParRe << TA.TypeRecord (Just 1)
                            (Dict.ofOne "meh"
                                ( TA.TypeRecord (Just 2)
                                    (Dict.ofOne
                                        "blah"
                                        TH.taNumber
                                    )
                                )
                            )
                        ]
                        (toImm TH.taNone)
                }
            )
        , codeTest
            """
            Tuple3 direct item mutability
            """
            """
            x =
                !a = 3 & False & 2

                @a.third += 1
            """
            (infer "x")
            Test.isOk
        , codeTest
            """
            Tuple2 direct item mutability, annotated
            """
            """
            x =
               fn _:
               !a as Number & Number =
                 1 & 2

               @a.first += 1
            """
            (infer "x")
            Test.isOk
        , codeTest
            """
            functional update
            """
            """
            a = fn b: { b with x = 1 }
            """
            (infer "a")
            (Test.isOkAndEqualTo
                (TA.TypeRecord (Just 1) (Dict.ofOne "x" TH.taNumber) >> fn re:
                    {
                    # TODO should I have free tyvars here?
                    , freeTyvars = Dict.empty
                    , type = TH.taFunction [re] re
                    }
                )
            )
        , codeTest "SKIP(needs reordering) instantiate and refine inferred records"
            """
            a = fn t: { t with x = 1 }
            c = a
            """
            (infer "c")
            (Test.isOkAndEqualTo
                (TA.TypeRecord (Just 1) (Dict.ofOne "x" TH.taNumber) >> fn re:
                    { freeTyvars = Dict.empty
                    , type = TH.taFunction [re] re
                    }
                )
            )
        , codeTest "[reg] excessive forallness in records"
            """
            x =
              fn q:
              a = q.first
              a
            """
            (infer "x")
            (Test.isOkAndEqualTo
                (TA.TypeRecord (Just 1) (Dict.ofOne "first" (tyvar 2)) >> fn re:
                    # TODO tyvar 1 too?
                    { freeTyvars = freeTyvars [2]
                    , type = TH.taFunction [re] (tyvar 2)
                    }
                )
            )
        , codeTest
            """
            [reg] refineType when the record has a non-extensible alias
            """
            """
            alias A = { c as Number, d as Number }

            upd as fn A: A = fn a:
              { a with c = .c + 1 }
            """
            (infer "upd")
            Test.isOk
        , codeTest
            """
            [reg] infinite recursion on addSubstitution/unify_
            """
            """
            alias B = { l as [Text] }

            readOne as fn B: (Text & B) =
                fn b:
                try b.l as
                    , []: "" & b
                    , [h, ...t]: h & { b with l = t }
            """
            (infer "readOne")
            Test.isOk
        , codeTest
            """
            [reg] unifyToNonExtensibleRecord correctly substitutes the record extension
            """
            """
            alias R = { x as Number, y as Number }

            rec as fn R: R =
                fn s:

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


patterns as Test =
    Test.Group "Patterns"
        [
        , codeTest
            """
            Constructor unpacking
            """
            """
            union Z a = Z a

            identityFunction =
               fn a:
               Z b = Z a
               b
            """
            (infer "identityFunction")
            (Test.isOkAndEqualTo
                { freeTyvars = freeTyvars [1]
                , type = TH.taFunction [tyvar 1] (tyvar 1)
                }
            )
        , codeTest
            """
            List unpacking
            """
            """
            x =
               fn q:
               [ first, second ] = q
               first
            """
            (infer "x")
            (Test.isOkAndEqualTo
                { freeTyvars = freeTyvars [1]
                , type =
                    TH.taFunction
                        [ TH.taList (tyvar 1)]
                        (tyvar 1)
                }
            )
        , codeTest
            """
            Records are correctly unpacked
            """
            """
            x =
                fn q:
                { first } = q
                first
            """
            (infer "x")
            #
            (Test.isOkAndEqualTo
                { freeTyvars = freeTyvars [1]
                , type =
                    TH.taFunction
                        [ TA.TypeRecord Nothing (Dict.fromList [ ( "first" & tyvar 1 ) ])]
                        (tyvar 1)
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
        , codeTest
            """
            [reg] Constructors should instantiate their variable types
            """
            """
            each as fn [a], (fn a: b): None =
                fn ls, f:
                try ls as
                    , Nil: None

            result =
                1 :: Nil = Nil
            """
            (infer "result")
            #
            Test.isOk
            #
        , codeTest
            """
            [reg] Trying to check against an inferred value?
            """
            """
            tuple as Text & Number =
                "" & 1

            x =
                (a as Text) & (b as Number) =
                    tuple
            """
            (infer "x")
            #
            Test.isOk
        ]



#
# Try..As
#


try_as as Test =
    Test.Group "try..as"
        [
        , codeTest
            """
            basic functionality
            """
            """
            x =
                fn q:
                try q as
                    , True: 2
                    , _: 3
            """
            (infer "x")
            (Test.isOkAndEqualTo
                { freeTyvars = Dict.empty
                , type = TA.TypeFn [ unionCons 1 "True" >> toImm >> TA.ParSp ] (toUni TH.taNumber)
                }
            )

        #
        , codeTest
            """
            SKIP rejects non-matching patterns
            """
            """
            x =
                fn q:
                try q as
                    , True: 2
                    , []: 3
            """
            (infer "x")
            (Test.errorContains [ "List", "Bool" ])

        #
        , codeTest
            """
            rejects non-matching patterns
            """
            """
            x =
                fn q:
                try q as
                    , 1: 2
                    , []: 3
            """
            (infer "x")
            (Test.errorContains [ "Number", "Nil" ])

        #
        , codeTest
            """
            rejects non-matching blocks
            """
            """
            x =
                fn q:
                try q as
                    , True: 2
                    , False: False
            """
            (infer "x")
            (Test.errorContains [ "Number", "False" ])
        , codeTest
            """
            [reg] actually infers blocks
            """
            """
            x as Number =
                try "" as
                    , "": y
            """
            (infer "x")
            (Test.errorContains [ "y" ])
        ]



#
# if..else
#


if_else as Test =
    Test.Group "if..else"
        [
        , codeTest
            """
            basic functionality
            """
            """
            x =
                fn q:
                if q then 1
                else 2
            """
            (infer "x")
            (Test.isOkAndEqualTo
                { freeTyvars = Dict.empty
                , type = TA.TypeFn [TH.taBool >> toImm >> TA.ParSp] (toUni TH.taNumber)
                }
            )

        #
        , codeTest
            """
            rejects non-bool conditions
            """
            """
            x =
                fn q:
                if 1 then 1
                else 2
            """
            (infer "x")
            (Test.errorContains [ "Number" ])

        #
        , codeTest
            """
            rejects non-matching blocks
            """
            """
            x =
                fn q:
                if q then 2
                else False
            """
            (infer "x")
            (Test.errorContains [ "Number"])
        ]


#
# NonFunction
#


nonFunction as Test =
    Test.Group "NonFunction"
        [
        , codeTest
            # TODO ----> I need a way to make the typeclass "propagate" when unifying tyvars
            """
            SKIP (burnedout) Basic functionality
            """
            """
            blah as fn [a]: [a] with a NonFunction =
              fn a:
              a

            meh =
                blah [fn x: x]
            """
            (infer "meh")
            (Test.errorContains [ "ErrorTypeAllowsFunctions"])

        , codeTest
            """
            SKIP (burnedout) Constraint is enforced with annotation
            """
            """
            blah as fn [a]: [a] with a NonFunction =
              fn a: a

            meh as fn b: b =
                fn a: blah a
            """
            (infer "meh")
            (Test.errorContains [ "ErrorTypeAllowsFunctions"])

        , codeTest
            """
            SKIP (burnedout) Constraint is enforced without annotation
            """
            """
            blah as fn [a]: [a] with a NonFunction =
                fn a: a

            meh =
                fn a: blah a
            """
            (infer "meh")
            (Test.isOkAndEqualTo
                {
                , type = TH.taNumber
                , freeTyvars = Dict.ofOne 1
                        {
                        , originalName = ""
                        , allowFunctions = False
                        , generalizedAt = Pos.G
                        , generalizedFor = RefLocal ""
                        }
                }
            )

        [#
                , codeTest
                    """
                    [reg] ???
                    """
                    """
                    id as fn i: i = fn i: i

                    sort as fn [a]: [a] with a NonFunction =
                        sortBy id __

                    sortBy as fn (fn a: b), [a]: [a] with b NonFunction =
                        fn function, list:
                        todo "implemented natively"
                    """
                    (infer "sort")
                    (Test.isOk)
        #]




        ]


#
# Misc
#


misc as Test =
    Test.Group
        "Misc"
        [
        , codeTest
            """
            Placeholder work with unique args
            """
            """
            stuff as fn !Number: Number = todo ""
            v =
                1 >> stuff __
            """
            (infer "v")
            (Test.isOkAndEqualTo
                {
                , type = TH.taNumber
                , freeTyvars = Dict.empty
                }
            )

        , codeTest
            """
            [reg] named tyvars should not "bleed" to other definitions
            """
            """
            union DD q =
                , RBEmpty_elm_builtin

            empty as DD key =
                RBEmpty_elm_builtin

            merge as fn (fn key, b, res: res), res: res =
              fn rightStep, initialResult:

              stepState as fn key, b, [key & a] & res: [key & a] & res =
                fn rKey, rValue, q:
                try q.first as
                  , []: q

              initialResult
            """
            (infer "merge")
            Test.isOk

        , codeTest
            """
            [reg] Constructors not being generalized led to tyvar bleed
            """
            """
            union DD a b = Blah

            ddget as fn a, DD a b: DD a b =
                fn a, b:
                Blah

            formatSnippet as Text =
                try [""] as
                    , ["emphasys", s]: s

            fmtBlock as Text =
                try ddget 1 Blah as
                    , Blah:
                        ""
            """
            (infer "formatSnippet")
            Test.isOk

        , codeTest
            """
            [reg] Non-annotated variables are not correctly inserted
            """
            """
            n = 3

            z as Number = n + 1
            """
            (infer "z")
            Test.isOk
        ]

