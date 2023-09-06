tests as Test =
    Test.Group
        "TypeCheck"
        [
        , functions
        , statements
        , recursiveTypes
        , variableTypes
        , higherOrderTypes
        , records
        , patterns
        , try_as
        , if_else
        , nonFunction
        , misc
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
    , freeTyvars as Dict TA.TyvarId TA.Tyvar
    # TODO this needs to contain also an Env, otherwise I can't print it properly
    , type as TA.RawType
    }


outToHuman as fn Out: Text =
    fn out:
    type =
        out.type
        >> Human/Type.doRawType Compiler/TypeCheck.initEnv __
        >> Human/Format.formatExpression { isRoot = True, originalContent = "" } __
        >> Fmt.render

    [
    , "  tyvars = " .. Debug.toHuman (Dict.toList out.freeTyvars)
    , "  type = " .. type
    ]
    >> Text.join "\n" __


tyvar as fn Int: TA.RawType =
    TH.taTyvar


freeTyvars as fn [ TA.TyvarId ]: Dict TA.TyvarId TA.Tyvar =
    fn ids:
    List.for Dict.empty ids fn id, d:
        Dict.insert id { maybeAnnotated = Nothing } d


freeTyvarsAnnotated as fn [ TA.TyvarId & Name ]: Dict TA.TyvarId TA.Tyvar =
    fn ids:
    Dict.empty
    >> List.for __ ids fn id & name, d:
        Dict.insert id { maybeAnnotated = Just { allowFunctions = True, name } } d


#
#
#
infer as fn Text: fn Text: Result Text Out =
    fn targetName:
    fn code:
    params as Compiler/MakeCanonical.ReadOnly =
        {
        , errorModule = TH.errorModule code
        , meta = TH.meta
        , umr = TH.moduleUmr
        }

    params
    >> Compiler/MakeCanonical.textToCanonicalModule True __
    >> TH.resErrorToStrippedText
    >> onOk fn caModule:
    [ caModule ]
    >> Compiler/TypeCheck.initStateAndGlobalEnv [] __
    >> TH.resErrorToStrippedText
    >> onOk fn luv & typeCheckGlobalEnv_:
    typeCheckGlobalEnv as Compiler/TypeCheck.Env =
        { typeCheckGlobalEnv_ with
        , variables =
            .variables
            >> Dict.insert
                (RefGlobal << USR TH.moduleUmr "add")
                {
                , definedAt = Pos.T
                , freeTyvars = Dict.empty
                , freeUnivars = Dict.empty
                , type = toImm << TH.taFunction [ TH.taNumber, TH.taNumber ] TH.taNumber
                }
                __
            >> Dict.insert
                (RefGlobal << USR TH.moduleUmr "reset")
                {
                , definedAt = Pos.T
                , freeTyvars = Dict.empty
                , freeUnivars = Dict.empty
                , type = toImm << TH.taFunction [ TH.taNumber ] TH.taNone
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
    toMatch as fn CA.Pattern & TA.ValueDef: Maybe TA.ValueDef =
        fn pattern & def:
        try pattern as
            , CA.PatternAny _ (Just name) maybeAnnotation: if name == targetName then Just def else Nothing
            , _: Nothing

    matches =
        moduleWithDestroy.valueDefs
        >> Dict.toList
        >> List.filterMap toMatch __

    try matches as

        , []:
            Err "dict fail"

        , def :: tail:
            {
            , freeTyvars = def.freeTyvars
            , type = def.type.raw
            }
            >> normalizeOut
            >> Ok


normalizeOut as fn Out: Out =
    fn out:
    !hash =
        Hash.fromList []

    {
    , freeTyvars = Dict.for Dict.empty out.freeTyvars (fn id, tc, d: Dict.insert (TA.normalizeTyvarId @hash id) tc d)
    , type = TA.normalizeType @hash out.type
    }


#
# Functions
#

functions as Test =
    Test.Group
        "functions"
        [
        , codeTest
            "Known function with correct params"
            "a = add 3 1"
            (infer "a")
            (Test.isOkAndEqualTo
                 {
                 , freeTyvars = Dict.empty
                 , type = TH.taNumber
                 }
            )
        , codeTest "Known function with wrong *number* of args" "a = add False" (infer "a") (Test.errorContains [ "Number", "Arguments" ])
        , codeTest "Known function with wrong params" "a = add False 1" (infer "a") (Test.errorContains [ "Bool" ])
        , codeTest
            "Function inference 1"
            "a = fn x: add x 1"
            (infer "a")
            (Test.isOkAndEqualTo
                 {
                 , freeTyvars = Dict.empty
                 , type = TH.taFunction [ TH.taNumber ] TH.taNumber
                 }
            )
        , codeTest
            "Function inference 2: same as 1, but with swapped args"
            "a = fn x: add 1 x"
            (infer "a")
            (Test.isOkAndEqualTo
                 {
                 , freeTyvars = Dict.empty
                 , type = TH.taFunction [ TH.taNumber ] TH.taNumber
                 }
            )
        , codeTest
            "[reg] fn had type None"
            "a = fn x: 1"
            (infer "a")
            (Test.isOkAndEqualTo
                 {
                 , freeTyvars = freeTyvars [ 1 ]
                 , type = TA.TypeFn [ tyvar 1 >> toImm >> TA.ParSp ] (toUni TH.taNumber)
                 }
            )
        , codeTest
            "[reg] Multiple arguments are correctly inferred"
            """
            a = fn x, y, z: x + y + z
            """
            (infer "a")
            Test.isOk
        , codeTest
            "Annotation should be consistent with mutability"
            """
            f as fn @Number: Number = fn a:
              a
            """
            (infer "f")
            (Test.errorContains [ "RecyclingDoesNotMatch" ])
        , codeTest
            """
            [reg] Free tyvar should not be compatible with constructor
            """
            """
            listCons as fn item: item =
                fn item:
                []
            """
            (infer "listCons")
            (Test.errorContains [ "Incompatible" ])
        , codeTest
            """
            Annotations that are too general should be rejected
            """
            """
            f as fn a: b =
                fn a: a
            """
            (infer "f")
            (Test.errorContains [])
        ]


#
# Statements
#

statements as Test =
    Test.Group
        "statements"
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
            (Test.isOkAndEqualTo { freeTyvars = Dict.empty, type = TH.taBool })
        , codeTest
            """
            Definition statements return type None
            """
            """
            a =
              f = fn x: 3
            """
            (infer "a")
            (Test.isOkAndEqualTo { freeTyvars = Dict.empty, type = TH.taNone })
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
# Recusrive/circular types
#

recursiveTypes as Test =
    Test.Group
        "Recursive types"
        [
        , codeTest
            """
            Normal types cannot be self recursive
            """
            """
            alias A = { a as A }
            a as A = todo ""
            """
            (infer "a")
            (Test.errorContains [ "Circular" ])
        , codeTest
            """
            Normal types cannot be mutually recursive
            """
            """
            alias A = { b as B }
            alias B = { a as A }
            a as A = todo ""
            """
            (infer "a")
            (Test.errorContains [ "Circular" ])
        , codeTest
            """
            Variant types can be recursive
            """
            """
            var A = 'a2 B
            alias B = { a as A }
            a as A = todo ""
            b as B = todo ""
            """
            (infer "a")
            Test.isOk
        ]


#
# Variable types
#

variableTypes as Test =
    Test.Group
        "Variable types"
        [
        , codeTest
            """
            Identity, annotated
            """
            """
            id as fn a: a =
              fn a: a
            """
            (infer "id")
            (Test.isOkAndEqualTo
                 {
                 , freeTyvars = freeTyvarsAnnotated [ 1 & "a" ]
                 , type = TH.taFunction [ tyvar 1 ] (tyvar 1)
                 }
            )
        , codeTest
            """
            Identity, inferred
            """
            """
            id =
              fn a: a
            """
            (infer "id")
            (Test.isOkAndEqualTo
                 {
                 , freeTyvars = freeTyvars [ 1 ]
                 , type = TH.taFunction [ tyvar 1 ] (tyvar 1)
                 }
            )
        , codeTest
            """
            Annotated vars are instantiated when referenced
            """
            """
            q as [item] =
              Core.Nil

            r as [Text] =
                  q
            """
            (infer "r")
            Test.isOk
        , codeTest
            """
            [reg] on is missing tyvars
            """
            """
            andThen as [a] = []

            on = andThen
            """
            (infer "on")
            (Test.isOkAndEqualTo
                 {
                 , freeTyvars = freeTyvars [ 1 ]
                 , type = TH.taList (tyvar 1)
                 }
            )
        , codeTest
            """
            [reg] Unifying functions does not unfiy their args
            """
            """
            var Dict_ k v = 'empty
            dict_member as fn k, Dict_ k v: Bool = todo ""
            dict_filter as fn (fn k, v: Bool), Dict_ k v: Dict_ k v = todo ""

            freeTyvars as Dict_ Number {} = todo ""
            typeTyvars as Dict_ Number None = todo ""

            x = dict_filter (fn k, v: dict_member v typeTyvars) freeTyvars
            """
            (infer "x")
            (Test.errorContains [ "{}", "Number" ])
        ]


#
# Higher order types
#

higherOrderTypes as Test =
    Test.Group
        "higher order types"
        [
        , codeTest
            """
            Parse precedence
            """
            """
            var T a = 't a

            a as fn T a: T a =
                fn l: l
            """
            (infer "a")
            (Test.isOkAndEqualTo
                 {
                 , freeTyvars = freeTyvarsAnnotated [ 1 & "a" ]
                 , type = TH.taFunction [ TA.TypeExact (TH.localType "T") [ tyvar 1 ] ] (TA.TypeExact (TH.localType "T") [ tyvar 1 ])
                 }
            )
        , codeTest
            """
            Variant type constructors
            """
            """
            var X a = 'l
            l = 'l
            """
            (infer "l")
            (Test.isOkAndEqualTo
                 {
                 , freeTyvars = freeTyvars [ 1 ]
                 , type = TA.TypeExact (TH.localType "X") [ tyvar 1 ]
                 }
            )
        , codeTest
            """
            [reg] type check mistakes a variant type with free tyvars for a free tyvar?
            """
            """
            var O r e o = 'o r e o

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
            var O o = 'o Text o

            fun as Number: Text: O wrong = _: a:
                'o a a
            """
            (infer "fun")
            (Test.errorContains [ "wrong" ])
        , codeTest
            """
            [reg] Should complain about undefined type argument
            """
            """
            var O a = 'o Text output
            x = 1
            """
            (infer "x")
            (Test.errorContains [ "output" ])
        , codeTest
            """
            [reg] Named vars can't be refined?
            """
            """
            var Wrap a = 'w a

            f as fn a: Wrap a =
                fn a: a
            """
            (infer "f")
            (Test.errorContains [ "Wrap" ])
        ]


#
# Records
#

records as Test =
    Test.Group
        "Records"
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
                 , freeTyvars = freeTyvars [ 1, 2, 3 ]
                 , type =
                     TH.taFunction
                         [
                         , TA.TypeRecord (Just 1) (Dict.ofOne "meh" (TA.TypeRecord (Just 2) (Dict.ofOne "blah" (tyvar 3))))
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
                 , freeTyvars = freeTyvars [ 1, 2 ]
                 , type =
                     TA.TypeFn
                         [
                         , TA.ParRe << TA.TypeRecord (Just 1) (Dict.ofOne "meh" (TA.TypeRecord (Just 2) (Dict.ofOne "blah" TH.taNumber)))
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
                 (TA.TypeRecord (Just 1) (Dict.ofOne "x" TH.taNumber)
                  >> (fn re:
                       {
                       , freeTyvars = freeTyvars [ 1 ]
                       , type = TH.taFunction [ re ] re
                       }
                  )
                 )
            )
        , codeTest
            "SKIP(needs reordering) instantiate and refine inferred records"
            """
            a = fn t: { t with x = 1 }
            c = a
            """
            (infer "c")
            (Test.isOkAndEqualTo
                 (TA.TypeRecord (Just 1) (Dict.ofOne "x" TH.taNumber)
                  >> (fn re:
                       {
                       , freeTyvars = Dict.empty
                       , type = TH.taFunction [ re ] re
                       }
                  )
                 )
            )
        , codeTest
            "[reg] excessive forallness in records"
            """
            x =
              fn q:
              a = q.first
              a
            """
            (infer "x")
            (Test.isOkAndEqualTo
                 (TA.TypeRecord (Just 1) (Dict.ofOne "first" (tyvar 2))
                  >> (fn re:
                       {
                       , freeTyvars = freeTyvars [ 1, 2 ]
                       , type = TH.taFunction [ re ] (tyvar 2)
                       }
                  )
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
        , codeTest
            """
            [reg] Record missing attributes
            """
            """
            alias R = { x as Number, y as Number }

            r as R = {
              , x = 3
              }
            """
            (infer "r")
            (Test.errorContains [ "Missing" ])
        ]


#
# Patterns
#

patterns as Test =
    Test.Group
        "Patterns"
        [
        , codeTest
            """
            Constructor unpacking
            """
            """
            var Z a = 'z a

            identityFunction =
               fn a:
               'z b = 'z a
               b
            """
            (infer "identityFunction")
            (Test.isOkAndEqualTo
                 {
                 , freeTyvars = freeTyvars [ 1 ]
                 , type = TH.taFunction [ tyvar 1 ] (tyvar 1)
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
                 {
                 , freeTyvars = freeTyvars [ 1 ]
                 , type = TH.taFunction [ TH.taList (tyvar 1) ] (tyvar 1)
                 }
            )
        , codeTest
            """
            Complete records are correctly unpacked
            """
            """
            x =
                fn q:
                { first } = q
                first
            """
            (infer "x")
            (#
             Test.isOkAndEqualTo
                 {
                 , freeTyvars = freeTyvars [ 1 ]
                 , type = TH.taFunction [ TA.TypeRecord Nothing (Dict.fromList [ "first" & tyvar 1 ]) ] (tyvar 1)
                 }
            )
        , codeTest
            """
            Incomplete records are correctly unpacked
            """
            """
            x =
                fn q:
                { with first } = q
                first
            """
            (infer "x")
            (#
             Test.isOkAndEqualTo
                 {
                 , freeTyvars = freeTyvars [ 1, 2 ]
                 , type = TH.taFunction [ TA.TypeRecord (Just 2) (Dict.fromList [ "first" & tyvar 1 ]) ] (tyvar 1)
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
                    , Core.Nil: None

            result =
                1 :: Core.Nil = Core.Nil
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
    Test.Group
        "try..as"
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
                 {
                 , freeTyvars = Dict.empty
                 , type = TA.TypeFn [ TH.taBool >> toImm >> TA.ParSp ] (toUni TH.taNumber)
                 }
            )
        #
        , codeTest
            """
            rejects non-matching patterns
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
            (Test.errorContains [ "Number", "Bool" ])
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
    Test.Group
        "if..else"
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
                 {
                 , freeTyvars = Dict.empty
                 , type = TA.TypeFn [ TH.taBool >> toImm >> TA.ParSp ] (toUni TH.taNumber)
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
            (Test.errorContains [ "Bool" ])
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
            (Test.errorContains [ "Number" ])
        ]


#
# NonFunction
#

nonFunction as Test =
    Test.Group
        "NonFunction"
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
            (Test.errorContains [ "ErrorTypeAllowsFunctions" ])
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
            (Test.errorContains [ "ErrorTypeAllowsFunctions" ])
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
                 , freeTyvars = Dict.ofOne 1 { maybeAnnotated = Nothing }
                 , type = TH.taNumber
                 }
            )
        ]


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

#
# Misc
#

misc as Test =
    Test.Group
        "Misc"
        [
        , codeTest
            """
            [reg] Undefined types should be rejected
            """
            """
            v as ThisTypeIsNotDefined = todo ""
            """
            (infer "v")
            (Test.errorContains [ "ThisTypeIsNotDefined" ])
        , codeTest
            """
            Placeholder works with unique args
            """
            """
            stuff as fn !Number: Number = todo ""
            v =
                1 >> stuff __
            """
            (infer "v")
            (Test.isOkAndEqualTo
                 {
                 , freeTyvars = Dict.empty
                 , type = TH.taNumber
                 }
            )
        , codeTest
            """
            [reg] named tyvars should not "bleed" to other definitions
            """
            """
            var DD q =
                , 'RBEmpty_elm_builtin

            empty as DD key =
                'RBEmpty_elm_builtin

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
            var DD a b = 'Blah

            ddget as fn a, DD a b: DD a b =
                fn a, b:
                'Blah

            formatSnippet as Text =
                try [""] as
                    , ["emphasys", s]: s

            fmtBlock as Text =
                try ddget 1 'Blah as
                    , 'Blah:
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
