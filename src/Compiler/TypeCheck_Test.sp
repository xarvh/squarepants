
tests as Test =
    Test.Group "TypeCheck" [
        , functions
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


# TODO test rejection of circular aliases
# TODO test rejection of arguments with the same name


#
#
#


codeTest =
    Test.codeTest outToHuman


alias Out =
    {
    , type as TA.Type
    , freeTyvars as Dict TA.TyvarId TA.Tyvar
    }


outToHuman as Out: Text =
    out:

    [
    , "  tyvars = " .. Debug.toHuman (Dict.toList out.freeTyvars)
    , "  type = " .. Debug.toHuman out.type
    ]
    >> Text.join "\n"



tyvar as Int: TA.Type =
    TH.taTyvar


tyvarImm as Int: TA.Type =
    TH.taTyvarImm


freeTyvarsAnnotated as [TA.TyvarId & Name]: Dict TA.TyvarId TA.Tyvar =
    ids:
    Dict.empty
    >> List.for ids (id & originalName):
        Dict.insert id { originalName, allowFunctions = True, allowUniques = False }


#
#
#
infer as Text: Text: Result Text Out =
    targetName: code:

    params as Compiler/MakeCanonical.Params = {
        , meta = TH.meta
        , stripLocations = True
        , source = TH.source
        , name = TH.moduleName
        }

    Compiler/MakeCanonical.textToCanonicalModule params code
    >> TH.resErrorToStrippedText code
    >> onOk caModule:

    modules as [CA.Module] =
        List.append Prelude.coreModules [caModule]

    Compiler/TypeCheck.initStateAndGlobalEnv modules
    >> TH.resErrorToStrippedText code
    >> onOk (lastUnificationVarId & typeCheckGlobalEnv_):

    typeCheckGlobalEnv as Compiler/TypeCheck.Env =
        { typeCheckGlobalEnv_ with
        , variables = .variables
            >> Dict.insert
                (RefGlobal << USR TH.moduleUmr "add")
                {
                , definedAt = Pos.T
                , type = TH.taFunction [TH.taNumber, TH.taNumber] TH.taNumber
                , freeTyvars = Dict.empty
                , isUnique = False
                }
            >> Dict.insert
                (RefGlobal << USR TH.moduleUmr "reset")
                {
                , definedAt = Pos.T
                , type = TH.taFunction [TH.taNumber] TH.taNone
                , freeTyvars = Dict.empty
                , isUnique = False
                }
        }

    caModule
    >> Compiler/TypeCheck.doModule lastUnificationVarId typeCheckGlobalEnv
    >> TH.resErrorToStrippedText code
    >> onOk taModule:

    taModule
    >> Compiler/UniquenessCheck.doModule
    >> TH.resErrorToStrippedText code
    >> onOk moduleWithDestroy:

    toMatch as (CA.Pattern & TA.ValueDef): Maybe TA.ValueDef =
        (pattern & def):

        try pattern as
            CA.PatternAny _ { isUnique, maybeAnnotation, maybeName = Just name }:
                if name == targetName then Just def else Nothing
            _:
                Nothing

    matches =
        moduleWithDestroy.valueDefs
        >> Dict.toList
        >> List.filterMap toMatch

    try matches as
        []:
            Err "dict fail"

        def :: tail:
            try def.pattern as
                TA.PatternAny _ { isUnique, maybeAnnotation, maybeName, type }:
                    {
                    , type
                    , freeTyvars = def.freeTyvars
                    }
                    >> normalizeOut
                    >> Ok

                _:
                    Err "pattern fail"




normalizeTyvarId as Hash TA.TyvarId TA.TyvarId@: TA.TyvarId: TA.TyvarId =
    hash@: id:

    try Hash.get hash id as
        Just nid: nid
        Nothing:
          maxId @= 0
          Hash.each hash k: v:
            if v > maxId then
                @maxId := v
            else
                None

          nid = maxId + 1
          Hash.insert @hash id nid
          nid


normalizeType as Hash TA.TyvarId TA.TyvarId@: TA.Type: TA.Type =
    hash@: type:

    try type as
        TA.TypeExact uni usr args:
            TA.TypeExact uni usr (List.map (normalizeType @hash) args)

        TA.TypeFn pars out:
            TA.TypeFn
                (List.map (Tuple.mapSecond (normalizeType @hash)) pars)
                (normalizeType @hash out)

        TA.TypeRecord uni attrs:
            TA.TypeRecord uni (Dict.map (k: (normalizeType @hash)) attrs)

        TA.TypeVar id:
            TA.TypeVar (normalizeTyvarId @hash id)

        TA.TypeRecordExt uni id attrs:
            TA.TypeRecordExt uni
                (normalizeTyvarId @hash id)
                (Dict.map (k: (normalizeType @hash)) attrs)

        TA.TypeError:
            TA.TypeError


normalizeOut as Out: Out =
    out:

    hash @= Hash.empty

    {
    , type = normalizeType @hash out.type
    , freeTyvars = Dict.empty >> Dict.for out.freeTyvars id: tc: Dict.insert (normalizeTyvarId @hash id) tc
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
            (Test.errorContains [ "different number of arguments"])
        , codeTest "Known function with wrong params"
            "a = add False 1"
            (infer "a")
            (Test.errorContains [ "Bool"])
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
                , type = TH.taFunction [tyvarImm 1] TH.taNumberAllowUni
                , freeTyvars = Dict.empty
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
            f as @Number: Number = a:
              a
            """
            (infer "f")
            (Test.errorContains [])
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
            (Test.isOkAndEqualTo { type = TH.taBool, freeTyvars = Dict.empty })
        , codeTest
            """
            Definition statement return type None
            """
            """
            a =
              f = fn x: 3
            """
            (infer "a")
            (Test.isOkAndEqualTo { type = TH.taNone, freeTyvars = Dict.empty })
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
                { type = TH.taFunction [tyvarImm 1] (tyvarImm 1)
                , freeTyvars = freeTyvarsAnnotated [1 & "a"]
                }
            )

        , codeTest
            "Annotated vars are instantiated when referenced"
            """
            q as [item] =
              Core.Nil

            r as [Text] =
                  q
            """
            (infer "r")
            Test.isOk
        ]


mu as Test =
    Test.Group "mutability"
        []
[#
        [ codeTest
            "Statement blocks that define mutables can't return functions"
            """
            a =
              @x = 1
              y: y
            """
            (infer "a")
            (Test.errorContains [ "can't return functions" ])
        , codeTest
            "Statement blocks that define mutables can't return functions (with annotation)"
            """
            a as y: y =
              @x = 1
              y: y
            """
            (infer "a")
            (Test.errorContains [ "can't return functions" ])
        , codeTest "Immutable variables can't be used as mutable"
            """
            a = x:
              Core.reinit @x 1
            """
            (infer "a")
            (Test.errorContains [ "mutable"])
#        , codeTest
#            "Detect mismatching annotations"
#            """
#            a as Number: None =
#              reset
#            """
#            (infer "a")
#            (Test.errorContains [ "utability"])
        , codeTest
            "Correctly unify annotation's mutability"
            """
            a as Number:- None =
              reset
            """
            (infer "a")
            (Test.isOkAndEqualTo
                { ty = typeFunction TH.taNumber LambdaConsuming TH.taNone
                , freeTypeVariables = Dict.empty
                , isMutable = False
                }
            )
#        , codeTest
#            "Functions can't be mutable 1"
#            """
#            z =
#                @a = x: x
#            """
#            (infer "z")
#            (Test.errorContains ["unction", "utable"])
#        , codeTest
#            "Functions can't be mutable 2"
#            """
#            a = @f:
#                Core.reinit @f (x: x)
#            """
#            (infer "a")
#            (Test.errorContains [ "mutable args cannot be functions"])
#        , codeTest
#            "Functions can't be mutable 3"
#            """
#            a = @f:
#              f 1
#            """
#            (infer "a")
#            (Test.errorContains [ "mutable args cannot be functions"])
#        , codeTest
#            "Lambda argument mutability is correctly inferred"
#            "a = x: reset @x"
#            (infer "a")
#            (Test.errorContains [ "mutability clash" ])
        , codeTest
            "*Nested* lambda argument mutability is correctly inferred"
            "a = x: (y: reset y) x"
            (infer "a")
            (Test.errorContains [ "mutability clash"])
        , codeTest
            "Functions can't be mutable (annotation)"
            """
            @a as @(Number: Number) =
              add 1
            """
            (infer "a")
            (Test.errorContains [ "utable" ])
        , codeTest
            "Mutables can contain functions via free tyvars"
            """
            a = x:-
              @s = x
              s

            z as x: x =
              a (x: x)
            """
            (infer "a")
            Test.isOk
        , codeTest
            "[reg] Mutable assignment as last stament yields None"
            """
            a as None =
                @x = mut 1
                Core.reinit @x 2
            """
            (infer "a")
            Test.isOk
        ]
#]



#
# Higher order types
#


higherOrderTypes as Test =
    Test.Group "higher order types"
        [
        , codeTest
            "Parse precedence"
            """
            union T a = T a

            a as fn T a: T a =
                fn l: l
            """
            (infer "a")
            (Test.isOkAndEqualTo
                { type =
                    TH.taFunction
                        [ TA.TypeExact (TA.UniIsFixed TA.ForceImm) (TH.localType "T") [ tyvarImm 1 ]]
                        ( TA.TypeExact (TA.UniIsFixed TA.ForceImm) (TH.localType "T") [ tyvarImm 1 ])
                , freeTyvars = freeTyvarsAnnotated [1 & "a"]
                }
            )
        , codeTest
            """
            Union type constructors
            """
            """
            union X a = L
            l = L
            """
            (infer "l")
            (Test.isOkAndEqualTo
                {
                , freeTyvars = Dict.empty
                , type =
                    TA.TypeExact (TA.UniIsFixed TA.ForceImm)
                        (TH.localType "X")
                        [ tyvarImm 1 ]
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
            union Wrap a = W a

            f as fn a: Wrap a =
                fn a: a
            """
            (infer "f")
            (Test.errorContains [ "Wrap"])
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
                , freeTyvars = Dict.empty
                , type =
                    TH.taFunction
                        [ TA.TypeRecordExt TA.ForceImm 1
                            (Dict.singleton "meh" ( TA.TypeRecordExt TA.ForceImm 2 (Dict.singleton "blah" (tyvarImm 3))))
                        ]
                        (tyvarImm 3)
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
                , freeTyvars = Dict.empty
                , type =
                    TA.TypeFn
                        [ Recycle & TA.TypeRecordExt TA.AllowUni 1
                            (Dict.singleton "meh"
                                ( TA.TypeRecordExt TA.AllowUni 2
                                    (Dict.singleton
                                        "blah"
                                        TH.taNumberAllowUni
                                    )
                                )
                            )
                        ]
                        TH.taNone
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
               !a as !(!Number & !Number) =
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
                (TA.TypeRecordExt TA.ForceImm 1 (Dict.singleton "x" TH.taNumber) >> re:
                    { freeTyvars = Dict.empty
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
                (TA.TypeRecordExt TA.AllowUni 1 (Dict.singleton "x" TH.taNumber) >>  re:
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
                (TA.TypeRecordExt TA.ForceImm 1 (Dict.singleton "first" (tyvarImm 2)) >>  re:
                    { freeTyvars = Dict.empty
                    , type = TH.taFunction [re] (tyvarImm 2)
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
                { freeTyvars = Dict.empty
                , type = TH.taFunction [tyvarImm 1] (tyvarImm 1)
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
                { freeTyvars = Dict.empty
                , type =
                    TH.taFunction
                        [TH.taList (tyvarImm 1)]
                        (tyvarImm 1)
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
                { freeTyvars = Dict.empty
                , type =
                    TH.taFunction
                        [ TA.TypeRecord TA.ForceImm (Dict.fromList [ ( "first" & tyvarImm 1 ) ])]
                        (tyvarImm 1)
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
                , type = TH.taFunction [TH.taBool] TH.taNumberAllowUni
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
                , type = TH.taFunction [TH.taBool] TH.taNumberAllowUni
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
            (Test.errorContains [ "Bool"])

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
            """
            Basic functionality
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
        ]

