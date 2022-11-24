

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


outToHuman as Compiler/TypeCheck.Instance: Text =
    out:

    [
    , "  tyvars = " .. Debug.toHuman (Dict.toList out.freeTyvars)
    , "  type = " .. Debug.toHuman out.type
    ]
        >> Text.join "\n"



tyNumber as TA.Type =
    TA.TypeOpaque Pos.T ("Number" >> Meta.spCoreUSR) []


tyNone as TA.Type =
    TA.TypeOpaque Pos.T ("None" >> Meta.spCoreUSR) []


ftv as Text: Dict Text TA.TypeClasses =
    n:
    Dict.singleton n { allowFunctions = Just True, allowUniques = Just False }


forall as List Text: Dict Text TA.TypeClasses =
    vars:
    List.for vars (n: Dict.insert n { allowFunctions = Just True, allowUniques = Just False }) Dict.empty



#TODO merge these two

function as [TA.Type]: TA.Type: TA.Type =
    from: to:
    TA.TypeFn Pos.T (List.map (t: False & t) from) to


#typeVariable as Name: TA.Type =
#    name:
#    TA.TypeAnnotationVariable Pos.T name


#
#
#
infer as Text: Text: Result Text Compiler/TypeCheck.Instance =
    name: code:

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

    typeCheckState & typeCheckGlobalEnv_ =
        Compiler/TypeCheck.initStateAndGlobalEnv modules

    typeCheckGlobalEnv as Compiler/TypeCheck.Env =
        { typeCheckGlobalEnv_ with
        , variables = .variables
            >> Dict.insert
                (CA.RefGlobal << USR TH.moduleUmr "add")
                {
                , definedAt = Pos.T
                , type = function [tyNumber, tyNumber] tyNumber
                , freeTyvars = Dict.empty
                }
            >> Dict.insert
                (CA.RefGlobal << USR TH.moduleUmr "reset")
                {
                , definedAt = Pos.T
                , type = function [tyNumber] tyNone
                , freeTyvars = Dict.empty
                }
        }

    caModule
    >> Compiler/TypeCheck.doModule typeCheckState typeCheckGlobalEnv
    >> TH.resErrorToStrippedText code
    >> onOk taModule:

    toMatch as (CA.Pattern & TA.ValueDef): Maybe { isUnique as Bool, maybeAnnotation as Maybe CA.Type, def as TA.ValueDef } =
        (pattern & def):
        try pattern as
            CA.PatternAny Pos.T { isUnique, maybeAnnotation, maybeName = Just name }:
                Just { isUnique, maybeAnnotation, def }
            _:
                Nothing

    matches =
        taModule.valueDefs
        >> Dict.toList
        >> List.filterMap toMatch

    try matches as
        []:
            Err "dict fail"

        { isUnique, maybeAnnotation, def } :: tail:
            try def.pattern as

#                ty & tyvars =
#                    HCA.normalizeTypeAndTyvars var.ty var.freeTypeVariables

                TA.PatternAny Pos.T { isUnique, maybeAnnotation, maybeName, type }:
                    {
                    , definedAt = Pos.T
                    , type = Compiler/TypeCheck.applyAllSubstitutions taModule.substitutions type
                    , freeTyvars = Dict.empty #todo "" #def.tyvars
                    }
                    >> Ok

                _:
                    Err "pattern fail"



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
                , definedAt = Pos.T
                , type = tyNumber
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
                , definedAt = Pos.T
                , type = function [tyNumber] tyNumber
                , freeTyvars = Dict.empty
                }
            )
        , codeTest
            "Function inference 2: same as 1, but with swapped args"
            "a = fn x: add 1 x"
            (infer "a")
            (Test.isOkAndEqualTo
                {
                , definedAt = Pos.T
                , type = function [tyNumber] tyNumber
                , freeTyvars = Dict.empty
                }
            )
#        , codeTest
#            "[reg] fn has type None"
#            "a = x: 1"
#            (infer "a")
#            (Test.isOkAndEqualTo
#                { tyvars = ftv "1"
#                , type = function [typeVariable "a"] tyNumber
#                }
#            )
#
#        #
        , codeTest "[reg] Multiple arguments are correctly inferred"
            """
            a = fn x, y, z: x + y + z
            """
            (infer "a")
            Test.isOk
#
#        #
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
        []
        [#
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
            [reg] Definition statement with annotation return type None
            """
            """
            a as None =
              f = 3
            """
            (infer "a")
            Test.isOk
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
        #]



#
# Variable types
#


variableTypes as Test =
    Test.Group "Variable types"
        []
        [#
        [ codeTest
            "Identity"
            """
            id as a: a =
              a: a
            """
            (infer "id")
            (Test.isOkAndEqualTo
                { ty = typeFunction (typeVariable "0a") (typeVariable "0a")
                , freeTypeVariables = ftv "0a"
                , isMutable = False
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
        #]


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
                { ty = typeFunction tyNumber LambdaConsuming tyNone
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
        []
        [#
        , codeTest
            "ONLY Parse precedence"
            """
            union T a = T a

            a as T a: T a =
                l: l
            """
            (infer "a")
            (Test.isOkAndEqualTo
                { ty =
                    typeFunction
                        (CA.TypeConstant Pos.T (TH.localType "T") [ typeVariable "0a" ])
                        (CA.TypeConstant Pos.T (TH.localType "T") [ typeVariable "0a" ])
                , isMutable = False
                , freeTypeVariables = ftv "0a"
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
                , isMutable = False
                , freeTypeVariables = Dict.singleton "1" { allowFunctions = True, allowUniques = True }
                , ty =
                    CA.TypeConstant Pos.T
                        (TH.localType "X")
                        [ CA.TypeVariable (Pos.I 11) "a" { allowFunctions = True, allowUniques = True }]
                }
            )
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
        , codeTest
            """
            SKIP [reg] Named vars can't be refined?
            """
            """
            union Wrap a = W a

            f as a: Wrap a =
                a: a
            """
            (infer "f")
            (Test.errorContains [ "Wrap"])
        #]



#
# Records
#


records as Test =
    Test.Group "Records"
        []
        [#
        , codeTest "Attribute access"
            """
            a = b: b.meh.blah
            """
            (infer "a")
            (Test.isOkAndEqualTo
                { freeTypeVariables = forall [ "2", "4", "5" ]
                , isMutable = False
                , ty =
                    typeFunction
                        (CA.TypeRecordExt (Pos.I 2)
                            "a"
                            { allowFunctions = True, allowUniques = False }
                            (Dict.singleton "meh" (CA.TypeRecordExt (Pos.I 2) "b" { allowFunctions = True, allowUniques = False } (Dict.singleton "blah" (typeVariable "c"))))
                        )
                        (typeVariable "c")
                }
            )
        , codeTest
            """
            Attribute mutation
            """
            """
            a =
                @b:-
                @b.meh.blah += 1
            """
            (infer "a")
            (Test.isOkAndEqualTo {
                , freeTypeVariables =
                    [ "2", "4" ]
                    >> List.map (n: n & { allowFunctions = True, allowUniques = True })
                    >> Dict.fromList
                , isMutable =
                    False
                , ty =
                    typeFunction
                        (CA.TypeMutable Pos.T << CA.TypeRecordExt (Pos.I 2)
                            "a"
                            { allowFunctions = True, allowUniques = True }
                            (Dict.singleton "meh"
                                (CA.TypeMutable Pos.T << CA.TypeRecordExt (Pos.I 2)
                                    "b"
                                    { allowFunctions = True, allowUniques = True }
                                    (Dict.singleton
                                        "blah"
                                        (CA.TypeMutable Pos.N CoreTypes.number)
                                    )
                                )
                            )
                        )
                        LambdaConsuming
                        CoreTypes.none
                }
            )
        , codeTest "Tuple3 direct item mutability"
            """
            x =
                @a = mut << 3 & False & 2

                @a.third += 1
            """
            (infer "x")
            Test.isOk
        , codeTest "Tuple2 direct item mutability, annotated"
            """
            x = y:
               @a as @(Number & Number) =
                 mut << 1 & 2

               @a.first += 1
            """
            (infer "x")
            Test.isOk
        , codeTest
            "functional update"
            "a = b: { b with x = 1 }"
            (infer "a")
            (Test.isOkAndEqualTo
                (CA.TypeRecordExt Pos.T "a" { allowFunctions = True, allowUniques = False } (Dict.singleton "x" CoreTypes.number) >> re:
                    { freeTypeVariables = forall [ "2" ]
                    , isMutable = False
                    , ty = typeFunction re re
                    }
                )
            )
        , codeTest "SKIP instantiate and refine inferred records"
            """
            a = t: { t with x = 1 }
            c = a
            """
            (infer "c")
            (Test.isOkAndEqualTo
                (CA.TypeRecordExt Pos.T "a" { allowFunctions = True, allowUniques = False } (Dict.singleton "x" CoreTypes.number) >> re:
                    {
                    , freeTypeVariables = forall [ "a" ]
                    , isMutable = False
                    , ty = typeFunction re re
                    }
                )
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
                        (CA.TypeRecordExt
                            (Pos.I 2)
                            "a"
                            { allowFunctions = True, allowUniques = False }
                            (Dict.fromList [ "first" & typeVariable "b" ])
                        )
                        (typeVariable "b")
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
        #]



#
# Patterns
#


patterns as Test =
    Test.Group "Patterns"
        []
        [#
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
                        (CoreTypes.list ( CA.TypeVariable (Pos.I 11) "a" { allowFunctions = True, allowUniques = False }))
                        (CA.TypeVariable (Pos.I 11) "a" {allowFunctions = True, allowUniques = False })
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
                { freeTypeVariables = forall [ "2" ]
                , isMutable = False
                , ty =
                    typeFunction
                        (CA.TypeRecord Pos.T (Dict.fromList [ ( "first" & typeVariable "a" ) ]))
                        (typeVariable "a")
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
                    Core.Nil:
                        None

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
        #]



#
# Try..As
#


try_as as Test =
    Test.Group "try..as"
        []
        [# codeTest "basic functionality"
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
                , ty = typeFunction CoreTypes.bool CoreTypes.number
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
        , codeTest "[reg] actually infers blocks"
            """
            x as Number =
              try "" as
                "": y
            """
            (infer "x")
            (Test.errorContains [ "y" ])
        #]



#
# if..else
#


if_else as Test = 
    Test.Group "if..else"
        []
        [# codeTest "basic functionality"
            """
            x = q:
              if q then 1
              else 2
            """
            (infer "x")
            (Test.isOkAndEqualTo
                { freeTypeVariables = Dict.empty
                , isMutable = False
                , ty = typeFunction CoreTypes.bool CoreTypes.number
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
        #]



#
# NonFunction
#


nonFunction as Test =
    Test.Group "NonFunction"
        []
        [#
        codeTest
            """
            Basic functionality
            """
            """
            blah as [a]: [a] with a NonFunction =
              a:
              a

            meh =
              blah [x: x]
            """
            (infer "meh")
            (Test.errorContains [ "can't contain functions"])
        #]

