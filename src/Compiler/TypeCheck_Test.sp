tests as Test =
    Test.'group
        """
        TypeCheck
        """
        [
        , functions
        , lambdaSets
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

Out =
    {
    , freeTyvars as Dict TA.TyvarId TA.Tyvar
    # TODO this needs to contain also an Env, otherwise I can't print it properly
    , type as TA.RawType
    }


codeTest as fn Text, Text, fn Text: Result Text Out, Test.CodeExpectation Out: Test =
    Test.codeTest (outToHuman "tyvars" (fn out: Dict.toList out.freeTyvars)) __ __ __ __


outToHuman =
    #as fn Out: Text =
    fn extraName, extraGetter:
    fn out:
    env =
        Compiler/TypeCheck.initEnv TH.imports Dict.empty

    type =
        out.type
        >> Human/Type.doRawType CoreDefs.coreModule __
        >> Human/Format.formatExpression { isRoot = 'true, originalContent = "" } __
        >> Fmt.render

    [
    , "  " .. extraName .. " = " .. Debug.toHuman (extraGetter out)
    #, "  type = " .. Debug.toHuman out.type
    , "  type = "
    .. type
    ]
    >> Text.join "\n" __



lset1 as TA.LambdaSet = TH.emptyLset


tyvar as fn Int: TA.RawType =
    TH.taTyvar


freeTyvars as fn [ TA.TyvarId ]: Dict TA.TyvarId TA.Tyvar =
    fn ids:
    List.for Dict.empty ids fn id, d:
        Dict.insert id { maybeAnnotated = 'nothing } d


freeTyvarsAnnotated as fn [ TA.TyvarId & Name ]: Dict TA.TyvarId TA.Tyvar =
    fn ids:
    Dict.empty
    >> List.for __ ids fn id & name, d:
        Dict.insert id { maybeAnnotated = 'just { allowFunctions = 'true, name } } d


#
#
#
add as CA.ValueDef =
    {
    , directDeps = Dict.empty
    , maybeAnnotation =
        'just
            {
            , raw = TH.caFunction [ TH.caNumber, TH.caNumber ] TH.caNumber
            , tyvars = Dict.empty
            , univars = Dict.empty
            }
    , maybeBody = 'nothing
    , name = "add"
    , namePos = Pos.'t
    }


reset as CA.ValueDef =
    {
    , directDeps = Dict.empty
    , maybeAnnotation =
        'just
            {
            , raw = TH.caFunction [ TH.caNumber ] TH.caNone
            , tyvars = Dict.empty
            , univars = Dict.empty
            }
    , maybeBody = 'nothing
    , name = "reset"
    , namePos = Pos.'t
    }


inferRootDef as fn Text, Text: Result Text EA.GlobalDefinition =
    fn targetName, code:
    params as Compiler/MakeCanonical.ReadOnly =
        {
        , errorModule = TH.errorModule code
        , imports = TH.imports
        , resolveToUsr = TH.resolveToUsr
        , umr = TH.moduleUmr
        }

    params
    >> Compiler/MakeCanonical.textToCanonicalModule 'true __
    >> TH.resErrorToStrippedText
    >> onOk fn caModuleRaw:
    caModule as CA.Module =
        { caModuleRaw with
        , valueDefs =
            .valueDefs
            >> Dict.insert "add" add __
            >> Dict.insert "reset" reset __
        }

    keysToUsrs =
        __
        >> Dict.keys
        >> List.map ('USR TH.moduleUmr __) __

    requiredUsrs as [ USR ] =
        [
        , keysToUsrs caModule.valueDefs
        , keysToUsrs caModule.constructorDefs
        , keysToUsrs caModule.variantTypeDefs
        , keysToUsrs caModule.aliasDefs
        ]
        >> List.concat

    loadCaModule as fn USR: Res CA.Module =
        fn 'USR umr _:
        if umr == TH.moduleUmr then
            'ok caModule
        else if umr == CoreDefs.umr then
            'ok CoreDefs.coreModule
        else
            [ "no module " .. toHuman umr ]
            >> Error.'raw
            >> 'err

    {
    , loadCaModule
    , projectImports = TH.imports
    , requiredUsrs
    }
    >> Compiler/LazyBuild.build
    >> TH.resErrorToStrippedText
    >> onOk fn { constructors, natives, rootValues }:
    targetUsr =
        EA.translateUsr ('USR TH.moduleUmr targetName) 0

    try List.find (fn rv: rv.usr == targetUsr) rootValues as
        'nothing: 'err "find fail"
        'just def: 'ok def


infer as fn Text: fn Text: Result Text Out =
    fn targetName:
    fn code:
    inferRootDef targetName code
    >> onOk fn def:
    !hash =
        Hash.fromList []

    ft as Dict TA.TyvarId TA.Tyvar =
        Dict.for Dict.empty def.freeTyvars (fn id, tc, d: Dict.insert (TA.normalizeTyvarId @hash id) tc d)

    'ok
        {
        , freeTyvars = ft
        , type =
            def.returnType.raw
            >> TA.normalizeType @hash __
            >> TA.stripTypePos
        }


OutWithLSC =
    {
    # TODO this needs to contain also an Env, otherwise I can't print it properly
    , type as TA.RawType
    }


codeTestWithLambdaSetConstraints as fn Text, Text, Text, Test.CodeExpectation OutWithLSC: Test =
    inferLambdaSets as fn Text: fn Text: Result Text OutWithLSC =
        fn targetName:
        fn code:
        inferRootDef targetName code
        >> onOk fn def:
        !hash =
            Hash.fromList []

        ft as Dict TA.TyvarId TA.Tyvar =
            Dict.for Dict.empty def.freeTyvars (fn id, tc, d: Dict.insert (TA.normalizeTyvarId @hash id) tc d)

        'ok
            {
#            , lambdaSetConstraints =
#                def.lambdaSetConstraints
#                >> Dict.map (fn k, v: Dict.keys v) __
#                >> Dict.toList
            , type =
                def.returnType.raw
                >> TA.normalizeType @hash __
                >> TA.stripTypePos
            }

    fn name, code, target, test:
    Test.codeTest (outToHuman "lSets" (fn out: out.type)) name code (inferLambdaSets target) test


#
# Functions
#

functions as Test =
    Test.'group
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
        , codeTest "Known function with wrong *number* of args" "a = add 'false" (infer "a") (Test.errorContains [ "Number", "Arguments" ])
        , codeTest "Known function with wrong params" "a = add 'false 1" (infer "a") (Test.errorContains [ "Bool" ])
        , codeTest
            "Function inference 1"
            "a = fn x: add x 1"
            (infer "a")
            (Test.isOkAndEqualTo
                 {
                 , freeTyvars = Dict.empty
                 , type = TH.taFunction lset1 [ TH.taNumber ] TH.taNumber
                 }
            )
        , codeTest
            "Function inference 2: same as 1, but with swapped args"
            "a = fn x: add 1 x"
            (infer "a")
            (Test.isOkAndEqualTo
                 {
                 , freeTyvars = Dict.empty
                 , type = TH.taFunction lset1 [ TH.taNumber ] TH.taNumber
                 }
            )
        , codeTest
            "[reg] fn had type None"
            "a = fn x: 1"
            (infer "a")
            (Test.isOkAndEqualTo
                 {
                 , freeTyvars = freeTyvars [ 1 ]
                 , type = TA.'typeFn Pos.'t TH.emptyLset [ tyvar 1 >> toImm >> TA.'parSp ] (toUni TH.taNumber)
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
            (Test.errorContains [ "I need the annotation and the value to have the same type" ])
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
        , codeTest
            """
            [reg] Should unify bound tyvars when needed
            """
            """
            for as fn a, b, (fn b, a: a): a =
                fn a, b, f: a

            z =
                for "" 1 fn b, a:
                    b
            """
            (infer "z")
            (Test.errorContains [ "Text" ])
        ]


lambdaSets as Test =
    Test.'group
        "lambdaSets"
        [
        , codeTestWithLambdaSetConstraints
            """
            Adds a lambda constraint to its argument (no annotation)
            """
            """
            a =
                fn bool, fun:
                    if bool then fun
                    else fn x: x
            """
            "a"
            (Test.isOkAndEqualTo
                 {
#                 , lambdaSetConstraints =
#                     [
#                     , 1 & [ TH.moduleUsr "a" & 1 ]
#                     , 2 & [ TH.moduleUsr "a" & 2 ]
#                     ]
                 , type = TH.taFunction
                        TH.emptyLset
                        [ TH.taBool, TH.taFunction lset1 [ tyvar 1 ] (tyvar 1) ]
                        (TH.taFunction lset1 [ tyvar 1 ] (tyvar 1))
                 }
            )
        , codeTestWithLambdaSetConstraints
            """
            Calls expand lambda constraints
            """
            """
            a =
                fn bool, fun1, fun2, meta:

                if bool then
                    meta fun1
                else if 'false then
                    meta fun2
                else if 'true then
                    meta fn x: x
                else
                    meta fn x: x

            p = fn x: x
            q = fn x: x
            m = fn f: f

            b =
                a 'false p q m

            c =
                a 'false q q m

            z =
                { b, c }
            """
            "z"
            [#
                a as fn(Y) Bool, (fn(X) a: a), (fn(X) a: a): (fn(X) a: a)
                X contains a.2, a.3

                b as fn(Z) a: a
                Z contains a.2, a.3, p.1, q.1

                c as fn(W) a: a
                W contains a.2, a.3, q.1
            #]
            (Test.isOkAndEqualTo
                 {




#                 , lambdaSetConstraints =
#                     [
#                     , 3
#                     & [
#                     , TH.moduleUsr "a" & 1
#                     , TH.moduleUsr "a" & 2
#                     , TH.moduleUsr "p" & 1
#                     , TH.moduleUsr "q" & 1
#                     ]
#                     , 4
#                     & [
#                     , TH.moduleUsr "a" & 1
#                     , TH.moduleUsr "a" & 2
#                     , TH.moduleUsr "q" & 1
#                     ]
#                     ]
                 , type =
                     [
                     , "b" & TH.taFunction TH.emptyLset [ tyvar 2 ] (tyvar 2)
                     , "c" & TH.taFunction TH.emptyLset [ tyvar 1 ] (tyvar 1)
                     ]
                     >> Dict.fromList
                     >> TA.'typeRecord Pos.'t 'nothing __
                 }
            )
        ]


#
# Statements
#

statements as Test =
    Test.'group
        "statements"
        [
        , codeTest
            """
            Statement blocks should return the last statement's type
            """
            """
            a =
              3
              'false
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
    Test.'group
        "Recursive types"
        [
        , codeTest
            """
            Normal types cannot be self recursive
            """
            """
            A = { a as A }
            a as A = this_is_sp_native
            """
            (infer "a")
            (Test.errorContains [ "Circular" ])
        , codeTest
            """
            Normal types cannot be mutually recursive
            """
            """
            A = { b as B }
            B = { a as A }
            a as A = this_is_sp_native
            """
            (infer "a")
            (Test.errorContains [ "Circular" ])
        , codeTest
            """
            Variant types can be recursive
            """
            """
            var A = 'a1, 'a2 B
            B = { a as A }
            a as A = 'a2 b
            b as B = { a = 'a1 }
            """
            (infer "a")
            Test.isOk
        ]


#
# Variable types
#

variableTypes as Test =
    Test.'group
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
                 , type = TH.taFunction lset1 [ tyvar 1 ] (tyvar 1)
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
                 , type = TH.taFunction lset1 [ tyvar 1 ] (tyvar 1)
                 }
            )
        , codeTest
            """
            Annotated vars are instantiated when referenced
            """
            """
            q as [item] =
              Core.'nil

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
            dict_member as fn k, Dict_ k v: Bool = this_is_sp_native
            dict_filter as fn (fn k, v: Bool), Dict_ k v: Dict_ k v = this_is_sp_native

            freeTyvars as Dict_ Number {} = this_is_sp_native
            typeTyvars as Dict_ Number None = this_is_sp_native

            x = dict_filter (fn k, v: dict_member v typeTyvars) freeTyvars
            """
            (infer "x")
            (Test.errorContains [ "{}", "Number" ])
        ]


#
# Higher order types
#

higherOrderTypes as Test =
    Test.'group
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
                 , type = TH.taFunction lset1 [ TA.'typeExact Pos.'t (TH.moduleUsr "T") [ tyvar 1 ] ] (TA.'typeExact Pos.'t (TH.moduleUsr "T") [ tyvar 1 ])
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
                 , type = TA.'typeExact Pos.'t (TH.moduleUsr "X") [ tyvar 1 ]
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
    Test.'group
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
                         lset1
                         [
                         , TA.'typeRecord Pos.'t ('just 1) (Dict.ofOne "meh" (TA.'typeRecord Pos.'t ('just 2) (Dict.ofOne "blah" (tyvar 3))))
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
                     TA.'typeFn
                         Pos.'t
                         TH.emptyLset
                         [
                         , TA.'parRe << TA.'typeRecord Pos.'t ('just 1) (Dict.ofOne "meh" (TA.'typeRecord Pos.'t ('just 2) (Dict.ofOne "blah" TH.taNumber)))
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
                !a = 3 & 'false & 2

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
                 (TA.'typeRecord Pos.'t ('just 1) (Dict.ofOne "x" TH.taNumber)
                  >> (fn re:
                       {
                       , freeTyvars = freeTyvars [ 1 ]
                       , type = TH.taFunction lset1 [ re ] re
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
                 (TA.'typeRecord Pos.'t ('just 1) (Dict.ofOne "x" TH.taNumber)
                  >> (fn re:
                       {
                       , freeTyvars = Dict.empty
                       , type = TH.taFunction lset1 [ re ] re
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
                 (TA.'typeRecord Pos.'t ('just 1) (Dict.ofOne "first" (tyvar 2))
                  >> (fn re:
                       {
                       , freeTyvars = freeTyvars [ 1, 2 ]
                       , type = TH.taFunction lset1 [ re ] (tyvar 2)
                       }
                  )
                 )
            )
        , codeTest
            """
            [reg] refineType when the record has a non-extensible alias
            """
            """
            A = { c as Number, d as Number }

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
            B = { l as [Text] }

            readOne as fn B: (Text & B) =
                fn b:
                try b.l as
                     []: "" & b
                     [h, t...]: h & { b with l = t }
            """
            (infer "readOne")
            Test.isOk
        , codeTest
            """
            [reg] unifyToNonExtensibleRecord correctly substitutes the record extension
            """
            """
            R = { x as Number, y as Number }

            rec as fn R: R =
                fn s:

                if 'true then
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
            R = { x as Number, y as Number }

            r as R = {
              , x = 3
              }
            """
            (infer "r")
            (Test.errorContains [ "Missing" ])
        , codeTest
            """
            [reg] Inferring and then generalizing an extensible record should still constrain the tyvar to a record!
            """
            """
            f =
                fn record:
                record.attr

            main =
                f 'true
            """
            (infer "f")
            (Test.errorContains [ "Bool", "attr" ])
        , codeTest
            """
            Inferred records are correctly merged
            """
            """
            f =
                fn record:
                record.attr

            g =
                fn record:
                record.blah

            meh as fn a, (fn a: Bool), (fn a: Number): Bool =
                fn a, ff, gg:
                'false

            main =
                fn r:
                meh r f g
            """
            (infer "main")
            (Test.isOkAndEqualTo
                 {
                 , freeTyvars = freeTyvars [ 1 ]
                 , type =
                     TH.taFunction
                         lset1
                         [
                         , TA.'typeRecord
                             Pos.'t
                             ('just 1)
                             (Dict.fromList
                                  [
                                  , "attr" & TH.taBool
                                  , "blah" & TH.taNumber
                                  ]
                             )
                         ]
                         TH.taBool
                 }
            )
        ]


#
# Patterns
#

patterns as Test =
    Test.'group
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
                 , type = TH.taFunction lset1 [ tyvar 1 ] (tyvar 1)
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
                 , type = TH.taFunction lset1 [ TH.taList (tyvar 1) ] (tyvar 1)
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
                 , type = TH.taFunction lset1 [ TA.'typeRecord Pos.'t 'nothing (Dict.fromList [ "first" & tyvar 1 ]) ] (tyvar 1)
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
            (Test.isOkAndEqualTo
                 {
                 , freeTyvars = freeTyvars [ 1, 2 ]
                 , type = TH.taFunction lset1 [ TA.'typeRecord Pos.'t ('just 2) (Dict.fromList [ "first" & tyvar 1 ]) ] (tyvar 1)
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
                     Core.'nil: 'none

            result =
                1 :: Core.'nil = Core.'nil
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
    Test.'group
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
                     'true: 2
                     _: 3
            """
            (infer "x")
            (Test.isOkAndEqualTo
                 {
                 , freeTyvars = Dict.empty
                 , type = TA.'typeFn Pos.'t TH.emptyLset [ TH.taBool >> toImm >> TA.'parSp ] (toUni TH.taNumber)
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
                     'true: 2
                     []: 3
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
                     'true: 2
                     'false: 'false
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
                     "": y
            """
            (infer "x")
            (Test.errorContains [ "y" ])
        ]


#
# if..else
#

if_else as Test =
    Test.'group
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
                 , type = TA.'typeFn Pos.'t TH.emptyLset [ TH.taBool >> toImm >> TA.'parSp ] (toUni TH.taNumber)
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
                else 'false
            """
            (infer "x")
            (Test.errorContains [ "Number" ])
        ]


#
# NonFunction
#

nonFunction as Test =
    Test.'group
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
                 , freeTyvars = Dict.ofOne 1 { maybeAnnotated = 'nothing }
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
                this_is_sp_native
            """
            (infer "sort")
            (Test.isOk)
#]

#
# Misc
#

misc as Test =
    Test.'group
        "Misc"
        [
        , codeTest
            """
            [reg] Undefined types should be rejected
            """
            """
            v as ThisTypeIsNotDefined = this_is_sp_native
            """
            (infer "v")
            (Test.errorContains [ "ThisTypeIsNotDefined" ])
        , codeTest
            """
            Placeholder works with unique args
            """
            """
            stuff as fn !Number: Number = this_is_sp_native
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
                   []: q

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
                     ["emphasys", s]: s

            fmtBlock as Text =
                try ddget 1 'Blah as
                     'Blah:
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
