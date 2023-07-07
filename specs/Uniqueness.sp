[#

This is a *specification* of Squarepants Mutability/Uniqueness system.
It is meant to be complete and accurate, it is **NOT** meant as newbie-friendly introduction.

Squarepants implements [uniqueness typing](http://edsko.net/2017/01/08/linearity-in-haskell/) and uses it to manage in-place mutation, track variables lifetime and manage side effects.

Uniqueness typing allows Squarepants to ensure that, at any given time, there is only a single reference to a particular item.

All the crunching is done at compile time, which means that, for unique values, the run-time does not need to keep track of allocation/destruction: no garbage collection, no reference counting needed.

#]


specs as Test =
    Test.Group "Uniqueness"
      [
      , howDoesItLookLike
      , uniquenessTyping
      , mutation
      , parentScope
      , polymorphism
      , unions
      , records
      ]


#
# Boilerplate code, not really needed to understand the system
#

valueTest as fn Text, (fn None: a), Test.CodeExpectation a: Test =
    Test.valueTest toHuman __ __ __

codeTest =
    Test.codeTest Debug.toHuman __ __ __ __

infer as fn Text: fn Text: Result Text Compiler/TypeCheck_Test.Out =
    Compiler/TypeCheck_Test.infer



howDoesItLookLike as Test =
    Test.Group
        #
        #
        #
        """
        How does mutability look like?
        """
        [
        , codeTest
            """
            Example: maintaining mutable state
            """
            """
            average as fn [Number]: Number =
                fn numbers:

                # Unique values can be changed in place, ie, "mutated"
                !total as Number =
                    0

                !count as Number =
                    0

                (todo "List.each") numbers fn number:
                    @total += number
                    @count += 1

                # In Squarepants division by 0 yields 0
                total / count
            """
            (infer "average")
            Test.isOk

        , codeTest
            """
            SKIP (needs IO in the test env) Example: File IO
            """
            """
            logToFile as fn @IO, Text: Result IO.Error None =
                fn @io, content:

                IO.openFile @io IO.Append "blah.log"
                >> isOk fn @fileDescriptor:

                IO.writeFile @io content @fileDescriptor

                # fileDescriptor is automatically closed here
            """
            (infer "logToFile")
            Test.isOk
        ]




uniquenessTyping as Test =
    Test.Group
        """
        Uniqueness Typing
        """
        [
        , Test.Group
            """
            All literal expressions allow uniqueness
            """
            [
            , codeTest "failure" "f as fn Number: !Number = fn x: x" (infer "f") (Test.errorContains [ "ErrorUniquenessDoesNotMatch" ])
            , codeTest "Number"  "f as fn a: !Number = fn _: 1" (infer "f") Test.isOk
            , codeTest "Text"    """f as fn a: !Text = fn _: "meh" """ (infer "f") Test.isOk
            , codeTest "Record"  "f as fn a: !{} = fn _: {}" (infer "f") Test.isOk
            , codeTest "Constructor 1"  "f as fn a: !Bool = fn _: True" (infer "f") Test.isOk
            # TODO: Constructor with pars
            ]

        #
        , Test.Group
            """
            Conversions
            """
            [
            , codeTest
                """
                Immutables cannot be used in place of uniques 2
                """
                """
                scope =
                    x = 1
                    @x += 1
                """
                (infer "a")
                (Test.errorContains ["ErrorShouldBeUnique"])
            , codeTest
                """
                Uniques can be implicitly transformed in immutables
                """
                """
                a as Number = 1
                """
                (infer "a")
                Test.isOk
            ]
        , Test.Group
            """
            A variable with mutable type must be explicitly declared as mutable with `!`
            """
            [
            , codeTest "1"
                """
                z =
                    !a as Number = 1
                """
                (infer "z")
                Test.isOk
            ]
        , Test.Group
            """
            Referencing a mutable variable "spends" it
            """
            [
            , codeTest "base"
                """
                scope =
                    !x =
                        1

                    !y =
                        # The first time we do it it works!
                        x

                    !z =
                        # But here `x` is now spent, so we get a compiler error!
                        x
                """
                (infer "scope")
                (Test.errorContains ["used already here"])

            , codeTest "tuple"
                """
                scope =
                    !x =
                        1

                    !y =
                        x & x
                """
                (infer "scope")
                (Test.errorContains ["used already here"])
            ]
        , Test.Group
            """
            A function cannot consume uniques outside its own scope.
            """
            [
            , codeTest
                """
                base
                """
                """
                scope =
                    !x = 1
                    fn z: x
                """
              (infer "scope")
              (Test.errorContains [ "outside their body", "x" ])
            ]
        ]


mutation as Test =
    Test.Group "Mutation"
        [
        , Test.Group
            """
            Uniques can be mutated in place
            """
            [
            [# TODO enable once the new parser self-compiles
            , valueTest
                """
                Base
                """
                _:
                    !x =
                        1

                    @x += 2

                    x == 3
                (Test.isOkAndEqualTo True)
            #]
            , codeTest
                """
                Mutation does NOT consume the unique
                """
                """
                scope =
                    !x = 1
                    @x += 1
                    @x += 1
                """
                (infer "scope")
                Test.isOk
            , codeTest
                """
                Recycling requires the unique not to be spent
                """
                """
                scope =
                    !x = 1
                    (todo "consume") x
                    @x += 1
                """
                (infer "scope")
                (Test.errorContains ["used again here"])
            ]
        , Test.Group
            """
            A function can be defined to mutate its arguments
            """
            [
            , codeTest
                """
                base
                """
                """
                funz as fn @Number: None =
                    fn @a:
                    @a += 3

                scope =
                    !x = 0
                    funz @x
                    funz @x
                """
                (infer "scope")
                Test.isOk
            ]
        , Test.Group
            """
            Calling a function that recycles a unique variable temporarily consumes the variable.
            """
            [
            , codeTest
                """
                base
                """
                """
                scope =
                    !x = 0
                    (todo "funz") @x @x
                """
                (infer "scope")
                (Test.errorContains [ "twice" ])
            ]
        ]


parentScope as Test =
    Test.Group
        """
        Recycling a variable in the parent scope
        """
        [
        , Test.Group
            """
            A function that recycles any unique belonging to an ancestor scope "requires" that unique.
            """
            [
            , codeTest
                """
                LetIns cannot return functions with requirements
                """
                """
                scope =
                    !x =
                        1

                    f =
                        fn n:
                        @x += n
                        None

                    f
                """
                (infer "scope")
                (Test.errorContains ["x", "from outside"])
            , codeTest
                """
                Functions cannot return functions with UNIQUE requirements
                """
                """
                f =
                    fn !x:
                    fn n:
                    @x += n
                    None
                """
                (infer "f")
                (Test.errorContains ["x", "from outside"])
            , codeTest
                """
                Functions cannot return functions with RECYCLED requirements
                """
                """
                f =
                    fn @x:
                    fn n:
                    @x += n
                    None
                """
                (infer "f")
                (Test.errorContains ["x", "from outside"])
            , codeTest
                """
                The Array Test
                """
                """
                union Array a = Meh

                array_push as fn a, @Array a: None = todo ""

                addFunctions as fn @Array (fn Int: Int): None =
                    fn @functions:

                    !x =
                        1

                    f as fn Int: Int =
                        fn n:
                        @x += 1
                        n

                    array_push f @functions
                    None
                """
                (infer "addFunctions")
                (Test.errorContains ["x", "outside"])
            ]
        ]



records as Test =
    Test.Group
        """
        Records
        """
        [
        , Test.Group
            """
            The attribute of a mutable record can be accessed as a mutable:
            """
            [
            , codeTest
                """
                Simple case
                """
                """
                scope =
                    !record = { x = 0, y = 0 }
                    @record.x += 3
                """
                (infer "scope")
                Test.isOk
            , codeTest
                """
                Reject double reference
                """
                """
                scope =
                    !record = { x = 0, y = 0 }
                    (todo "") @record.x @record.y
                """
                (infer "scope")
                (Test.errorContains [ "same unique twice in the same function call" ])
            ]
        ]


polymorphism as Test =
    Test.Group
        """
        Polymorphism
        """
        [
        , codeTest
            """
            Basic syntax
            """
            """
            fun as fn (fn 1?a: 2?b), 1?a: 2?b =
                fn f, 1?a:

                f a
            """
            (infer "fun")
            Test.isOk

        , codeTest
            """
            A function that returns a Uni can be used in place of a function that returns an Imm
            """
            """
            meh as fn (fn None: Number): Number =
                fn f: f None

            blah = meh (fn None: 1)
            """
            (infer "blah")
            Test.isOk

        , codeTest
            """
            A function that returns an Imm CANNOT be used in place of a function that returns an Uni
            """
            """
            meh as fn (fn None: !Number): !Number =
                fn f: f None

            x as Number = 1

            blah = meh (fn None: x)
            """
            (infer "blah")
            (Test.errorContains [ "return", "uniqueness" ])

        # isOk
        , codeTest
            """
            a Uni, b Uni
            """
            """
            union Re error payload = Er error, Okk payload
            isOkk as fn (fn 1?a: 2?Re error b), 1?Re error a: 2?Re error b = todo ""

            scope =
                !v = isOkk (fn !a: Okk 0) (Okk 0)
            """
            (infer "scope")
            Test.isOk

        , codeTest
            """
            a Uni, b Imm
            """
            """
            union Result_ error payload = Err_ error, Ok_ payload
            isOk_ as fn (fn 1?a: 2?Result_ error b), 1?Result_ error a: 2?Result_ error b = todo ""
            immB as Number = 1

            v = isOk_ (fn !a: Ok_ immB) (Ok_ 0)
            """
            (infer "v")
            Test.isOk
        , codeTest
            """
            No annotation
            """
            """
            na = fn 0?x: x
            """
            (infer "na")
            (Test.isOkAndEqualTo
                {
                , freeTyvars = Dict.empty
                , type = TA.TypeFn [TA.ParSp { uni = Depends 0, raw = TA.TypeVar 1}] { uni = Depends 0, raw = TA.TypeVar 1}
                }
            )
        , codeTest
            """
            Generalization
            """
            """
            na as fn 1?a: 1?a =
                fn 1?x: x

            scope as None =
                !uni = na 0

            none as None =
                na scope
            """
            (infer "na")
            Test.isOk
        , codeTest
            """
            [rec] variable without any uniqueness flag should be imm
            """
            """
            scope =
                num = 1

                x as Number = num + 1
                y as Number = num + 2
            """
            (infer "scope")
            Test.isOk
        ]


unions as Test =
    Test.Group
        """
        Unions
        """
        [
        , codeTest
            """
            Uniques inside immutables are converted to immutables
            """
            """
            x = Z 0
            """
            (infer "x")
            (Test.isOkAndEqualTo
                {
                , freeTyvars = Dict.empty
                , type = TA.TypeUnion (Just 1) (Dict.ofOne "Z" [ TH.taNumber ])
                }
            )

        , codeTest
            """
            [reg] Lists of immutables
            """
            """
            i as Number = 1
            x = [ i, i ]
            """
            (infer "x")
            Test.isOk

        , codeTest
            """
            [reg] solveOneEquality can receive switched given/required when evaluating a cast?
            """
            """
            z as [fn None: None] = [fn None: None, ...[]]
            """
            (infer "z")
            Test.isOk

        # LetIn
        , codeTest
            """
            LetIn: Unpack immutable to immutable
            """
            """
            scope =
                x = Z 0
                (Z y) = x
            """
            (infer "scope")
            Test.isOk
        , codeTest
            """
            LetIn: Unpack unique to immutable
            """
            """
            scope =
                !x = Z 0
                (Z y) = x
            """
            (infer "scope")
            Test.isOk
        , codeTest
            """
            LetIn: Unpack unique to unique
            """
            """
            scope =
                !x = Z 0
                #!(Z y) = x
                #@y += 1
            """
            (infer "scope")
            Test.isOk
        , codeTest
            """
            LetIn: Unpack immutable to unique
            """
            """
            scope =
                x = Z 0
                !(Z y) = x
            """
            (infer "scope")
            (Test.errorContains [ "y", "Unique" ])
        # Fn
        , codeTest
            """
            Fn: Unpack immutable to immutable
            """
            """
            union Z a = Z a
            f as fn Z a: Z a =
                 fn Z a: Z a
            """
            (infer "f")
            Test.isOk
        , codeTest
            """
            Fn: Unpack unique to immutable
            """
            """
            union Z a = Z a
            f as fn !(Z a): Z a =
                 fn !(Z a): Z a
            """
            (infer "f")
            Test.isOk
        , codeTest
            """
            Fn: Unpack unique to unique
            """
            """
            union Z a = Z a
            f as fn !(Z a): !(Z a) =
                 fn !(Z a): Z a
            """
            (infer "f")
            Test.isOk
        , codeTest
            """
            Fn: Unpack immutable to unique
            """
            """
            union Z a = Z a
            f as fn Z a: !(Z a) =
                 fn Z a: Z a
            """
            (infer "f")
            (Test.errorContains [ "Unique" ])
        ]
