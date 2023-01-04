[#

This is a *specification* of Squarepants Mutability/Uniqueness system.
It is meant to be complete and accurate, it is **NOT** meant as newbie-friendly introduction.

Squarepants implements [uniqueness typing](http://edsko.net/2017/01/08/linearity-in-haskell/) and uses it to manage in-place mutation, track variables lifetime and manage side effects.

Uniqueness typing allows Squarepants to ensure that, at any given time, there is only a single reference to a particular item.

All the crunching is done at compile time, which means that, for unique values, the run-time does not need to keep track of allocation/destruction: no garbage collection, no reference counting needed.

#]


specs as Test =
    Test.Group "Mutability spec" [
      , howDoesItLookLike
      , uniquenessTyping
      , mutation
      , parentScope
      , records
      , unions
      ]


#
# Boilerplate code, not really needed to understand the system
#

valueTest as Text: (None: a): Test.CodeExpectation a: Test =
    Test.valueTest toHuman

codeTest =
    Test.codeTest Debug.toHuman

infer as Text: Text: Result Text Compiler/TypeCheck_Test.Out =
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
                !total as !Number =
                    0

                !count as !Number =
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
        , codeTest
            """
            Types can be flagged as unique
            """
            """
            alias A = !Number

            alias B a = !a

            z = 1
            """
            (infer "z")
            Test.isOk
        #
        , Test.Group
            """
            Functions cannot be unique
            """
            [
            , codeTest
                """
                Type annotation
                """
                """
                alias C = !fn Text: Number

                z = 1
                """
                (infer "z")
                (Test.errorContains ["TypeFn", "Uni"])
            , codeTest
                """
                Annotated
                """
                """
                scope =
                    !f as !fn Number: Number =
                        todo ""
                """
                (infer "scope")
                (Test.errorContains ["TypeFn"])
            , codeTest
                """
                Inferred
                """
                """
                scope =
                    !f =
                        fn _: 1
                """
                (infer "scope")
                (Test.errorContains ["is unique", "but its type is"])
            ]
        #
        , Test.Group
            """
            Conversions
            """
            [
            , codeTest
                """
                Immutables cannot be used in place of uniques 1
                """
                """
                a as !Number = 1
                """
                (infer "a")
                (Test.errorContains ["ErrorUniquenessDoesNotMatch"])
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
                (Test.errorContains ["is immutable"])
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
                    !a as !Number = 1
                """
                (infer "z")
                Test.isOk
            , codeTest "2"
                """
                a as !Number = 1
                """
                (infer "a")
                (Test.errorContains ["UniquenessDoesNotMatch"])
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
              (Test.errorContains [ "but they were declared in its parent scope", "x" ])
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
                    @x =
                        1

                    @x += 2

                    @x == 3
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
                Mutation requires the unique not to be spent
                """
                """
                scope =
                    !x = 1
                    (todo "consume") x
                    @x += 1
                """
                (infer "scope")
                (Test.errorContains ["being spent here"])
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
            Calling a function that mutates a unique variable temporarily consumes the variable.
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

                    f
                """
                (infer "scope")
                (Test.errorContains ["x", "required"])
            , codeTest
                """
                Functions cannot return functions with UNIQUE requirements
                """
                """
                f =
                    fn !x:
                    fn n:
                    @x += n
                """
                (infer "f")
                (Test.errorContains ["x", "required"])
            , codeTest
                """
                Functions cannot return functions with RECYCLED requirements
                """
                """
                f =
                    fn @x:
                    fn n:
                    @x += n
                """
                (infer "f")
                (Test.errorContains ["x", "required"])
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
            An immutable attribute of a mutable record can be unpacked to an immutable
            """
            [
            , codeTest
                """
                1
                """
                """
                scope =
                    !r = { x = 0, y = 1 }

                    { x = immutableX, y = !mutableY } =
                        r

                    xx as Number =
                        immutableX

                    !yy as !Number =
                        mutableY
                """
                (infer "scope")
                Test.isOk
            , codeTest
                """
                SKIP (parser doesn't like !y) 2
                """
                """
                scope =
                    !r = { x = 0, y = 1 }

                    { x, !y } =
                        r

                    xx as Number =
                        x

                    !yy as !Number =
                        y
                """
                (infer "scope")
                Test.isOk
            ]
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
                (Test.errorContains [ "same mutable twice in the same function call" ])
            , codeTest
                """
                SKIP (low priority) Unpacking an immutable does not consume the unique
                """
                """
                scope =
                    x = 1
                    !record = { x, y = 0 }

                    a = record.x

                    @record.y += 1
                """
                (infer "scope")
                Test.isOk
            ]
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
            union Z a = Z a
            x = Z 0
            """
            (infer "x")
            (Test.isOkAndEqualTo
                {
                , freeTyvars = Dict.empty
                , type =
                    TA.TypeExact (TA.UniIsFixed TA.ForceImm)
                        (TH.localType "Z")
                        [ TH.taNumber ]
                }
            )
        # LetIn
        , codeTest
            """
            LetIn: Unpack immutable to immutable
            """
            """
            union Z a = Z a
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
            union Z a = Z a
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
            union Z a = Z a
            scope =
                !x = Z 0
                (Z !y) = x
                @y += 1
            """
            (infer "scope")
            Test.isOk
        , codeTest
            """
            LetIn: Unpack immutable to unique
            """
            """
            union Z a = Z a
            scope =
                x = Z 0
                (Z !y) = x
            """
            (infer "scope")
            (Test.errorContains [ "??" ])
        # Fn
        , codeTest
            """
            Fn: Unpack immutable to immutable
            """
            """
            union Z a = Z a
            f as fn (Z a): Z a =
                 fn (Z a): Z a
            """
            (infer "f")
            Test.isOk
        , codeTest
            """
            Fn: Unpack unique to immutable
            """
            """
            union Z a = Z a
            f as fn (Z !a): Z a =
                 fn (Z !a): Z a
            """
            (infer "f")
            Test.isOk
        , codeTest
            """
            Fn: Unpack unique to unique
            """
            """
            union Z a = Z a
            f as fn (Z !a): Z !a =
                 fn (Z !a): Z a
            """
            (infer "f")
            Test.isOk
        , codeTest
            """
            Fn: Unpack immutable to unique
            """
            """
            union Z a = Z a
            f as fn (Z a): Z !a =
                 fn (Z a): Z a
            """
            (infer "f")
            (Test.errorContains [ "??" ])
        ]
