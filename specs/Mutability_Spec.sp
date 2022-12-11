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
#      , parentScope
#      , records
#      , unions
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
            SKIP Example: maintaining mutable state
            """
            """
            average as fn [Number]: Number =
                numbers:

                # The core function `mut` transforms an immutable value in a mutable one
                !total as !Number =
                    mut 0

                !count as !Number =
                    mut 0

                each numbers number:
                    @total += number
                    @count += 1

                # Most algebraic operators accept both immutable and mutable values
                # Also in Squarepants division by 0 yields 0
                @total / @count
            """
            (infer "average")
            Test.isOk

        , codeTest
            """
            SKIP Example: File IO
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
    Test.Group "Uniqueness Typing" [
        , codeTest
            """
            Types can be flagged as mutable
            """
            """
            alias A = !Number

            alias B a = !a

            alias C = !(fn Text: Number)

            z = 1
            """
            (infer "z")
            Test.isOk
        #
        , Test.Group
            """
            Mutable types are not interchangeable with their non-mutable counterpart
            """
            [
            , codeTest "1"
                """
                a as !Number = 1
                """
                (infer "a")
                (Test.errorContains ["ErrorUniquenessDoesNotMatch"])
            , codeTest "2"
                """
                a as Number = mut 1
                """
                (infer "a")
                (Test.errorContains ["incompatible"])
            ]
        , Test.Group
            """
            A variable with mutable type must be explicitly declared as mutable with `!`
            """
            [
            , codeTest "1"
                """
                z =
                    !a as !Number = mut 1
                """
                (infer "z")
                Test.isOk
            , codeTest "2"
                """
                a as !Number = mut 1
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
                        mut 1

                    !y =
                        # The first time we do it it works!
                        x

                    !z =
                        # But here `x` is now consumed, so we get a compiler error!
                        x
                """
                (infer "scope")
                (Test.errorContains ["used already here"])

            , codeTest "tuple"
                """
                scope =
                    !x =
                        mut 1

                    !y =
                        x & x
                """
                (infer "scope")
                (Test.errorContains ["used already here"])
            ]
        , Test.Group
            """
            "SKIP When we construct a function with mutable elements in its closure that function itself must be mutable."
            """
            [
            , codeTest
                """
                base
                """
                """
                scope =
                    !x = mut 1
                    fn z: x
                """
              (infer "scope")
              (Test.errorContains [ "?" ])
            ]
        ]


mutation as Test =
    Test.Group "Mutation" [
        , Test.Group "Uniques can be mutated in place" [
            [# TODO enable once the new parser self-compiles
            , valueTest
                """
                Base
                """
                _:
                    @x =
                        mut 1

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
                    !x = mut 1
                    @x += 1
                    @x += 1
                """
                (infer "scope")
                Test.isOk
            , codeTest
                """
                Mutation requires the unique not to be consumed
                """
                """
                scope =
                    !x = mut 1
                    consume x
                    @x += 1
                """
                (infer "scope")
                (Test.errorContains ["being consumed here"])
            ]
        , Test.Group "A function can be defined to mutate its arguments" [
            , codeTest
                """
                base
                """
                """
                funz as fn @Number: None =
                    fn @a:
                    @a += 3

                scope =
                    !x = mut 0
                    funz @x
                    funz @x
                """
                (infer "scope")
                Test.isOk
            ]
        , Test.Group "Calling a function that mutates a unique variable temporarily consumes the variable." [
            , codeTest
                """
                base
                """
                """
                scope =
                    !x = mut 0
                    funz @x @x
                """
                (infer "scope")
                (Test.errorContains [ "twice" ])
            ]
        ]


parentScope as Test =
    Test.Group
        """
        Mutating a variable in the parent scope
        """
        [
        , Test.Group
            """
            A function that mutates any mutable belonging to an ancestor scope is "tainted" by that mutableGeneral
            """
            [
            , codeTest
                """
                SKIP also wait for removing auto-curry
                """
                """
                """
                (infer "scope")
                (Test.errorContains [])
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
            A record that has at least one mutable attribute is itself mutable
            """
            [
            , codeTest
                """
                Annotation, valid
                """
                """
                scope =
                    @r as { x as Number, y as @Number } =
                        { x = 0, y = mut 0 }
                """
                (infer "scope")
                Test.isOk
            , codeTest
                """
                Annotation, invalid 1
                """
                """
                scope =
                    @r as { x as Number, y as Number } =
                        { x = 0, y = 0 }
                """
                (infer "scope")
                (Test.errorContains [ "UNIQUENESS" ])
            , codeTest
                """
                Annotation, invalid 2
                """
                """
                scope =
                    r as { x as Number, y as @Number } =
                        { x = 0, y = mut 0 }
                """
                (infer "scope")
                (Test.errorContains [ "UNIQUENESS" ])

            , codeTest
                """
                No annotation, valid
                """
                """
                scope =
                    @r =
                        { x = 0, y = mut 0 }
                """
                (infer "scope")
                Test.isOk
            , codeTest
                """
                No annotation, invalid 1
                """
                """
                scope =
                    @r =
                        { x = 0, y = 0 }
                """
                (infer "scope")
                (Test.errorContains [ "not compatible" ])
            , codeTest
                """
                No annotation, invalid 2
                """
                """
                scope =
                    r =
                        { x = 0, y = mut 0 }
                """
                (infer "scope")
                (Test.errorContains [ "UNIQUE" ])
            , codeTest
                """
                Reject global unique
                """
                """
                scope =
                    { x = 0, y = mut 0 }
                """
                (infer "scope")
                (Test.errorContains [ "UNIQUE" ])
            ]
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
                    @r = { x = 0, y = mut 1 }

                    { x = immutableX, y = @mutableY } =
                        r

                    xx as Number =
                        immutableX

                    @yy as @Number =
                        mutableY
                """
                (infer "scope")
                Test.isOk
            , codeTest
                """
                SKIP 2
                """
                """
                scope =
                    @r = { x = 0, y = mut 1 }

                    { x, @y } =
                        r

                    xx as Number =
                        x

                    @yy as @Number =
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
                    @record = { x = 0, y = mut 0 }
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
                    @record = { x = 0, y = mut 0 }
                    doStuff @record.x @record.y
                """
                (infer "scope")
                (Test.errorContains [ "same mutable twice in the same function call" ])
            , codeTest
                """
                SKIP Unpacking an immutable does not consume the unique
                """
                """
                scope =
                    @record = { x = 0, y = mut 0 }

                    a = record.x

                    @record.x += 1
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
        , Test.Group
            """
            Constructors that take at least one mutable argument will produce a mutable version of their type
            """
            [
            , codeTest
                """
                Annotated
                """
                """
                union Something =
                    , Mutable @Number
                    , Immutable Number

                scope =
                    @m as @Something =
                        Mutable (mut 0)

                    i as Something =
                        Immutable 1
                """
                (infer "scope")
                Test.isOk
            , codeTest
                """
                Inferred
                """
                """
                union Something =
                    , Mutable @Number
                    , Immutable Number

                scope =
                    @m =
                        Mutable (mut 0)

                    i =
                        Immutable 1

                    @mm as @Something =
                        m

                    ii as Something =
                        i
                """
                (infer "scope")
                Test.isOk
            , codeTest
                """
                Free
                """
                """
                union Blah a =
                    , Blah a

                scope =
                    @m =
                        Blah (mut 0)

                    @mm as Blah @Number =
                        m
                """
                (infer "scope")
                Test.isOk
            ]
        ]

