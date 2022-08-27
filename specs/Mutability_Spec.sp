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
      ]


#
# Boilerplate code, not really needed to understand the system
#

valueTest as Text: (None: a): Test.CodeExpectation a: Test =
    Test.valueTest toHuman

codeTest =
    Test.codeTest Debug.toHuman

check as Text: Result Text CA.Module =
    code:

    blah as Res CA.Module =
        params as Compiler/MakeCanonical.Params = {
            , meta = TH.meta
            , stripLocations = True
            , source = TH.source
            , name = TH.moduleName
            }

        Compiler/MakeCanonical.textToCanonicalModule params code
        >> onOk module:

        Compiler/UniquenessCheck.doModule module
        >> onOk moduleWithDestroyIn:

        modules =
            Dict.insert TH.moduleUmr module Prelude.coreModulesByUmr

        Compiler/Pipeline.globalExpandedTypes modules >> onOk expandedTypes:

        { types, constructors, instanceVariables } = expandedTypes

        env as Compiler/TypeCheck.Env = {
            , types
            , constructors
            , currentModule = TH.moduleUmr
            , meta = TH.meta
            , nonFreeTyvars = Dict.empty
            , nonAnnotatedRecursives = Dict.empty
            , instanceVariables = Dict.mapKeys CA.RefRoot instanceVariables
            }

        Compiler/TypeCheck.fromModule env module
        >> onOk typeCheckEnv:

        Ok moduleWithDestroyIn


    TH.resErrorToStrippedText code blah



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
            average as [Number]: Number =
                numbers:

                # The core function `mut` transforms an immutable value in a mutable one
                @total as @Number =
                    mut 0

                @count as @Number =
                    mut 0

                each numbers number:
                    @total += number
                    @count += 1

                # Most algebraic operators accept both immutable and mutable values
                # Also in Squarepants division by 0 yields 0
                @total / @count
            """
            check
            Test.isOk

        , codeTest
            """
            SKIP Example: File IO
            """
            """
            logToFile as @IO: Text: Result IO.Error None =
                @io: content:

                IO.openFile @io IO.Append "blah.log"
                >> isOk @fileDescriptor:

                IO.writeFile @io content @fileDescriptor

                # fileDescriptor is automatically closed here
            """
            check
            Test.isOk
        ]




uniquenessTyping as Test =
    Test.Group "Uniqueness Typing" [
        , codeTest
            """
            Types can be flagged as mutable
            """
            """
            alias A = @Number

            alias B a = @a

            alias C = @(Text: Number)
            """
            check
            Test.isOk
        , Test.Group "Mutable types are not interchangeable with their non-mutable counterpart" [
            , codeTest "1"
                """
                a as @Number = 1
                """
                check
                (Test.errorContains ["The two types are not compatible"])
            , codeTest "2"
                """
                a as Number = mut 1
                """
                check
                (Test.errorContains ["The two types are not compatible"])
            ]
        , Test.Group "A variable with mutable type must be explicitly declared as mutable with `@`" [
            , codeTest "1"
                """
                z =
                    @a as @Number = mut 1
                """
                check
                Test.isOk
            , codeTest "2"
                """
                a as @Number = mut 1
                """
                check
                (Test.errorContains ["annotation and variable declaration have different mutability"])
            ]
        , Test.Group """Referencing a mutable variable "consumes" it""" [
            , codeTest "base"
                """
                scope =
                    @x =
                        mut 1

                    @y =
                        # The first time we do it it works!
                        x

                    @z =
                        # But here `x` is now consumed, so we get a compiler error!
                        x
                """
                check
                (Test.errorContains ["used already here"])

            , codeTest "tuple"
                """
                scope =
                    @x =
                        mut 1

                    @y =
                        x & x
                """
                check
                (Test.errorContains ["used already here"])
            ]
        , Test.Group
            """
            Functions can consume mutables by using `:-` instead of `:`
            """ [
            , codeTest
                """
                Consume a unique
                """
                """
                consumer as @Number:- None =
                    x:-
                    None

                scope =
                    @x =
                        mut 1

                    consumer x

                    x
                """
              check
              (Test.errorContains [ "x", "used already here"])
            , codeTest
                """
                Refuse an immutable
                """
                """
                consumer as @Number:- None =
                    x:-
                    None

                scope =
                    x =
                        1

                    consumer x
                """
                check
                (Test.errorContains ["incompatible"])
            , codeTest
                """
                Refuse to mutate 1
                """
                """
                consumer as @Number:- None =
                    x:-
                    None

                scope as None =
                    @x as @Number =
                        mut 1

                    consumer @x
                """
                check
                (Test.errorContains ["but the function needs to consume it"])
            , codeTest
                """
                Refuse to mutate 2
                """
                """
                scope =

                    consumer =
                        x:-
                        None

                    @x =
                        mut 1

                    consumer @x
                """
                check
                (Test.errorContains ["but the function needs to consume it"])
            , codeTest
                """
                Annotation should match implementation 1
                """
                """
                consumer as @Number:- None =
                    @x:
                    None
                """
                check
                (Test.errorContains ["different mutability"])
            , codeTest
                """
                Annotation should match implementation 2
                """
                """
                consumer as @Number: None =
                    @x:-
                    None
                """
                check
                (Test.errorContains ["different mutability"])
            , codeTest
                """
                SKIP When consumed the `@` is optional in annotation, definition and call, and will be removed by the formatter.
                """
                """
                """
                check
                (Test.errorContains [""])
            ]
        , Test.Group
            """
            "When we construct a function with mutable elements in its closure that function itself must be mutable."
            """ [
            , codeTest
                """
                SKIP base
                """
                """
                scope =
                    @x = mut 1
                    z: x
                """
              check
              (Test.errorContains [ "?" ])
            ]
        ]


mutation as Test =
    Test.Group "Mutation" [
        , Test.Group "Mutables can be mutated in place" [
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
                    @x = mut 1
                    @x += 1
                    @x += 1
                """
                check
                Test.isOk
            , codeTest
                """
                Mutation requires the unique not to be consumed
                """
                """
                scope =
                    @x = mut 1
                    consume x
                    @x += 1
                """
                check
                (Test.errorContains ["being consumed here"])
            ]
        , Test.Group "A function can be defined to mutate its arguments" [
            , codeTest
                """
                base
                """
                """
                funz as @Number: None =
                    @a:
                    @a += 3

                scope =
                    @x = mut 0
                    funz @x
                    funz @x
                """
                check
                Test.isOk
            ]
        , Test.Group "Calling a function that mutates a unique variable temporarily consumes the variable." [
            , codeTest
                """
                base
                """
                """
                scope =
                    @x = mut 0
                    funz @x @x
                """
                check
                (Test.errorContains [ "twice" ])
            ]
        , Test.Group "Functions that mutate their arguments cannot be auto-curried; partial calls will result in a compiler error." [
            , codeTest
                """
                SKIP No point in checking this since auto-curry will be removed.
                """
                """
                """
                check
                (Test.errorContains [])
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
                check
                (Test.errorContains [])
            ]
        ]



parentScope as Test =
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
                check
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
                check
                (Test.errorContains [ "different mutability" ])
            , codeTest
                """
                Annotation, invalid 2
                """
                """
                scope =
                    r as { x as Number, y as @Number } =
                        { x = 0, y = mut 0 }
                """
                check
                (Test.errorContains [ "different mutability" ])

            , codeTest
                """
                No annotation, valid
                """
                """
                scope =
                    @r =
                        { x = 0, y = mut 0 }
                """
                check
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
                check
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
                check
                (Test.errorContains [ "UNIQUE" ])
            ]
        ]



