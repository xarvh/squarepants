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
      ]


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
            ]
        ]




#
# Boilerplate code, not really needed to understand the system
#

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

