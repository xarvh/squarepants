[#

This file both documents and tests Squarepants' mutability system.

Squarepants mutability is loosely based on https://en.wikipedia.org/wiki/Uniqueness_type

Mutable variables can be "consumed" and re-assigned.

They are guaranteed to be unique.

The rules for this mutability system are not trivial, but they are designed so that the overwhelming majority of actual use cases *are* easy to understand; rather than explaining all the rules to a new user the design relies on the compiler to give meaningful feedback whenever a rule is violated, so that the user can learn "as they go".

All the crunching is done at compile time, which means the run-time does not need to keep track of allocation/destruction: no garbage collection, no reference counting needed.

#]


specs as Test = Test.Group "Mutability spec" [
    , Test.Group
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
#            average as [Number]: Number =
#                numbers:
#
#                # The core function `mut` transforms an immutable value in a mutable one
#                @total as @Number =
#                    mut 0
#
#                @count as @Int =
#                    mut 0
#
#                List.each numbers number:
#                    @total += number
#                    @count += 1
#
#                # Most algebraic operators accept both mutable and immutable values
#                @total / @count
            """
            check
            Test.isOk

        , codeTest
            """
            Example: File IO?
            """
            """
#            logToFile as @IO: Text: Result IO.Error None =
#                @io: content:
#
#                IO.openFile @io IO.Append "blah.log"
#                >> isOk @fileDescriptor:
#
#                IO.writeFile @io content @fileDescriptor
#
#                # fileDescriptor is autoamtically closed here
            """
            check
            Test.isOk
        ]
    , Test.Group
        #
        #
        #
        """
        Why have mutability?
        """
        [
        # TODO: Can we test any of these?
        , codeTest "Faster number-crunching" "" check Test.isOk
        , codeTest "Opt-in manual control of memory allocation" "" check Test.isOk
        , codeTest "Some algorithms are easier to express with mutability" "" check Test.isOk
        , codeTest "Rust-like lifetime check of resources" "" check Test.isOk
        ]
    , Test.Group
        #
        #
        #
        """
        How do we keep mutability sane?
        """
        [
        , codeTest
            """
            Forbid global mutables
            """
            """
            @a = mut 1
            """
            check
            (Test.errorContains [ "Mutable" ])
        ]
    ]



#
# Boilerplate code, not really needed to understand the system
#

codeTest =
    Test.codeTest Debug.toHuman

check as Text: Result Text Compiler/TypeCheck.Env =
    code:

    tcEnvResult as Res Compiler/TypeCheck.Env =
        params as Compiler/MakeCanonical.Params = {
            , meta = TH.meta
            , stripLocations = True
            , source = TH.source
            , name = TH.moduleName
            }

        Compiler/MakeCanonical.textToCanonicalModule params code >> onOk module:

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

    TH.resErrorToStrippedText code tcEnvResult

