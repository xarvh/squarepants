
allTests as [ Test ] = [
    , Compiler/Lexer_Test.tests
    , Compiler/Parser_Test.tests
    , Compiler/MakeCanonical_Test.tests
    , Compiler/TypeCheck_Test.tests
    #, Compiler/CanonicalToJs_Test.tests
#    , Hash_Test.tests
#    , Array_Test.tests
#    , List_Test.tests
#    , Dict_Test.tests
    , RefHierarchy_Test.tests
    ]


#
# Errors
#


indent as Text: Text =
    s:
    s
    >> Text.split "\n"
    >> List.map (l: "  " .. l)
    >> Text.join "\n"


testOutcomeToText as Text: Text: Test.TestOutcome: Text =
    name: code: outcome:

    try outcome as
        Test.Success:
            Term.green << "* PASS: " .. name

        Test.Skipped:
            Term.yellow << "* skip: " .. name

        Test.Error error:
            (Term.red << "FAIL ! " .. name) .. "\n" .. indent code .. "\n" .. indent error


order as Test.TestOutcome: Int =
    outcome:
    try outcome as
        Test.Success: 0
        Test.Skipped: 1
        Test.Error _: 2


selftestMain as None: IO None =
    None:
    allTests
    >> Test.flatten
    >> List.sortBy (x: order x.outcome & x.name)
    >> List.map (x: testOutcomeToText x.name x.code x.outcome)
    >> Text.join "\n"
    >> IO.writeStdout



[# Command line options:

    squarepants
          --> show usage

    squarepants *.sp
          --> compile

          --platform=posix
          --out=outputFile.js

          --platformflags?

    squarepants selftest
          --> selftest

#]

union CliOptions =
    , Help
    , Selftest
    , Compile {
        , self as Text
        , mainModulePath as Text
        , maybeOutputPath as Maybe Text
        }


parseCli as [Text]: CliOptions =
    args:

    try args as
        self :: "selftest" :: tail:
            Selftest

        self :: head :: tail:
            #TODO check that `Text.startsWithRegex ".*[.]sp$" head`?
            {
            , self
            , mainModulePath = head
            , maybeOutputPath = List.head tail
            }
            >> Compile

        _:
            Help


#
# main
#

main as IO.Program =
    env: args:

    try parseCli args as
        Help:
            """

            Hi! This is the Squarepants compiler!

            To compile something, write:

                squarepants pathToMainModule.sp

            """
            >> IO.writeStdout

        Selftest:
            selftestMain None

        Compile { self, mainModulePath, maybeOutputPath }:
            Compile.compileMain env self mainModulePath maybeOutputPath

