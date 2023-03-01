

allTests as [ Test ] =
    [
    , Compiler/Lexer_Test.tests
    , Compiler/Parser_Test.tests
#    , Compiler/MakeCanonical_Test.tests
#    , Compiler/TypeCheck_Test.tests
    #, Compiler/CanonicalToJs_Test.tests
    , Hash_Test.tests
    , Array_Test.tests
    , List_Test.tests
    , Dict_Test.tests
#    , RefHierarchy_Test.tests
#    , Uniqueness.specs
    ]


indent as fn Text: Text =
    fn s:
    s
    >> Text.split "\n" __
    >> List.map (fn l: "  " .. l) __
    >> Text.join "\n" __


testOutcomeToText as fn Text, Text, Test.TestOutcome: Text =
    fn name, code, outcome:

    try outcome as
        , Test.Success:
            Term.green << "* PASS: " .. name

        , Test.Skipped:
            Term.yellow << "* skip: " .. name

        , Test.Error error:
            (Term.red << "FAIL ! " .. name) .. "\n" .. indent code .. "\n" .. indent error


order as fn Test.TestOutcome: Int =
    fn outcome:
    try outcome as
        , Test.Success: 0
        , Test.Skipped: 1
        , Test.Error _: 2


selftestMain as fn None: IO Int =
    fn None:
    allTests
    >> Test.flattenAndRun __
    >> List.sortBy (fn x: order x.outcome & x.name) __
    >> List.map (fn x: testOutcomeToText x.name x.code x.outcome) __
    >> Text.join "\n" __
    >> IO.writeStdout


#
# main
#

main as IO.Program =
    fn env, args:

    selftestMain None

