allTests as [ Test ] =
    [
#    , Compiler/Lexer_Test.tests
#    , Compiler/Parser_Test.tests
#    , Compiler/MakeCanonical_Test.tests
    , Compiler/TypeCheck_Test.tests
    ]


#, Compiler/CanonicalToJs_Test.tests
#    , Hash_Test.tests
#    , Array_Test.tests
#    , List_Test.tests
#    , Dict_Test.tests
#    , RefHierarchy_Test.tests
#    , Uniqueness.specs
indent as fn Text: Text =
    fn s:
    s
    >> Text.split "\n" __
    >> List.map (fn l: "  " .. l) __
    >> Text.join "\n" __


testOutcomeToText as fn Text, Text, Test.TestOutcome: Text =
    fn name, code, outcome:
    try outcome as
        Test.'success: Term.green << "* PASS: " .. name
        Test.'skipped: Term.yellow << "* skip: " .. name
        Test.'error error: (Term.red << "FAIL ! " .. name) .. "\n" .. indent code .. "\n" .. indent error


order as fn Test.TestOutcome: Int =
    fn outcome:
    try outcome as
        Test.'success: 0
        Test.'skipped: 1
        Test.'error _: 2


selftestMain as fn None: IO Int =
    fn 'none:
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
    selftestMain 'none
