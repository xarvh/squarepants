var Test =
    , 'single Text Text (fn None: TestOutcome)
    , 'group Text [ Test ]
    , 'notNow Test


var TestOutcome =
    , 'success
    , 'skipped
    , 'error Text


maybeToOutcome as fn Maybe Text: TestOutcome =
    fn m:
    try m as
        'just e: 'error e
        'nothing: 'success


#
# Test constructors
#

var CodeExpectation ok =
    , 'codeExpectation (fn fn ok: Text, Result Text ok: Maybe Text)


valueTest as fn fn a: Text, Text, fn None: a, CodeExpectation a: Test =
    fn toText, title, generateValue, ce:
    'codeExpectation toMaybeError =
        ce

    'single title "" fn _:
        'none
        >> generateValue
        >> 'ok
        >> toMaybeError toText __
        >> maybeToOutcome


codeTest as fn fn ok: Text, Text, Text, fn Text: Result Text ok, CodeExpectation ok: Test =
    fn toText, title, code, functionToTest, ce:
    'codeExpectation toMaybeError =
        ce

    'single title code fn _:
        code
        >> functionToTest
        >> toMaybeError toText __
        >> maybeToOutcome


freeform as fn fn ok: Maybe Text: CodeExpectation ok =
    fn test:
    'codeExpectation
    << (fn toText, result:
     try result as
         'err e: 'just e
         'ok actualOk: test actualOk
    )


isOk as CodeExpectation ok =
    'codeExpectation fn toText, result:
        try result as
            'err e: 'just e
            'ok actualOk: 'nothing


isOkAndEqualTo as fn ok: CodeExpectation ok =
    fn expectedOk:
    'codeExpectation fn toText, result:
        try result as

            'err e:
                'just e

            'ok actualOk:
                if actualOk == expectedOk then
                    'nothing
                else
                    [
                    , "expected = "
                    , toText expectedOk
                    , ""
                    , "actual = "
                    , toText actualOk
                    ]
                    >> Text.join "\n" __
                    >> 'just


errorContains as fn [ Text ]: CodeExpectation ok =
    fn snippets:
    'codeExpectation fn toText, result:
        try result as

            'ok ok:
                'just << "I was expecting an error, but got: Ok " .. toText ok

            'err e:
                missing =
                    List.filter snippets (fn sn: not (Text.contains sn e))

                if missing == [] then
                    'nothing
                else
                    indentedError =
                        e >> Text.split "\n" __ >> List.map __ (fn l: "    " .. l) >> Text.join "\n" __

                    'just << "Error message:\n\n" .. indentedError .. "\n\nis missing snippets: " .. Text.join ", " missing


#
# Test running
#

T =
    {
    , code as Text
    , getOutcome as fn None: TestOutcome
    , name as Text
    }


outcomesRec as fn [ T ], Text, Test: [ T ] =
    fn accum, path, test:
    try test as

        'single name code f:
            { code, getOutcome = f, name = path .. name } :: accum

        'notNow t:
            thing as T =
                { code = "", getOutcome = fn 'none: 'skipped, name = path .. getName t }

            thing :: accum

        'group pathSegment ts:
            List.for accum ts (outcomesRec __ (path .. pathSegment .. " / ") __)


getName as fn Test: Text =
    fn test:
    try test as
        'single n code f: n
        'group n ls: n
        'notNow t: getName t


flattenAndRun as fn [ Test ]: [ { code as Text, name as Text, outcome as TestOutcome } ] =
    fn tests:
    flattened as [ T ] =
        outcomesRec [] "" ('group "" tests) >> List.map __ (fn r: if Text.contains "SKIP" r.name then { r with getOutcome = fn 'none: 'skipped } else r)

    onlies as [ T ] =
        List.filter flattened (fn r: Text.contains "ONLY" r.name)

    runnable as [ T ] =
        if onlies /= [] then onlies else flattened

    runTest as fn T: { code as Text, name as Text, outcome as TestOutcome } =
        fn r:
        { code, getOutcome, name } =
            r

        { code, name, outcome = getOutcome 'none }

    List.map runnable runTest


errorsFirst as fn TestOutcome: Number =
    fn outcome:
    try outcome as
        'error e: -1
        'skipped: 0
        'success: 1
