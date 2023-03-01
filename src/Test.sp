union Test =
    , Single Text Text (fn None: TestOutcome)
    , Group Text [ Test ]
    , NotNow Test


union TestOutcome =
    , Success
    , Skipped
    , Error Text


maybeToOutcome as fn Maybe Text: TestOutcome =
    fn m:
    try m as
        , Just e:
            Error e

        , Nothing:
            Success


#
# Test constructors
#


union CodeExpectation ok =
    , CodeExpectation (fn (fn ok: Text), Result Text ok: Maybe Text)


valueTest as fn (fn a: Text), Text, (fn None: a), CodeExpectation a: Test =
    fn toText, title, generateValue, ce:

    CodeExpectation toMaybeError =
        ce

    Single title "" fn _:
      None
        >> generateValue
        >> Ok
        >> toMaybeError toText __
        >> maybeToOutcome


codeTest as fn (fn ok: Text), Text, Text, (fn Text: Result Text ok), CodeExpectation ok: Test =
    fn toText, title, code, functionToTest, ce:

    CodeExpectation toMaybeError =
        ce

    Single title code fn _:
        code
            >> functionToTest
            >> toMaybeError toText __
            >> maybeToOutcome


freeform as fn (fn ok: Maybe Text): CodeExpectation ok =
    fn test:

    CodeExpectation << fn toText, result:
    try result as
        , Err e: Just e
        , Ok actualOk: test actualOk


isOk as CodeExpectation ok =
    CodeExpectation fn toText, result:
        try result as
            , Err e:
                Just e

            , Ok actualOk:
                Nothing


isOkAndEqualTo as fn ok: CodeExpectation ok =
    fn expectedOk:

    CodeExpectation fn toText, result:
      try result as
          , Err e:
              Just e

          , Ok actualOk:
              if actualOk == expectedOk then
                  Nothing
              else
                  [ "expected = "
                  , toText expectedOk
                  , ""
                  , "actual = "
                  , toText actualOk
                  ]
                      >> Text.join "\n" __
                      >> Just

errorContains as fn [Text]: CodeExpectation ok =
    fn snippets:

    CodeExpectation fn toText, result:
      try result as
          , Ok ok:
              Just << "I was expecting an error, but got: Ok " .. toText ok

          , Err e:
              missing =
                  snippets >> List.filter (fn sn: not (Text.contains sn e)) __
              if missing == [] then
                  Nothing
              else
                  indentedError =
                      e >> Text.split "\n" __ >> List.map (fn l: "    " .. l) __ >> Text.join "\n" __

                  Just << "Error message:\n\n" .. indentedError .. "\n\nis missing snippets: " .. Text.join ", " missing


#
# Test running
#


alias T = {
    , name as Text
    , code as Text
    , getOutcome as fn None: TestOutcome
    }


outcomesRec as fn Text, Test, [T]: [T] =
    fn path, test, accum:

    try test as
        , Single name code f:
              { name = path .. name, code, getOutcome = f } :: accum

        , NotNow t:
              thing as T = { name = path .. getName t, code = "", getOutcome = fn None: Skipped }
              thing :: accum

        , Group pathSegment ts:
              List.for accum ts (outcomesRec (path .. pathSegment .. " / ") __ __)


getName as fn Test: Text =
    fn test:

    try test as
        , Single n code f:
            n

        , Group n ls:
            n

        , NotNow t:
            getName t


flattenAndRun as fn [ Test ]: [{ name as Text, code as Text, outcome as TestOutcome }] =
    fn tests:

    flattened =
        outcomesRec "" (Group "" tests) []
        >> List.map (fn r: if Text.contains "SKIP" r.name then { r with getOutcome = fn None: Skipped } else r) __

    onlies =
        flattened
        >> List.filter (fn r: Text.contains "ONLY" r.name) __

    runnable =
        if onlies /= [] then onlies else flattened


    runTest =
        fn r:
        { name, code, getOutcome } = r
        { name, code, outcome = getOutcome None }


    List.map runTest runnable


errorsFirst as fn TestOutcome: Number =
    fn outcome:

    try outcome as
        , Error e:
            -1

        , Skipped:
            0

        , Success:
            1

