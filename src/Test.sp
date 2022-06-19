union Test =
    , Single Text Text (None: TestOutcome)
    , Group Text [ Test ]
    , NotNow Test


union TestOutcome =
    , Success
    , Skipped
    , Error Text


maybeToOutcome as Maybe Text: TestOutcome =
    m:
    try m as
        Just e:
            Error e

        Nothing:
            Success


#
# Test constructors
#


union CodeExpectation ok =
    , CodeExpectation ((ok: Text): Result Text ok: Maybe Text)


valueTest as (a: Text): Text: (None: a): CodeExpectation a: Test =
    toText: title: generateValue: ce:

    CodeExpectation toMaybeError =
        ce

    Single title "" _:
      None
        >> generateValue
        >> Ok
        >> toMaybeError toText
        >> maybeToOutcome


codeTest as (ok: Text): Text: Text: (Text: Result Text ok): CodeExpectation ok: Test =
    toText: title: code: functionToTest: ce:

    CodeExpectation toMaybeError =
        ce

    Single title code _:
        code
            >> functionToTest
            >> toMaybeError toText
            >> maybeToOutcome


freeform as (ok: Maybe Text): CodeExpectation ok =
    test:
    CodeExpectation << toText: result:
    try result as
        Err e: Just e
        Ok actualOk: test actualOk


isOk as CodeExpectation ok =
    CodeExpectation toText: result:
        try result as
            Err e:
                Just e

            Ok actualOk:
                Nothing


isOkAndEqualTo as ok: CodeExpectation ok =
    expectedOk:

    CodeExpectation toText: result:
      try result as
          Err e:
              Just e

          Ok actualOk:
              if actualOk == expectedOk then
                  Nothing
              else
                  [ "expected = "
                  , toText expectedOk
                  , ""
                  , "actual = "
                  , toText actualOk
                  ]
                      >> Text.join "\n"
                      >> Just

errorContains as [Text]: CodeExpectation ok =
    snippets:

    CodeExpectation toText: result:
      try result as
          Ok ok:
              Just << "I was expecting an error, but got: Ok " .. toText ok

          Err e:
              missing =
                  snippets >> List.filter sn: not (Text.contains sn e)
              if missing == [] then
                  Nothing
              else:
                  indentedError =
                      e >> Text.split "\n" >> List.map (l: "    " .. l) >> Text.join "\n"

                  Just << "Error message:\n\n" .. indentedError .. "\n\nis missing snippets: " .. Text.join ", " missing


#
# Test running
#


outcomesRec as Text: Test: [{ name as Text, code as Text, outcome as TestOutcome }]: [{ name as Text, code as Text, outcome as TestOutcome }] =
    path: test: accum:

    try test as
        Single name code f:
            # HACK?
            if Text.startsWith "SKIP" name then
              { name = path .. name, code = "", outcome = Skipped } :: accum

            else
              { name = path .. name, code, outcome = f None } :: accum

        NotNow t:
            { name = path .. getName t, code = "", outcome = Skipped } :: accum

        Group pathSegment ts:
            List.for ts (outcomesRec (path .. pathSegment .. " / ")) accum


getName as Test: Text =
    test:

    try test as
        Single n code f:
            n

        Group n ls:
            n

        NotNow t:
            getName t


flatten as [ Test ]: [{ name as Text, code as Text, outcome as TestOutcome }] =
    tests:

    List.for tests (outcomesRec "") []


errorsFirst as TestOutcome: Number =
    outcome:

    try outcome as
        Error e:
            0 - 1

        Skipped:
            0

        Success:
            1

