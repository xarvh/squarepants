union Test =
    , Single Text (None -> TestOutcome)
    , Group Text [ Test ]
    , NotNow Test


union TestOutcome =
    , Success
    , Skipped
    , Error Text


maybeToOutcome m =
    as Maybe Text -> TestOutcome
    try m as
        Just e:
            Error e

        Nothing:
            Success


#
# Test constructors
#


union CodeExpectation ok =
    , CodeExpectation ((ok -> Text) -> Result Text ok -> Maybe Text)


codeTest toText title code functionToTest (CodeExpectation toMaybeError) =
    as (ok -> Text) -> Text -> Text -> (Text -> Result Text ok) -> CodeExpectation ok -> Test

    Single (title .. "\n\n" .. code) fn None:
      code
          >> functionToTest
          >> toMaybeError toText
          >> maybeToOutcome


isOk =
    as CodeExpectation ok

    CodeExpectation fn toText result:
      try result as
          Err e:
              Just e

          Ok actualOk:
              Nothing


isOkAndEqualTo expectedOk =
    as ok -> CodeExpectation ok

    CodeExpectation fn toText result:
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



#
# Test running
#


outcomesRec path test accum =
    as Text -> Test -> [ Text & TestOutcome ] -> [ Text & TestOutcome ]

    try test as
        Single name f:
            path .. name & (f None) :: accum

        NotNow t:
            path .. getName t & Skipped :: accum

        Group pathSegment ts:
            List.foldl (outcomesRec (path .. pathSegment .. " / ")) ts accum


getName test =
    as Test -> Text

    try test as
        Single n f:
            n

        Group n ls:
            n

        NotNow t:
            getName t


flatten tests =
    as [ Test ] -> [ Text & TestOutcome ]

    List.foldl (outcomesRec "") tests []


errorsFirst outcome =
    as TestOutcome -> Number

    try outcome as
        Error e:
            -1

        Skipped:
            0

        Success:
            1

