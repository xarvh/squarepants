union Test =
    , Single Text Text (None -> TestOutcome)
    , Group Text [ Test ]
    , NotNow Test


union TestOutcome =
    , Success
    , Skipped
    , Error Text


maybeToOutcome m =
    is Maybe Text -> TestOutcome
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
    is (ok -> Text) -> Text -> Text -> (Text -> Result Text ok) -> CodeExpectation ok -> Test

    Single title code fn None:
      code
          >> functionToTest
          >> toMaybeError toText
          >> maybeToOutcome


isOk =
    is CodeExpectation ok

    CodeExpectation fn toText result:
      try result as
          Err e:
              Just e

          Ok actualOk:
              Nothing


isOkAndEqualTo expectedOk =
    is ok -> CodeExpectation ok

    CodeExpectation fn toText result:
      try result as
          Err e:
              Just e

          Ok actualOk:
              if actualOk == expectedOk:
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
    is Text -> Test -> [ Text & Text & TestOutcome ] -> [ Text & Text & TestOutcome ]

    try test as
        Single name code f:
            path .. name & code & (f None) :: accum

        NotNow t:
            path .. getName t & "" & Skipped :: accum

        Group pathSegment ts:
            List.foldl (outcomesRec (path .. pathSegment .. " / ")) ts accum


getName test =
    is Test -> Text

    try test as
        Single n code f:
            n

        Group n ls:
            n

        NotNow t:
            getName t


flatten tests =
    is [ Test ] -> [ Text & Text & TestOutcome ]

    List.foldl (outcomesRec "") tests []


errorsFirst outcome =
    is TestOutcome -> Number

    try outcome as
        Error e:
            -1

        Skipped:
            0

        Success:
            1

