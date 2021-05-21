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


codeTest toString title code functionToTest (CodeExpectation toMaybeError) =
    as (ok -> Text) -> Text -> Text -> (Text -> Result Text ok) -> CodeExpectation ok -> Test

    Single (title .. "\n\n" .. code) fn None:
      code
          >> functionToTest
          >> toMaybeError toString
          >> maybeToOutcome


isOk =
    as CodeExpectation ok

    CodeExpectation fn toString result:
      try result as
          Err e:
              Just e

          Ok actualOk:
              Nothing
