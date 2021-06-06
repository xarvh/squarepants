
allTests =
    as [ Test ]

    [
    , Compiler/GetTokens_Test.tests
    ]


color code text =
    as Text -> Text -> Text

    code .. text .. "\x1b[0m"


green =
    color "\x1b[32m"


yellow =
    color "\x1b[33m"


red =
    color "\x1b[31m"



testOutcomeToText name outcome =
    as Text -> Test.TestOutcome -> Text

    try outcome as
        Test.Success:
            green << "* " .. name

        Test.Skipped:
            yellow << "? " .. name

        Test.Error error:
            (red << "X " .. name) .. "\n" .. error


main argv =
  allTests
      >> Test.flatten
      >> fn (name & outcome): testOutcomeToText name outcome
      >> Text.join "\n"
