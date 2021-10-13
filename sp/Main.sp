
allTests =
    is [ Test ]

    [
    , Compiler/GetTokens_Test.tests
    ]


color code text =
    is Text -> Text -> Text

    code .. text .. "\x1b[0m"


green =
    color "\x1b[32m"


yellow =
    color "\x1b[33m"


red =
    color "\x1b[31m"


indent s =
    is Text -> Text
    s
      >> Text.split "\n"
      >> List.map (fn l: "  " .. l)
      >> Text.join "\n"


testOutcomeToText name outcome =
    is Text -> Test.TestOutcome -> Text

    try outcome as
        Test.Success:
            green << "* " .. name

        Test.Skipped:
            yellow << "? " .. name

        Test.Error error:
            (red << "-----------------------\nX " .. name) .. "\n" .. indent error


main argv =
  allTests
      >> Test.flatten
      >> List.map (fn (name & outcome): testOutcomeToText name outcome)
      >> Text.join "\n"
