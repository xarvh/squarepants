
allTests =
    is [ Test ]

    [
    , Compiler/Lexer_Test.tests
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


testOutcomeToText name code outcome =
    is Text -> Text -> Test.TestOutcome -> Text

    try outcome as
        Test.Success:
            green << "* PASS: " .. name

        Test.Skipped:
            yellow << "* skip: " .. name

        Test.Error error:
            (red << "FAIL ! " .. name) .. "\n" .. indent code .. "\n" .. indent error


main arg =
  if arg == "":
      allTests
          >> Test.flatten
          >> List.map (fn (name & code & outcome): testOutcomeToText name code outcome)
          >> Text.join "\n"

  else:
      try Compiler/Lexer_Test.lexTokens arg as
          Err err: err
          Ok tokens:
              tokens
                  >> List.map Debug.toHuman
                  >> Text.join "\n"
                  >> fn s: "===============================\n" .. s

