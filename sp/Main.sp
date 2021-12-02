
allTests =
    as [ Test ]

    [
#    , Compiler/Lexer_Test.tests
#    , Compiler/Parser_Test.tests
#    , Compiler/MakeCanonical_Test.tests
    , Compiler/TypeCheck_Test.tests
#    , SPCore/List_Test.tests
#    , SPCore/Dict_Test.tests
    ]


color code text =
    as Text: Text: Text

    code .. text .. "\x1b[0m"


blue =
    color  "\x1b[34m"

green =
    color "\x1b[32m"


yellow =
    color "\x1b[33m"


red =
    color "\x1b[31m"


indent s =
    as Text: Text
    s
      >> Text.split "\n"
      >> List.map (fn l: "  " .. l)
      >> Text.join "\n"


testOutcomeToText name code outcome =
    as Text: Text: Test.TestOutcome: Text

    try outcome as
        Test.Success:
            green << "* PASS: " .. name

        Test.Skipped:
            yellow << "* skip: " .. name

        Test.Error error:
            (red << "FAIL ! " .. name) .. "\n" .. indent code .. "\n" .. indent error


formattedToConsoleColoredText formattedText =
    as Error.FormattedText: Text
    try formattedText as
        Error.FormattedText_Default t: t
        Error.FormattedText_Emphasys t: yellow t
        Error.FormattedText_Warning t: red t
        Error.FormattedText_Decoration t: blue t


order outcome =
    as Test.TestOutcome: Int
    try outcome as
        Test.Success: 0
        Test.Skipped: 1
        Test.Error _: 2



main arg =
  if arg == "":
      allTests
          >> Test.flatten
          >> List.sortBy (fn (name & code & outcome): order outcome & name)
          >> List.map (fn (name & code & outcome): testOutcomeToText name code outcome)
          >> Text.join "\n"

  else:
      result =
          arg
              >> Compiler/TestHelpers.textToFormattableModule
              >> Result.mapError (Error.toFormattedText << Compiler/TestHelpers.dummyErrorEnv arg)

      try result as
          Err formattedErrors:
              formattedErrors
                  >> List.map formattedToConsoleColoredText
                  >> Text.join ""

          Ok statements:
              statements
                  >> List.map Debug.toHuman
                  >> Text.join "\n"
                  >> fn s: "===============================\n" .. s

