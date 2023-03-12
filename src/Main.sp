
allTests as [ Test ] =
    [
    , Compiler/Lexer_Test.tests
    , Compiler/Parser_Test.tests
    , Compiler/MakeCanonical_Test.tests
    , Compiler/TypeCheck_Test.tests
    , Hash_Test.tests
    , Array_Test.tests
    , List_Test.tests
    , Dict_Test.tests
    , RefHierarchy_Test.tests
    , Uniqueness.specs
    ]





#
# TODO would be nice to have an args library
#
alias Option state =
  {
  , name as Text
  , info as Text
  , parser as fn Maybe Text, state: Result Text state
  }


parseArguments as fn [Option state], [Text], state: Result Text ([Text] & state) =
    fn options, args, initState:

    optionTexts & others =
        List.partition (Text.startsWith "--" __) args

    findOption as fn Text, state: Result Text state =
        fn optionText, state:

        try Text.split "=" optionText as
            , []:
                Ok state

            , optionName :: rest:
                try List.find (fn o: o.name == optionName) options as
                    , Nothing:
                        Err << "Unknown option " .. optionName

                    , Just option:
                        value = if rest == [] then Nothing else Just (Text.join "=" rest)
                        option.parser value state

    initState
    >> List.forRes __ optionTexts findOption
    >> Result.map (Tuple.pair others __) __



#
# Errors
#


indent as fn Text: Text =
    fn s:
    s
    >> Text.split "\n" __
    >> List.map (fn l: "  " .. l) __
    >> Text.join "\n" __


testOutcomeToText as fn Text, Text, Test.TestOutcome: Text =
    fn name, code, outcome:

    try outcome as
        , Test.Success:
            Term.green << "* PASS: " .. name

        , Test.Skipped:
            Term.yellow << "* skip: " .. name

        , Test.Error error:
            (Term.red << "FAIL ! " .. name) .. "\n" .. indent code .. "\n" .. indent error


order as fn Test.TestOutcome: Int =
    fn outcome:
    try outcome as
        , Test.Success: 0
        , Test.Skipped: 1
        , Test.Error _: 2


selftestMain as fn None: IO Int =
    fn None:
    allTests
    >> Test.flattenAndRun
    >> List.sortBy (fn x: order x.outcome & x.name) __
    >> List.map (fn x: testOutcomeToText x.name x.code x.outcome) __
    >> Text.join "\n" __
    >> IO.writeStdout



[# Command line options:

    squarepants
          --> show usage

    squarepants *.sp
          --> compile

          --platform=posix

          --platformflags?

    squarepants selftest
          --> selftest

#]

union CliOptions =
    , Help
    , Selftest
    , Compile {
        , self as Text
        , mainModulePath as Text
        , maybeOutputPath as Maybe Text
        }


alias CliState = {
    , platform as Types/Platform.Platform
    }


cliDefaults as CliState = {
    , platform = Platforms/Posix.platform
    }


availablePlatforms as [Types/Platform.Platform] = [
    , Platforms/Posix.platform
    , Platforms/RawJavaScript.platform
    , Platforms/Browser.platform
    ]


parsePlatformName as fn Maybe Text, CliState: Result Text CliState =
    fn maybeValue, cliState:

    try maybeValue as
        , Nothing:
            Err "Please specify a platform name, for example: `--platform=posix`"

        , Just value:
            try List.find (fn p: p.name == value) availablePlatforms as
                , Nothing:
                    """
  I don't know this platform name: `""" .. value .. """`

  Valid platform names are:

                    """
                    ..
                    (List.map (fn p: "    " .. p.name) availablePlatforms >> Text.join "\n" __)
                    >> Err

                , Just platform:
                    Ok { cliState with platform }



cliOptions as [Option CliState] = [
  , {
    , name = "--platform"
    , info = "select build platform"
    , parser = parsePlatformName
    }
    ]



parseCli as fn [Text]: CliOptions =
    fn args:


    try args as
        , self :: "selftest" :: tail:
            Selftest

        , self :: head :: tail:
            #TODO check that `Text.startsWithRegex ".*[.]sp$" head`?
            {
            , self
            , mainModulePath = head
            , maybeOutputPath = List.head tail
            }
            >> Compile

        , _:
            Help


#
# main
#

main as IO.Program =
    fn env, args:

    try parseArguments cliOptions args cliDefaults as
        , Err message:
            IO.fail message

        , Ok (args & cliState):
            try args as
                , self :: "selftest" :: tail:
                    selftestMain None

                , self :: head :: tail:
                    #TODO check that `Text.startsWithRegex ".*[.]sp$" head`?
                    mainModulePath = head
                    maybeOutputPath = List.head tail
                    Compile.compileMain {
                        , env
                        , selfPath = self
                        , entryModulePath = mainModulePath
                        , maybeOutputPath
                        , platform = cliState.platform
                        }

                , _:
                    """

                    Hi! This is the Squarepants compiler!

                    To compile something, write:

                        squarepants pathToMainModule.sp

                    """
                    >> IO.writeStdout

