allTests as [ Test ] =
    [
    , Human/Format_Test.tests
    , Compiler/Lexer_Test.tests
    , Compiler/Parser_Test.tests
    , Compiler/MakeCanonical_Test.tests
    , Compiler/TypeCheck_Test.tests
    , Targets/Emitted_Test.tests
    , Hash_Test.tests
    , Array_Test.tests
    , List_Test.tests
    , Dict_Test.tests
    , Uniqueness.specs
    , SPLib/Format_Test.tests
    , SPLib/RefHierarchy_Test.tests
    ]



#
# Res to IO.Re
#
formattedToConsoleColoredText as fn Error.FormattedText: Text =
    fn formattedText:
    try formattedText as
        Error.'formattedText_Default t: t
        Error.'formattedText_Emphasys t: Term.yellow t
        Error.'formattedText_Warning t: Term.red t
        Error.'formattedText_Decoration t: Term.blue t


errorToText as fn Error: Text =
    __
    >> Error.toFormattedText
    >> List.map formattedToConsoleColoredText __
    >> Text.join "" __



resToIo as fn Res a: IO.Re a =
    fn res:
    try res as

        'ok a:
            'ok a

        'err e:
            e
            >> errorToText
            >> 'err


#
# TODO would be nice to have an args library
#
Option state =
    {
    , info as Text
    , name as Text
    , parser as fn Maybe Text, state: Result Text state
    }


parseArguments as fn [ Option state ], [ Text ], state: Result Text ([ Text ] & state) =
    fn options, args, initState:
    optionTexts & others =
        List.partition (Text.startsWith "--" __) args

    findOption as fn Text, state: Result Text state =
        fn optionText, state:
        try Text.split "=" optionText as

            []:
                'ok state

            optionName :: rest:
                try List.find (fn o: o.name == optionName) options as

                    'nothing:
                        'err << "Unknown option " .. optionName

                    'just option:
                        value =
                            if rest == [] then 'nothing else 'just (Text.join "=" rest)

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
        Test.'success: Term.green << "* PASS: " .. name
        Test.'skipped: Term.yellow << "* skip: " .. name
        Test.'error error: (Term.red << "FAIL ! " .. name) .. "\n" .. indent code .. "\n" .. indent error


order as fn Test.TestOutcome: Int =
    fn outcome:
    try outcome as
        Test.'success: 0
        Test.'skipped: 1
        Test.'error _: 2


selftestMain as fn @IO: IO.Re None =
    fn @io:
    allTests
    >> Test.flattenAndRun
    >> List.sortBy (fn x: order x.outcome & x.name) __
    >> List.map (fn x: testOutcomeToText x.name x.code x.outcome) __
    >> Text.join "\n" __
    >> __ .. "\n"
    >> IO.writeStdout @io __


formatMain as fn @IO, [ Text ]: IO.Re None =
    fn @io, targets:
    formatText as fn Text, Text: IO.Re Text =
        fn fsPath, content:
        Compiler/Parser.textToFormattableModule
            {
            , errorModule = { content, fsPath }
            , keepComments = 'true
            , stripLocations = 'false
            }
        >> resToIo
        >> onOk fn formattableAst:
        formattableAst
        >> Human/Format.formatStatements { isRoot = 'true, originalContent = content } __
        >> Fmt.render
        >> 'ok

    formatFile as fn Text: IO.Re None =
        fn name:
        IO.readFile @io name
        >> onOk fn moduleAsText:
        formatText name moduleAsText
        >> onOk fn formatted:
        IO.writeFile @io name formatted

    if targets == [] then
        IO.readStdin @io
        >> onOk fn moduleAsText:
        formatText "<stdin>" moduleAsText
        >> onOk fn formatted:
        IO.writeStdout @io formatted
    else
        List.mapRes formatFile targets
        >> onOk fn _:
        'ok 'none


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

#union CliOptions =
#    , Help
#    , Selftest
#    , Format [Text]
#    , Compile {
#        , self as Text
#        , mainModulePath as Text
#        , maybeOutputPath as Maybe Text
#        }

platformPosix =
    Platforms/Posix.platform (Meta.'importsPath Meta.'installed "posix")


platformBrowser =
    Platforms/Browser.platform (Meta.'importsPath Meta.'installed "browser")


CliState =
    {
    , platform as Platform.Platform
    , corelib as Maybe Text
    }


cliDefaults as CliState =
    {
    , platform = platformPosix
    , corelib = 'nothing
    }


availablePlatforms as [ Platform.Platform ] =
    [
    , platformPosix
    , platformBrowser
    ]


parsePlatformName as fn Maybe Text, CliState: Result Text CliState =
    fn maybeValue, cliState:
    try maybeValue as

        'nothing:
            'err "Please specify a platform name, for example: `--platform=posix`"

        'just value:
            try List.find (fn p: p.name == value) availablePlatforms as

                'nothing:
                    """
                    I don't know this platform name: `
                    """
                    .. value
                    .. """
                    `

                                        Valid platform names are:


                    """
                    .. (List.map (fn p: "    " .. p.name) availablePlatforms >> Text.join "\n" __)
                    >> 'err

                'just platform:
                    'ok { cliState with platform }

parseCorelibPath as fn Maybe Text, CliState: Result Text CliState =
    fn maybeValue, cliState:
    try maybeValue as
        'nothing:
            'err "Please specify the path where your corelib is."

        'just value:
            'ok { cliState with corelib = 'just value }


cliOptions as [ Option CliState ] =
    [
    , {
    , info = "select build platform"
    , name = "--platform"
    , parser = parsePlatformName
    }
    , {
    , info = "specify the path for for the corelib"
    , name = "--corelib"
    , parser = parseCorelibPath
    }
    ]


#
# main
#

main as IO.Program =
    fn @io, @env, rawArgs:
    try parseArguments cliOptions rawArgs cliDefaults as

        'err message:
            IO.writeStderr @io (message .."\n")

        'ok (args & cliState):
            try args as

                self :: "selftest" :: tail:
                    selftestMain @io

                self :: "format" :: tail:
                    formatMain @io tail

                self :: entryPoint :: tail:
                    maybeOutputPath =
                        List.head tail

                    BuildMain.compileMain
                        @io
                        {
                        , entryPoint
                        , maybeOutputPath
                        , platform = cliState.platform
                        , selfPath = self
                        , corelib = cliState.corelib
                        }
                    >> resToIo

                _:
                    """

                    Hi! This is the Squarepants compiler!

                    To compile something, write:

                        squarepants pathToMainModule.sp


                    """
                    >> IO.writeStdout @io __
    >> IO.reToStderr @io __
