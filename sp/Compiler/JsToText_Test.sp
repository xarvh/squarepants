
io_eval _ =
    todo "eval"


tests =
    as Test
    Test.Group "JsToString"
        [ self
        ]


codeTest =
    Test.codeTest Debug.toHuman


runProgram variable modules =
    as Text: [CA.Module]: Result Text Text

    todo "runProgram"
    [#
    endStatements =
        [ Compiler/CanonicalToJs.translatePath variable .. ";" ]

    evalToResult stats s =
        if Text.startsWith "eval()" s:
            (s :: "" :: stats)
                >> Text.join "\n\n"
                >> Err

        else
            Ok s

#    translateAll eenv globals modules =
#        as Error.Env: CA.Globals: [CA.Module]: List JA.Statement

    eenv =
        as Error.Env
        { moduleByName = Dict.empty }

    mod
        >> Compiler/CanonicalToJs.translateAll eenv
        >> List.map (Compiler/JsToString.emitStatement 0)
        >> fn stats:
                [ Compiler/CanonicalToJs.nativeDefinitions :: stats, endStatements ]
                    >> List.concat
                    >> Text.join "\n\n"
                    >> io_eval
                    >> evalToResult stats
                    #]


eval variable code =
    as Text: Text: Result Text Text
    code
        >> TH.textToCanonicalModule
        >> TH.resErrorToStrippedText code
        >> Result.onOk (fn mod: runProgram variable [ mod ])


self =
    as Test
    Test.Group "Markdown.eval workaround itself works"
        [ codeTest "base"
            """
            x = 1 + 1
            """
            (eval "Test.x")
            (Test.isOkAndEqualTo "2")
        , codeTest "None is null"
            """
            x = None
            """
            (eval "Test.x")
            (Test.isOkAndEqualTo "null")
        , codeTest "undefined reference"
            """
            x = None
            """
            (eval "Test.y")
            (Test.errorContains ["not defined"])
        ]
