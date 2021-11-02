# TODO some tests that should be added:
[# TESTS for single quote text

  - `"\""` passes

  - `"\n"` passes

  - `"
    "` fails

  - `"sdf\\asdf\\"` passes

  - `"sdf\\\asdf\\"` passes

  - `"sdf\\asdf\\\"` fails

  - state.pos as updated manually, so it should be tested!

#]

[# TESTS for triple quoted text

  - """ passes

  - "\\n" passes

  - "
    " fails

  - "sdf\\asdf\\" passes

  - "sdf\\\\asdf\\" passes

  - "sdf\\asdf\\" fails

  - state.pos as updated manually, so it should be tested!

#]


[# TESTS

  multiline comment tests:

  - properly nested comments should pass
  - improperly nested comments should fail
  - non-terminated comments should fail
  - state.pos as updated manually, so it should be tested!

#]


codeTest =
    as Text: Text: (Text: Result Text ok): Test.CodeExpectation ok: Test

    Test.codeTest Debug.toHuman


lexTokens s =
    as Text: Result Text (List Token)

    s
        >> Compiler/Lexer.lexer "Test"
        >> Compiler/TestHelpers.resErrorToText s


lexTokensAndDrop n s =
    s
        >> lexTokens
        >> Result.map (List.drop n)


non_mut_name =
    Token.Name Token.NameNoModifier


tests =
    Test.Group "Lexer"
        [ keywords
        , unaryAddittiveOps
        , indentation
        , comments
        , underscores
        ]


keywords =
    Test.Group "keywords"
        [ codeTest "[reg] `fn` as a keyword"
            "fn = 1"
            lexTokens
            (Test.isOkAndEqualTo
                [ Token 0 0 << Token.NewSiblingLine
                , Token 0 2 << Token.Fn
                , Token 3 4 << Token.Defop { mutable = False }
                , Token 5 6 << Token.NumberLiteral "1"
                ]
            )

        ]


unaryAddittiveOps =
    Test.Group "Unary addittive ops"
        [ codeTest "-a"
            "-a"
            lexTokens
            (Test.isOkAndEqualTo
                [ Token 0 0 << Token.NewSiblingLine
                , Token 0 1 << Token.Unop Prelude.unaryMinus
                , Token 1 2 << non_mut_name "a"
                ]
            )
        , codeTest "a - -a"
            "a - -a"
            lexTokens
            (Test.isOkAndEqualTo
                [ Token 0 0 << Token.NewSiblingLine
                , Token 0 1 << non_mut_name "a"
                , Token 2 3 << Token.Binop Prelude.subtract
                , Token 4 5 << Token.Unop Prelude.unaryMinus
                , Token 5 6 << non_mut_name "a"
                ]
            )
        , codeTest "a-a"
            "a-a"
            lexTokens
            (Test.isOkAndEqualTo
                [ Token 0 0 << Token.NewSiblingLine
                , Token 0 1 << non_mut_name "a"
                , Token 1 2 << Token.Unop Prelude.unaryMinus
                , Token 2 3 << non_mut_name "a"
                ]
            )
        , codeTest "Mutable colon:"
            "@:"
            lexTokens
            (Test.isOkAndEqualTo
                [ Token 0 0 Token.NewSiblingLine
                , Token 0 2 Token.MutableColon
                ]
            )
        , codeTest "-="
            "-="
            lexTokens
            (Test.isOkAndEqualTo
                [ Token 0 0 Token.NewSiblingLine
                , Token 0 2 << Token.Binop Prelude.mutableSubtract
                ]
            )
        ]


indentation =
    Test.Group "Blocks, sibling lines, indentation"
        [ codeTest "1"
            "\na =\n 1\nb = 1"
            lexTokens
            (Test.isOkAndEqualTo
                [ Token 1 1 << Token.NewSiblingLine
                , Token 1 2 << non_mut_name "a"
                , Token 3 4 << Token.Defop { mutable = False }
                , Token 5 6 << Token.BlockStart
                , Token 6 7 << Token.NumberLiteral "1"
                , Token 8 8 << Token.BlockEnd
                , Token 8 8 << Token.NewSiblingLine
                , Token 8 9 << non_mut_name "b"
                , Token 10 11 << Token.Defop { mutable = False }
                , Token 12 13 << Token.NumberLiteral "1"
                ]
            )
        ]




comments =
    Test.Group "Comments"
        [ codeTest "[reg] statement after comment"
            "\n#\na = 1\n"
            lexTokens
            (Test.isOkAndEqualTo
                [ Token 1 1 << Token.NewSiblingLine
                , Token 1 2 << Token.Comment
                , Token 3 3 << Token.NewSiblingLine
                , Token 3 4 << non_mut_name "a"
                , Token 5 6 << Token.Defop { mutable = False }
                , Token 7 8 << Token.NumberLiteral "1"
                ]
            )
        , codeTest "[reg] nested comments allow a spurious newline?"
            "\n[#[##]#]\na = 1\n"
            lexTokens
            (Test.isOkAndEqualTo
                [ Token 1 1 << Token.NewSiblingLine
                , Token 1 8 << Token.Comment
                , Token 10 10 << Token.NewSiblingLine
                , Token 10 11 << non_mut_name "a"
                , Token 12 13 << Token.Defop { mutable = False }
                , Token 14 15 << Token.NumberLiteral "1"
                ]
            )
        , codeTest "Single line"
            "# hello"
            lexTokens
            (Test.isOkAndEqualTo
                [ Token 0 0 <<  Token.NewSiblingLine
                , Token 0 7 <<  Token.Comment
                ]
            )
        , codeTest "Multi line"
            """
[# single line #]

a [# inline #] = 1

[#
    multi line
#]

[# [# nested #] #]
"""
            lexTokens
            (Test.isOkAndEqualTo
                [ Token 1 1 << Token.NewSiblingLine
                , Token 1 17 << Token.Comment
                , Token 20 20 << Token.NewSiblingLine
                , Token 20 21 << non_mut_name "a"
                , Token 22 33 << Token.Comment
                , Token 35 36 << Token.Defop { mutable = False }
                , Token 37 38 << Token.NumberLiteral "1"
                , Token 40 40 << Token.NewSiblingLine
                , Token 40 59 << Token.Comment
                , Token 62 62 << Token.NewSiblingLine
                , Token 62 79 << Token.Comment
                ]
            )
        ]


underscores =
    Test.Group "Underscores"
        [ codeTest "'_' as a Name"
            "_"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo [ Token 0 1 << Token.Name Token.NameNoModifier "_" ])
        , codeTest "'_10_20' as a Name"
            "_10_20"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo [ Token 0 6 << Token.Name Token.NameNoModifier "_10_20" ])
        , codeTest "'10_20' as a Number"
            "10_20"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo [ Token 0 5 << Token.NumberLiteral "10_20" ])
        ]
