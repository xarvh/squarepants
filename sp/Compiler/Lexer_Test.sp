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
                [ { end = 0, kind = Token.NewSiblingLine, start = 0 }
                , { end = 2, kind = Token.Fn, start = 0 }
                , { end = 4, kind = Token.Defop { mutable = False }, start = 3 }
                , { end = 6, kind = Token.NumberLiteral "1", start = 5 }
                ]
            )

        ]


unaryAddittiveOps =
    Test.Group "Unary addittive ops"
        [ codeTest "-a"
            "-a"
            lexTokens
            (Test.isOkAndEqualTo
                [ { kind = Token.NewSiblingLine, start = 0, end = 0 }
                , { kind = Token.Unop Prelude.unaryMinus, start = 0, end = 1 }
                , { kind = non_mut_name "a", start = 1, end = 2 }
                ]
            )
        , codeTest "a - -a"
            "a - -a"
            lexTokens
            (Test.isOkAndEqualTo
                [ { kind = Token.NewSiblingLine, start = 0, end = 0 }
                , { kind = non_mut_name "a", start = 0, end = 1 }
                , { kind = Token.Binop Prelude.subtract, start = 2, end = 3 }
                , { kind = Token.Unop Prelude.unaryMinus, start = 4, end = 5 }
                , { kind = non_mut_name "a", start = 5, end = 6 }
                ]
            )
        , codeTest "a-a"
            "a-a"
            lexTokens
            (Test.isOkAndEqualTo
                [ { kind = Token.NewSiblingLine, start = 0, end = 0 }
                , { kind = non_mut_name "a", start = 0, end = 1 }
                , { kind = Token.Unop Prelude.unaryMinus, start = 1, end = 2 }
                , { kind = non_mut_name "a", start = 2, end = 3 }
                ]
            )
        , codeTest "Arrow:"
            "->"
            lexTokens
            (Test.isOkAndEqualTo
                [ { kind = Token.NewSiblingLine, start = 0, end = 0 }
                , { kind = Token.Arrow { mutable = False }, start = 0, end = 2 }
                ]
            )
        , codeTest "-="
            "-="
            lexTokens
            (Test.isOkAndEqualTo
                [ { kind = Token.NewSiblingLine, start = 0, end = 0 }
                , { kind = Token.Binop Prelude.mutableSubtract, start = 0, end = 2 }
                ]
            )
        ]


indentation =
    Test.Group "Blocks, sibling lines, indentation"
        [ codeTest "1"
            "\na =\n 1\nb = 1"
            lexTokens
            (Test.isOkAndEqualTo
                [ { kind = Token.NewSiblingLine, start = 1, end = 1 }
                , { kind = non_mut_name "a", start = 1, end = 2 }
                , { kind = Token.Defop { mutable = False }, start = 3, end = 4 }
                , { kind = Token.BlockStart, start = 5, end = 6 }
                , { kind = Token.NumberLiteral "1", start = 6, end = 7 }
                , { kind = Token.BlockEnd, start = 8, end = 8 }
                , { kind = Token.NewSiblingLine, start = 8, end = 8 }
                , { kind = non_mut_name "b", start = 8, end = 9 }
                , { kind = Token.Defop { mutable = False }, start = 10, end = 11 }
                , { kind = Token.NumberLiteral "1", start = 12, end = 13 }
                ]
            )
        ]




comments =
    Test.Group "Comments"
        [ codeTest "[reg] statement after comment"
            "\n#\na = 1\n"
            lexTokens
            (Test.isOkAndEqualTo
                [ { kind = Token.NewSiblingLine, start = 1, end = 1 }
                , { kind = Token.Comment, start = 1, end = 2 }
                , { kind = Token.NewSiblingLine, start = 3, end = 3 }
                , { kind = non_mut_name "a", start = 3, end = 4 }
                , { kind = Token.Defop { mutable = False }, start = 5, end = 6 }
                , { kind = Token.NumberLiteral "1", start = 7, end = 8 }
                ]
            )
        , codeTest "[reg] nested comments allow a spurious newline?"
            "\n[#[##]#]\na = 1\n"
            lexTokens
            (Test.isOkAndEqualTo
                [ { kind = Token.NewSiblingLine, start = 1, end = 1 }
                , { kind = Token.Comment, start = 1, end = 8 }
                , { kind = Token.NewSiblingLine , start = 10, end = 10 }
                , { kind = non_mut_name "a", start = 10, end = 11 }
                , { kind = Token.Defop { mutable = False }, start = 12, end = 13 }
                , { kind = Token.NumberLiteral "1", start = 14, end = 15 }
                ]
            )
        , codeTest "Single line"
            "# hello"
            lexTokens
            (Test.isOkAndEqualTo
                [ { kind = Token.NewSiblingLine, start = 0, end = 0 }
                , { kind = Token.Comment, start = 0, end = 7 }
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
                [ { kind = Token.NewSiblingLine, start = 1, end = 1 }
                , { kind = Token.Comment, start = 1, end = 17 }
                , { kind = Token.NewSiblingLine, start = 20, end = 20 }
                , { kind = non_mut_name "a", start = 20, end = 21 }
                , { kind = Token.Comment, start = 22, end = 33 }
                , { kind = Token.Defop { mutable = False }, start = 35, end = 36 }
                , { kind = Token.NumberLiteral "1", start = 37, end = 38 }
                , { kind = Token.NewSiblingLine, start = 40, end = 40 }
                , { kind = Token.Comment, start = 40, end = 59 }
                , { kind = Token.NewSiblingLine, start = 62, end = 62 }
                , { kind = Token.Comment, start = 62, end = 79 }
                ]
            )
        ]


underscores =
    Test.Group "Underscores"
        [ codeTest "'_' as a Name"
            "_"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo [ { kind = Token.Name Token.NameNoModifier "_", start = 0, end = 1 } ])
        , codeTest "'_10_20' as a Name"
            "_10_20"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo [ { kind = Token.Name Token.NameNoModifier "_10_20", start = 0, end = 6 } ])
        , codeTest "'10_20' as a Number"
            "10_20"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo [ { kind = Token.NumberLiteral "10_20", start = 0, end = 5 } ])
        ]
