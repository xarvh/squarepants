

codeTest as Text: Text: (Text: Result Text ok): Test.CodeExpectation ok: Test =
    Test.codeTest SPCore.toHuman


valueTest =
    Test.valueTest SPCore.toHuman


lexTokens as Text: Result Text (List Token) =
    s:
    s
        >> Compiler/Lexer.lexer TH.moduleName
        >> TH.resErrorToStrippedText s


lexTokensAndDrop as Int: Text: Result Text [Token] =
    n: s:
    s
        >> lexTokens
        >> Result.map (List.drop n)


non_mut_name as Text: Token.Kind =
    n:
    Token.LowerName Token.NameNoModifier Nothing n []


tests as Test =
    Test.Group "Lexer"
        [ keywords
        , ops
        , unaryAddittiveOps
        , indentation
        , comments
        , underscores
        , position
        , textLiterals
        ]


keywords as Test =
    Test.Group "keywords"
        [
        , codeTest
            "[reg] can't @ keywords"
            "@with"
            lexTokens
            (Test.errorContains ["keyword"] )
        ]


ops as Test =
    Test.Group "Operators"
        [
        , codeTest "[reg] .. set Default"
            ".. []"
            lexTokens
            (Test.isOkAndEqualTo
                [ Token 0 0 << Token.NewSiblingLine
                , Token 0 2 << Token.Binop Prelude.textConcat
                , Token 3 4 << Token.SquareBracket Token.Open
                , Token 4 5 << Token.SquareBracket Token.Closed
                ]
            )
        , codeTest
            "[reg] can't @ keywords"
            "@with"
            lexTokens
            (Test.errorContains ["keyword"] )
        ]


unaryAddittiveOps as Test =
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
#        , codeTest "-="
#            "-="
#            lexTokens
#            (Test.isOkAndEqualTo
#                [ Token 0 0 Token.NewSiblingLine
#                , Token 0 2 << Token.Binop Prelude.mutableSubtract
#                ]
#            )
        ]


indentation as Test =
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
        , codeTest
            """
            [reg] spurious spaces in front of field name
            """
            """
            module =
               importAs =
                  SPCore
               globalTypes =
                  None
            """
            lexTokens
            (Test.isOkAndEqualTo
                [
                , Token 0  0  << Token.NewSiblingLine
                , Token 0  6  << Token.LowerName Token.NameNoModifier Nothing "module" []
                , Token 7  8  << Token.Defop { mutable = False }
                , Token 9 12 << Token.BlockStart
                , Token 12 20 << Token.LowerName Token.NameNoModifier Nothing "importAs" []
                , Token 21 22 << Token.Defop { mutable = False }
                , Token 23 29 << Token.BlockStart
                , Token 29 35 << Token.UpperName Nothing "SPCore"
                , Token 39 39 << Token.BlockEnd
                , Token 39 39 << Token.NewSiblingLine
                , Token 39 50 << Token.LowerName Token.NameNoModifier Nothing "globalTypes" []
                , Token 51 52 << Token.Defop { mutable = False }
                , Token 53 59 << Token.BlockStart
                , Token 59 63 << Token.UpperName Nothing "None"
                , Token 63 63 << Token.BlockEnd
                , Token 63 63 << Token.BlockEnd
                ]
            )
        ]




comments as Test =
    Test.Group "Comments"
        [ codeTest "[reg] statement after comment"
            "\n#\na = 1\n"
            lexTokens
            (Test.isOkAndEqualTo
                [
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
                [
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
                [ Token 0 0 << Token.NewSiblingLine
                , Token 0 16 << Token.Comment
                , Token 19 19 << Token.NewSiblingLine
                , Token 19 20 << non_mut_name "a"
                , Token 21 32 << Token.Comment
                , Token 34 35 << Token.Defop { mutable = False }
                , Token 36 37 << Token.NumberLiteral "1"
                , Token 39 39 << Token.NewSiblingLine
                , Token 39 58 << Token.Comment
                , Token 61 61 << Token.NewSiblingLine
                , Token 61 78 << Token.Comment
                ]
            )
        , codeTest
            "brackets"
            "[]"
            lexTokens
            (Test.isOkAndEqualTo
                [
                , Token 0 0 << Token.NewSiblingLine
                , Token 0 1 << Token.SquareBracket Token.Open
                , Token 1 2 << Token.SquareBracket Token.Closed
                ]
            )

        , codeTest
            """
            [reg] Inline comments should not break a block
            """
            """
            allTests = [
                , a
            #
                ]
            """
            lexTokens
            (Test.isOkAndEqualTo
                [
                , Token 0 0 << Token.NewSiblingLine
                , Token 0 8 << Token.LowerName Token.NameNoModifier Nothing "allTests" []
                , Token 9 10 << Token.Defop { mutable = False }
                , Token 11 12 << Token.SquareBracket Token.Open
                , Token 13 17 << Token.BlockStart
                , Token 17 18 << Token.Comma
                , Token 19 20 << Token.LowerName Token.NameNoModifier Nothing "a" []
                , Token 21 22 << Token.Comment
                , Token 23 27 << Token.NewSiblingLine
                , Token 27 28 << Token.SquareBracket Token.Closed
                , Token 28 28 << Token.BlockEnd
                ]
            )
        ]


underscores as Test =
    Test.Group "Underscores"
        [ codeTest "'_' as a Name"
            "_"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo [ Token 0 1 << Token.LowerName Token.NameNoModifier Nothing "_" []])
        , codeTest "'_10_20' as a Name"
            "_10_20"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo [ Token 0 6 << Token.LowerName Token.NameNoModifier Nothing "_10_20" []])
        , codeTest "'10_20' as a Number"
            "10_20"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo [ Token 0 5 << Token.NumberLiteral "10_20" ])
        ]


position as Test =
    Test.Group "Position"
        [
        , codeTest
            "[reg] ops position"
            "blah <>"
            lexTokens
            (Test.errorContains ["blah <>"])
        , codeTest
            "[reg] ops position, with newline"
            "blah <>\n"
            lexTokens
            (Test.errorContains ["blah <>"])
        ]


textLiterals as Test =
    Test.Group "Text literals"
        [
        , codeTest
            "Empty Text"
            "\"\""
            lexTokens
            (Test.isOkAndEqualTo [
                , Token 0 0 << Token.NewSiblingLine
                , Token 0 2 << Token.TextLiteral ""
                ]
            )
        , codeTest
            "Followed by colon"
            "\"n\":\n"
            lexTokens
            (Test.isOkAndEqualTo [
                , Token 0 0 << Token.NewSiblingLine
                , Token 0 3 << Token.TextLiteral "n"
                , Token 3 4 << Token.Colon
                ]
            )
        , codeTest
            "[reg] should not add the indent!"
            """
            try char as
                "":
                    None

                "@"
            """
            (lexTokensAndDrop 11)
            (Test.isOkAndEqualTo [
                , Token 38 41 << Token.TextLiteral "@"
                , Token 41 41 << Token.BlockEnd
                ]
            )
#
# these do not parse
#
#        , valueTest
#            """
#            Unindent function
#            """
#            (
#              _:
#                [
#                , "\n"
#                , "  a\n"
#                , "      \n"
#                , "\n"
#                , "  b\n"
#                , "  "
#                ]
#                  >> Text.join ""
#                  >> Compiler/Lexer.unindent
#            )
#            (
#                [
#                , "a\n"
#                , "    \n"
#                , "\n"
#                , "b"
#                ]
#                    >> Text.join ""
#                    >> Test.isOkAndEqualTo
#            )
        ]
