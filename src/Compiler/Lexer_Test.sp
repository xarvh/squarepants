tests as Test =
    Test.Group "Lexer"
        [
        , keywords
        , ops
        , unaryAddittiveOps
        , indentation
        , comments
        , underscores
        , position
        , textLiterals
        , numberLiterals
        ]


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


n = []

codeTest as fn Text, Text, (fn Text: Result Text ok), Test.CodeExpectation ok: Test =
    Test.codeTest toHuman __ __ __ __


valueTest as fn Text, (fn None: a), Test.CodeExpectation a: Test =
    Test.valueTest toHuman __ __ __


lexTokens as fn Text: Result Text [[Token]] =
    fn s:
    s
    >> TH.errorModule
    >> Compiler/Lexer.lexer
    >> TH.resErrorToStrippedText


lexTokensAndDrop as fn Int: fn Text: Result Text [[Token]] =
    fn name: fn s:
    s
    >> lexTokens
    >> Result.map (List.map (List.drop name __) __ ) __


lowerName as fn Text: Token.Kind =
    fn name:
    {
    , modifier = Token.NameNoModifier
    , isUpper = False
    , maybeModule = Nothing
    , name
    , attrPath = []
    }
    >> Token.Word



upperName as fn Text: Token.Kind =
    fn name:
    {
    , modifier = Token.NameNoModifier
    , isUpper = True
    , maybeModule = Nothing
    , name
    , attrPath = []
    }
    >> Token.Word


#
#
# Tests
#
#


keywords as Test =
    Test.Group "keywords"
        [
#        , codeTest
#            "[reg] can't @ keywords"
#            "@with"
#            lexTokens
#            (Test.errorContains ["keyword"] )
        ]


ops as Test =
    Test.Group "Operators"
        [
#        , codeTest "[reg] .. set Default"
#            ".. []"
#            lexTokens
#            (Test.isOkAndEqualTo
#                [
#                , Token 0 0 << Token.NewSiblingLine
#                , Token 0 2 << Token.Binop Prelude.textConcat
#                , Token 3 4 << Token.SquareBracket Token.Open
#                , Token 4 5 << Token.SquareBracket Token.Closed
#                ]
#            )
        ]


unaryAddittiveOps as Test =
    Test.Group "Unary addittive ops"
        [ codeTest "-a"
            "-a"
            lexTokens
            (Test.isOkAndEqualTo
                [[
                , Token 0 0 __ << Token.NewSiblingLine
                , Token 0 1 __ << Token.Unop Op.UnopMinus
                , Token 1 2 __ << lowerName "a"
                ]]
            )
        , codeTest "a - -a"
            "a - -a"
            lexTokens
            (Test.isOkAndEqualTo
                [[
                , Token 0 0 __ << Token.NewSiblingLine
                , Token 0 1 __ << lowerName "a"
                , Token 2 3 __ << Token.Binop Prelude.subtract
                , Token 4 5 __ << Token.Unop Op.UnopMinus
                , Token 5 6 __ << lowerName "a"
                ]]
            )
        , codeTest "a-a"
            "a-a"
            lexTokens
            (Test.isOkAndEqualTo
                [[
                , Token 0 0 __ << Token.NewSiblingLine
                , Token 0 1 __ << lowerName "a"
                , Token 1 2 __ << Token.Unop Op.UnopMinus
                , Token 2 3 __ << lowerName "a"
                ]]
            )
#        , codeTest "Consuming colon:"
#            ":-"
#            lexTokens
#            (Test.isOkAndEqualTo
#                [[ Token 0 0 Token.NewSiblingLine
#                , Token 0 2 Token.ConsumingColon
#                ]]
#            )
        , codeTest "-="
            "-="
            lexTokens
            (Test.isOkAndEqualTo
                [[
                , Token 0 0 Token.NewSiblingLine
                , Token 0 2 __ << Token.Binop Prelude.mutableSubtract
                ]]
            )
        ]


indentation as Test =
    Test.Group "Blocks, sibling lines, indentation"
        [ codeTest "1"
            "\na =\n 1\nb = 1"
            lexTokens
            (Test.isOkAndEqualTo
                [[
                , Token 1 1 __ << Token.NewSiblingLine
                , Token 1 2 __ << lowerName "a"
                , Token 3 4 __ << Token.Defop
                , Token 6 6 __ << Token.BlockStart
                , Token 6 7 __ << Token.NumberLiteral False "1"
                , Token 8 8 __ << Token.BlockEnd
                ], [
                , Token 8 8 __ << Token.NewSiblingLine
                , Token 8 9 __ << lowerName "b"
                , Token 10 11 __ << Token.Defop
                , Token 12 13 __ << Token.NumberLiteral False "1"
                ]]
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
                [[
                , Token 0  0  __ << Token.NewSiblingLine
                , Token 0  6  __ << lowerName "module"
                , Token 7  8  __ << Token.Defop
                , Token 12 12 __ << Token.BlockStart
                , Token 12 20 __ << lowerName "importAs"
                , Token 21 22 __ << Token.Defop
                , Token 29 29 __ << Token.BlockStart
                , Token 29 35 __ << upperName "SPCore"
                , Token 39 39 __ << Token.BlockEnd
                , Token 39 39 __ << Token.NewSiblingLine
                , Token 39 50 __ << lowerName "globalTypes"
                , Token 51 52 __ << Token.Defop
                , Token 59 59 __ << Token.BlockStart
                , Token 59 63 __ << upperName "None"
                , Token 63 63 __ << Token.BlockEnd
                , Token 63 63 __ << Token.BlockEnd
                ]]
            )
        , codeTest
            """
            Blocks and not
            """
            (Text.join "\n"
                [
                , "module ="
                , "   i ="
                , "        j"
                , "            >> k"
                , "            >> s"
                , ""
                , "   importAs ="
                , "      SPCore"
                , ""
                , "   globalTypes ="
                , "      None"
                , ""
                , "   a +     # no block start!"
                , "        b   # no sibling"
                , "        c"
                , ""
                , "   d =     # block start"
                , "        e   # sibling!"
                , "        f"
                , ""
                , "   g = h"
                ]
            )
            lexTokens
            (Test.isOkAndEqualTo
                [[
                , Token (0) (0)  Token.NewSiblingLine
                , Token (0) (6) (lowerName "module")
                , Token (7) (8) Token.Defop
                , Token (12) (12) (Token.BlockStart )
                , Token (12) (13) (lowerName "i")
                , Token (14) (15) (Token.Defop)
                , Token (24) (24) (Token.BlockStart )
                , Token (24) (25) (lowerName "j")
                , Token (38) (40) (Token.Binop Prelude.sendRight)
                , Token (41) (42) (lowerName "k")
                , Token (55) (57) (Token.Binop Prelude.sendRight)
                , Token (58) (59) (lowerName "s")
                , Token (64) (64) (Token.BlockEnd )
                , Token (64) (64) (Token.NewSiblingLine )
                , Token (64) (72) (lowerName "importAs")
                , Token (73) (74) (Token.Defop)
                , Token (81) (81) (Token.BlockStart )
                , Token (81) (87) (upperName "SPCore")
                , Token (92) (92) (Token.BlockEnd )
                , Token (92) (92) (Token.NewSiblingLine )
                , Token (92) (103) (lowerName "globalTypes")
                , Token (104) (105) (Token.Defop )
                , Token (112) (112) (Token.BlockStart )
                , Token (112) (116) (upperName ("None"))
                , Token (121) (121) (Token.BlockEnd )
                , Token (121) (121) (Token.NewSiblingLine )
                , Token (121) (122) (lowerName "a")
                , Token (123) (124) (Token.Binop Prelude.add)
                , Token (129) (146) (Token.Comment " no block start!")
                , Token (155) (156) (lowerName "b")
                , Token (159) (171) (Token.Comment " no sibling")
                , Token (180) (181) (lowerName "c")
                , Token (186) (186) (Token.NewSiblingLine )
                , Token (186) (187) (lowerName "d")
                , Token (188) (189) (Token.Defop )
                , Token (194) (207) (Token.Comment " block start")
                , Token (216) (216) (Token.BlockStart)
                , Token (216) (217) (lowerName "e")
                , Token (220) (230) (Token.Comment " sibling!")
                , Token (239) (239) (Token.NewSiblingLine )
                , Token (239) (240) (lowerName "f")
                , Token (245) (245) (Token.BlockEnd )
                , Token (245) (245) (Token.NewSiblingLine )
                , Token (245) (246) (lowerName "g")
                , Token (247) (248) (Token.Defop )
                , Token (249) (250) (lowerName "h")
                , Token (250) (250) (Token.BlockEnd )
                ]]
            )
        ]




comments as Test =
    Test.Group "Comments"
        [
        , codeTest "[reg] statement after comment"
            "\n#\na = 1\n"
            lexTokens
            (Test.isOkAndEqualTo
                [[
                , Token 3 3 __ << Token.NewSiblingLine
                , Token 3 4 __ << lowerName "a"
                , Token 5 6 __ << Token.Defop
                , Token 7 8 __ << Token.NumberLiteral False "1"
                ]]
            )
        , codeTest "[reg] nested comments allow a spurious newline?"
            "\n[#[##]#]\na = 1\n"
            lexTokens
            (Test.isOkAndEqualTo
                [[
                , Token 0 10 (Token.Comment "[##]")
                , Token 10 10 Token.NewSiblingLine
                , Token 10 11 __ << lowerName "a"
                , Token 12 13 __ << Token.Defop
                , Token 14 15 __ << Token.NumberLiteral False "1"
                ]]
            )
        , codeTest "Single line"
            "# hello"
            lexTokens
            (Test.isOkAndEqualTo
                [[
                , Token 0 7 (Token.Comment " hello")
                , Token 7 7 Token.NewSiblingLine
                ]]
            )
        , codeTest
            """
            Multi line
            """
            """
[# single line #]

a [# inline #] = 1

[#
    multi line
#]

[# [# nested #] #]
"""
            lexTokens
            (Test.isOkAndEqualTo [
                , [
                    , Token 0 19 (Token.Comment " single line ")
                    , Token 19 19 __ << Token.NewSiblingLine
                    , Token 19 20 __ << lowerName "a"
                    , Token 20 34 (Token.Comment " inline ")
                    , Token 34 35 __ << Token.Defop
                    , Token 36 37 __ << Token.NumberLiteral False "1"
                    ]
                , [
                  , Token 38 79 (Token.Comment "\n    multi line\n")
                  , Token 38 79 (Token.Comment " [# nested #] ")
                  , Token 79 79 Token.NewSiblingLine
                  ]
                ]
            )
        , codeTest
            "brackets"
            "[]"
            lexTokens
            (Test.isOkAndEqualTo
                [[
                , Token 0 0 __ << Token.NewSiblingLine
                , Token 0 1 __ << Token.SquareBracket Token.Open
                , Token 1 2 __ << Token.SquareBracket Token.Closed
                ]]
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
                [[
                , Token 0 0 __ << Token.NewSiblingLine
                , Token 0 8 __ << lowerName "allTests"
                , Token 9 10 __ << Token.Defop
                , Token 11 12 __ << Token.SquareBracket Token.Open
                , Token 17 18 __ << Token.Comma
                , Token 19 20 __ << lowerName "a"
                , Token 27 28 __ << Token.SquareBracket Token.Closed
                , Token 28 28 __ << Token.BlockEnd
                ]]
            )
        ]


underscores as Test =
    Test.Group "Underscores"
        [ codeTest "'_' as a Name"
            "_"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo [[ Token 0 1 __ << lowerName "_"]])
        , codeTest "'_10_20' as a Name"
            "_10_20"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo [[ Token 0 6 __ << lowerName "_10_20" ]])
        , codeTest "'10_20' as a Number"
            "10_20"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo [[ Token 0 5 __ << Token.NumberLiteral False "10_20" ]])
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
            (Test.isOkAndEqualTo [[
                , Token 0 0 __ << Token.NewSiblingLine
                , Token 0 2 __ << Token.TextLiteral ""
                ]]
            )
        , codeTest
            "Followed by colon"
            "\"n\":\n"
            lexTokens
            (Test.isOkAndEqualTo [[
                , Token 0 0 __ << Token.NewSiblingLine
                , Token 0 3 __ << Token.TextLiteral "n"
                , Token 3 4 __ << Token.Colon
                ]]
            )
#        , codeTest
#            "[reg] should not add the indent!"
#            """
#            try char as
#                , "":
#                    None
#
#                , "@"
#            """
#            (lexTokensAndDrop 11)
#            (Test.isOkAndEqualTo [[
#                , Token 38 41 << Token.TextLiteral "@"
#                , Token 41 41 << Token.BlockEnd
#                ]
#            )
        , valueTest
            """
            Unindent function
            """
            (fn _:
                [
                , "\n"
                , "  a\n"
                , "      \n"
                , "\n"
                , "  b\n"
                , "  "
                ]
                >> Text.join "" __
                >> Compiler/Lexer.unindent __
            )
            (Test.isOkAndEqualTo << Text.join ""
                [
                , "a\n"
                , "    \n"
                , "\n"
                , "b"
                ]
            )
        ]



numberLiterals as Test =
    Test.Group "Number literals"
        [
        , codeTest
            "Percent"
            "10%"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo [[
                , Token 0 2 __ << Token.NumberLiteral True "10"
                ]]
            )
        ]
