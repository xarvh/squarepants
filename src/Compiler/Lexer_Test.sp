tests as Test =
    Test.'group
        "Lexer"
        [
        , names
        , ops
        , unaryAddittiveOps
        , indentation
        , comments
        , underscores
        , position
        , textLiterals
        , numberLiterals
        , recordLiterals
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

codeTest as fn Text, Text, fn Text: Result Text ok, Test.CodeExpectation ok: Test =
    Test.codeTest toHuman __ __ __ __


valueTest as fn Text, fn None: a, Test.CodeExpectation a: Test =
    Test.valueTest toHuman __ __ __


lexTokens as fn Text: Result Text [ [ Token ] ] =
    fn s:
    s
    >> TH.errorModule
    >> Compiler/Lexer.lexer 'true __
    >> TH.resErrorToStrippedText


lexTokensAndDrop as fn Int: fn Text: Result Text [ [ Token ] ] =
    fn name:
    fn s:
    s
    >> lexTokens
    >> Result.map (List.map (List.drop name __) __) __


lowerName as fn Text: Token.Kind =
    fn name:
    Token.'lowercase { attrPath = [], maybeModule = 'nothing, name }


upperName as fn Text: Token.Kind =
    fn name:
    Token.'uppercase { maybeModule = 'nothing, name }


#
#
# Tests
#
#

names as Test =
    Test.'group
        """
        Names
        """
        [
        , codeTest
            """
            [reg] Simple record access
            """
            "a.b"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo
                 [
                 , [
                 , 'token 0 3 __ << Token.'lowercase { attrPath = [ "b" ], maybeModule = 'nothing, name = "a" }
                 ]
                 ]
            )
        , codeTest
            """
            [reg] Nested record access
            """
            "a.b.c"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo
                 [
                 , [
                 , 'token 0 5 __ << Token.'lowercase { attrPath = [ "b", "c" ], maybeModule = 'nothing, name = "a" }
                 ]
                 ]
            )
        ]


ops as Test =
    Test.'group
        "Operators"
        [
        , codeTest
            "[reg] .. set Default"
            ".. []"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo
                 [
                 , [
                 , 'token 1 3 __ << Token.'binop 0 CoreDefs.textConcat
                 , 'token 3 4 __ << Token.'squareBracket 0 Token.'open
                 , 'token 4 5 __ << Token.'squareBracket 0 Token.'closed
                 ]
                 ]
            )
        ]


unaryAddittiveOps as Test =
    Test.'group
        "Unary addittive ops"
        [
        , codeTest
            "-a"
            "-a"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo
                 [
                 , [
                 , 'token 0 1 __ << Token.'unop Op.'unopMinus
                 , 'token 1 2 __ << lowerName "a"
                 ]
                 ]
            )
        , codeTest
            "a - -a"
            "a - -a"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo
                 [
                 , [
                 , 'token 0 1 __ << lowerName "a"
                 , 'token 2 3 __ << Token.'binop 0 CoreDefs.subtract
                 , 'token 4 5 __ << Token.'unop Op.'unopMinus
                 , 'token 5 6 __ << lowerName "a"
                 ]
                 ]
            )
        , codeTest
            "SKIP a-a"
            "a-a"
            lexTokens
            (Test.isOkAndEqualTo
                 [
                 , [
                 , 'token 0 0 __ << Token.'newSiblingLine
                 , 'token 0 1 __ << lowerName "a"
                 , 'token 1 2 __ << Token.'binop 1 CoreDefs.subtract
                 , 'token 2 3 __ << lowerName "a"
                 ]
                 ]
            )
        , codeTest
            "-="
            "-="
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo
                 [
                 , [
                 , 'token 0 2 __ << Token.'binop 0 CoreDefs.mutableSubtract
                 ]
                 ]
            )
        ]


indentation as Test =
    Test.'group
        "Blocks, sibling lines, indentation"
        [
        , codeTest
            "1"
            "\na =\n 1\nb = 1"
            lexTokens
            (Test.isOkAndEqualTo
                 [
                 , [
                 , 'token 1 1 __ << Token.'newSiblingLine
                 , 'token 1 2 __ << lowerName "a"
                 , 'token 3 4 __ << Token.'defop
                 , 'token 6 6 __ << Token.'blockStart
                 , 'token 6 7 __ << Token.'numberLiteral 'false "1"
                 , 'token 8 8 __ << Token.'blockEnd
                 ]
                 , [
                 , 'token 8 8 __ << Token.'newSiblingLine
                 , 'token 8 9 __ << lowerName "b"
                 , 'token 10 11 __ << Token.'defop
                 , 'token 12 13 __ << Token.'numberLiteral 'false "1"
                 ]
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
                 , [
                 , 'token 0 0 __ << Token.'newSiblingLine
                 , 'token 0 6 __ << lowerName "module"
                 , 'token 7 8 __ << Token.'defop
                 , 'token 12 12 __ << Token.'blockStart
                 , 'token 12 20 __ << lowerName "importAs"
                 , 'token 21 22 __ << Token.'defop
                 , 'token 29 29 __ << Token.'blockStart
                 , 'token 29 35 __ << upperName "SPCore"
                 , 'token 39 39 __ << Token.'blockEnd
                 , 'token 39 39 __ << Token.'newSiblingLine
                 , 'token 39 50 __ << lowerName "globalTypes"
                 , 'token 51 52 __ << Token.'defop
                 , 'token 59 59 __ << Token.'blockStart
                 , 'token 59 63 __ << upperName "None"
                 , 'token 63 63 __ << Token.'blockEnd
                 , 'token 63 63 __ << Token.'blockEnd
                 ]
                 ]
            )
        ]


[#
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
        , Token (38) (40) (Token.Binop CoreDefs.sendRight)
        , Token (41) (42) (lowerName "k")
        , Token (55) (57) (Token.Binop CoreDefs.sendRight)
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
        , Token (123) (124) (Token.Binop CoreDefs.add)
        , Token [ fl " no block start!"] (155) (156) (lowerName "b")
        , Token [ fl " no sibling"] (180) (181) (lowerName "c")
        , Token (186) (186) (Token.NewSiblingLine )
        , Token (186) (187) (lowerName "d")
        , Token (188) (189) (Token.Defop )
        , Token (216) (216) (Token.BlockStart )
        , Token fl " block start"] (216) (217) (lowerName "e")
        , Token (239) (239) (Token.NewSiblingLine )
        , Token [ fl " sibling!"] (239) (240) (lowerName "f")
        , Token (245) (245) (Token.BlockEnd )
        , Token (245) (245) (Token.NewSiblingLine )
        , Token (245) (246) (lowerName "g")
        , Token (247) (248) (Token.Defop )
        , Token (249) (250) (lowerName "h")
        , Token (250) (250) (Token.BlockEnd )
        ]]
    )
  #]
comments as Test =
    Test.'group
        "Comments"
        [
        , codeTest
            "[reg] statement after comment"
            "\n#\na = 1\n"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo
                 [
                 , []
                 , [
                 , 'token 3 4 __ << lowerName "a"
                 , 'token 5 6 __ << Token.'defop
                 , 'token 7 8 __ << Token.'numberLiteral 'false "1"
                 ]
                 ]
            )
        , codeTest
            "[reg] nested comments allow a spurious newline?"
            "\n[#[##]#]\na = 1\n"
            lexTokens
            (Test.isOkAndEqualTo
                 [
                 , [
                 , 'token 1 9 __ << Token.'comment { indent = 0, isBlock = 'true, isFollowedByBlank = 'false }
                 ]
                 , [
                 , 'token 10 10 Token.'newSiblingLine
                 , 'token 10 11 __ << lowerName "a"
                 , 'token 12 13 __ << Token.'defop
                 , 'token 14 15 __ << Token.'numberLiteral 'false "1"
                 ]
                 ]
            )
        , codeTest
            "Single line"
            "# hello"
            lexTokens
            (Test.isOkAndEqualTo
                 [
                 , [
                 , 'token 0 7 __ << Token.'comment { indent = 0, isBlock = 'false, isFollowedByBlank = 'false }
                 ]
                 ]
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
            (Test.isOkAndEqualTo
                 [
                 , [
                 , 'token 0 17 __ << Token.'comment { indent = 0, isBlock = 'true, isFollowedByBlank = 'true }
                 ]
                 , [
                 , 'token 19 19 __ << Token.'newSiblingLine
                 , 'token 19 20 __ << lowerName "a"
                 , 'token 21 33 __ << Token.'comment { indent = 2, isBlock = 'true, isFollowedByBlank = 'false }
                 , 'token 34 35 __ << Token.'defop
                 , 'token 36 37 __ << Token.'numberLiteral 'false "1"
                 , 'token 39 59 __ << Token.'comment { indent = 0, isBlock = 'true, isFollowedByBlank = 'true }
                 , 'token 61 79 __ << Token.'comment { indent = 0, isBlock = 'true, isFollowedByBlank = 'false }
                 ]
                 ]
            )
        , codeTest
            "brackets"
            "[]"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo
                 [
                 , [
                 , 'token 0 1 __ << Token.'squareBracket 0 Token.'open
                 , 'token 1 2 __ << Token.'squareBracket 0 Token.'closed
                 ]
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
                 , [
                 , 'token 0 0 __ << Token.'newSiblingLine
                 , 'token 0 8 __ << lowerName "allTests"
                 , 'token 9 10 __ << Token.'defop
                 , 'token 11 12 __ << Token.'squareBracket 0 Token.'open
                 , 'token 17 18 __ << Token.'comma
                 , 'token 19 20 __ << lowerName "a"
                 , 'token 21 22 __ << Token.'comment { indent = 0, isBlock = 'false, isFollowedByBlank = 'false }
                 , 'token 27 28 __ << Token.'squareBracket 3 Token.'closed
                 , 'token 28 28 __ << Token.'blockEnd
                 ]
                 ]
            )
        ]


underscores as Test =
    Test.'group
        "Underscores"
        [
        , codeTest "'_' as a Name" "_" (lexTokensAndDrop 1) (Test.isOkAndEqualTo [ [ 'token 0 1 __ << lowerName "_" ] ])
        , codeTest "'_10_20' as a Name" "_10_20" (lexTokensAndDrop 1) (Test.isOkAndEqualTo [ [ 'token 0 6 __ << lowerName "_10_20" ] ])
        , codeTest "'10_20' as a Number" "10_20" (lexTokensAndDrop 1) (Test.isOkAndEqualTo [ [ 'token 0 5 __ << Token.'numberLiteral 'false "10_20" ] ])
        ]


position as Test =
    Test.'group
        "Position"
        [
        , codeTest "[reg] ops position" "blah <>" lexTokens (Test.errorContains [ "blah <>" ])
        , codeTest "[reg] ops position, with newline" "blah <>\n" lexTokens (Test.errorContains [ "blah <>" ])
        ]


textLiterals as Test =
    Test.'group
        "Text literals"
        [
        , codeTest
            "Empty Text"
            "\"\""
            lexTokens
            (Test.isOkAndEqualTo
                 [
                 , [
                 , 'token 0 0 __ << Token.'newSiblingLine
                 , 'token 0 2 __ << Token.'textLiteral Token.'singleQuote ""
                 ]
                 ]
            )
        , codeTest
            "Followed by colon"
            "\"n\":\n"
            lexTokens
            (Test.isOkAndEqualTo
                 [
                 , [
                 , 'token 0 0 __ << Token.'newSiblingLine
                 , 'token 0 3 __ << Token.'textLiteral Token.'singleQuote "n"
                 , 'token 3 4 __ << Token.'colon
                 ]
                 ]
            )
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
            (Test.isOkAndEqualTo
             << Text.join
                 ""
                 [
                 , "a\n"
                 , "    \n"
                 , "\n"
                 , "b"
                 ]
            )
        ]


numberLiterals as Test =
    Test.'group
        "Number literals"
        [
        , codeTest
            "Percent"
            "10%"
            (lexTokensAndDrop 1)
            (Test.isOkAndEqualTo
                 [
                 , [
                 , 'token 0 2 __ << Token.'numberLiteral 'true "10"
                 ]
                 ]
            )
        ]


#]

recordLiterals as Test =
    Test.'group
        "Record literals"
        [
        , codeTest
            """
            [reg] .shorthand should work on its own line
            """
            """
            x =
              .b
            """
            (lexTokensAndDrop 3)
            (Test.isOkAndEqualTo
                 [
                 , [
                 , 'token 6 6 Token.'blockStart
                 , 'token 6 8 __ << Token.'recordShorthand { attrPath = [], name = "b" }
                 , 'token 8 8 Token.'blockEnd
                 ]
                 ]
            )
        ]
