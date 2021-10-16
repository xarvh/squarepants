module Compiler.StringToTokens_Test exposing (..)

import Compiler.StringToTokens as Lexer
import Compiler.TestHelpers
import Prelude
import Test exposing (Test)
import Types.Error exposing (Error)
import Types.Token as Token exposing (Token)


codeTest =
    Test.codeTest Debug.toString


lexTokens : String -> Result String (List Token)
lexTokens s =
    Lexer.lexer "Test" s
        |> Compiler.TestHelpers.resErrorToString s


non_mut_name =
    Token.Name { mutable = False }


tests : Test
tests =
    Test.Group "StringToTokens"
        [ keywords
        , unaryAddittiveOps
        , indentation
        , comments
        , underscores
        ]


keywords : Test
keywords =
    Test.Group "keywords"
        [ codeTest "[reg] `fn` is a keyword"
            "fn = 1"
            lexTokens
            (Test.okEqual
                [ { end = 0, kind = Token.NewSiblingLine, start = 0 }
                , { end = 2, kind = Token.Fn, start = 0 }
                , { end = 4, kind = Token.Defop { mutable = False }, start = 3 }
                , { end = 6, kind = Token.NumberLiteral "1", start = 5 }
                ]
            )
        ]


unaryAddittiveOps : Test
unaryAddittiveOps =
    Test.Group "Unary addittive ops"
        [ codeTest "-a"
            " -a"
            lexTokens
            (Test.okEqual
                [ { kind = Token.BlockStart, start = 0, end = 1 }
                , { kind = Token.Unop Prelude.unaryMinus, start = 1, end = 2 }
                , { kind = non_mut_name "a", start = 2, end = 3 }
                , { kind = Token.BlockEnd, start = 3, end = 3 }
                ]
            )
        , codeTest "a - -a"
            "a - -a"
            lexTokens
            (Test.okEqual
                [ { kind = Token.NewSiblingLine, start = 0, end = 0 }
                , { kind = non_mut_name "a", start = 0, end = 1 }
                , { kind = Token.Binop " -" Prelude.subtract, start = 2, end = 3 }
                , { kind = Token.Unop Prelude.unaryMinus, start = 4, end = 5 }
                , { kind = non_mut_name "a", start = 5, end = 6 }
                ]
            )
        , codeTest "a-a"
            "a-a"
            lexTokens
            (Test.okEqual
                [ { kind = Token.NewSiblingLine, start = 0, end = 0 }
                , { kind = non_mut_name "a", start = 0, end = 1 }
                , { kind = Token.Binop "-" Prelude.subtract, start = 1, end = 2 }
                , { kind = non_mut_name "a", start = 2, end = 3 }
                ]
            )
        , codeTest "Arrow ->"
            "->"
            lexTokens
            (Test.okEqual
                [ { kind = Token.NewSiblingLine, start = 0, end = 0 }
                , { kind = Token.Arrow { mutable = False }, start = 0, end = 2 }
                ]
            )
        , codeTest "-="
            "-="
            lexTokens
            (Test.okEqual
                [ { kind = Token.NewSiblingLine, start = 0, end = 0 }
                , { kind = Token.Binop "-=" Prelude.mutableSubtract, start = 0, end = 2 }
                ]
            )
            |> Test.NotNow
        ]


indentation : Test
indentation =
    Test.Group "Blocks, sibling lines, indentation"
        [ codeTest "1"
            """
a =
 1
b = 1
"""
            lexTokens
            (Test.okEqual
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



----
--- Comments
--


comments : Test
comments =
    Test.Group "Comments"
        [ codeTest "[reg] statement after comment"
            """
#
a = 1
                """
            lexTokens
            (Test.okEqual
                [ { kind = Token.Comment, start = 1, end = 2 }
                , { kind = Token.NewSiblingLine, start = 3, end = 3 }
                , { kind = non_mut_name "a", start = 3, end = 4 }
                , { kind = Token.Defop { mutable = False }, start = 5, end = 6 }
                , { kind = Token.NumberLiteral "1", start = 7, end = 8 }
                ]
            )
        , codeTest "[reg] nested comments allow a spurious newline?"
            """
[#[##]#]
a = 1
            """
            lexTokens
            (Test.okEqual
                [ { kind = Token.Comment, start = 0, end = 7 }
                ]
            )
        , codeTest "Single line"
            "# hello"
            lexTokens
            (Test.okEqual
                [ { kind = Token.Comment, start = 0, end = 7 }
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
            (Test.okEqual
                [ { kind = Token.Comment, start = 1, end = 18 }
                , { kind = Token.NewSiblingLine, start = 20, end = 20 }
                , { kind = non_mut_name "a", start = 20, end = 21 }
                , { kind = Token.Comment, start = 22, end = 34 }
                , { kind = Token.Defop { mutable = False }, start = 35, end = 36 }
                , { kind = Token.NumberLiteral "1", start = 37, end = 38 }
                , { kind = Token.Comment, start = 40, end = 60 }
                , { kind = Token.Comment, start = 62, end = 79 }
                ]
            )
        ]


underscores : Test
underscores =
    Test.Group "Underscores"
        [ codeTest "'_' is a Name"
            "_"
            (lexTokens >> Result.map (List.drop 1))
            (Test.okEqual [ { kind = Token.Name { mutable = False } "_", start = 0, end = 1 } ])
        , codeTest "'_10_20' is a Name"
            "_10_20"
            (lexTokens >> Result.map (List.drop 1))
            (Test.okEqual [ { kind = Token.Name { mutable = False } "_10_20", start = 0, end = 6 } ])
        , codeTest "'10_20' is a Number"
            "10_20"
            (lexTokens >> Result.map (List.drop 1))
            (Test.okEqual [ { kind = Token.NumberLiteral "10_20", start = 0, end = 5 } ])
        ]
