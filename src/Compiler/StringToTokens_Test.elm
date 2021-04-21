module Compiler.StringToTokens_Test exposing (..)

import Compiler.StringToTokens as Lexer
import Prelude
import Test exposing (Test)
import Types.Error exposing (Error)
import Types.Token as Token exposing (Token)


simpleTest =
    Test.simple Debug.toString


lexTokens : String -> String -> Result Error (List Token)
lexTokens s _ =
    Lexer.lexer s


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
        [ simpleTest
            { name = "[reg] `fn` is a keyword"
            , run = lexTokens "fn = 1"
            , expected =
                Ok
                    [ { end = 0, kind = Token.NewSiblingLine, start = 0 }
                    , { end = 2, kind = Token.Fn, start = 0 }
                    , { end = 4, kind = Token.Defop { mutable = False }, start = 3 }
                    , { end = 6, kind = Token.NumberLiteral "1", start = 5 }
                    ]
            }
        ]


unaryAddittiveOps : Test
unaryAddittiveOps =
    Test.Group "Unary addittive ops"
        [ simpleTest
            { name = "-a"
            , run = lexTokens " -a"
            , expected =
                Ok
                    [ { kind = Token.BlockStart, start = 0, end = 1 }
                    , { kind = Token.Unop "-", start = 1, end = 2 }
                    , { kind = non_mut_name "a", start = 2, end = 3 }
                    , { kind = Token.BlockEnd, start = 3, end = 3 }
                    ]
            }
        , simpleTest
            { name = "a - -a"
            , run = lexTokens "a - -a"
            , expected =
                Ok
                    [ { kind = Token.NewSiblingLine, start = 0, end = 0 }
                    , { kind = non_mut_name "a", start = 0, end = 1 }
                    , { kind = Token.Binop " -" Prelude.subtract, start = 2, end = 3 }
                    , { kind = Token.Unop "-", start = 4, end = 5 }
                    , { kind = non_mut_name "a", start = 5, end = 6 }
                    ]
            }
        , simpleTest
            { name = "a-a"
            , run = lexTokens "a-a"
            , expected =
                Ok
                    [ { kind = Token.NewSiblingLine, start = 0, end = 0 }
                    , { kind = non_mut_name "a", start = 0, end = 1 }
                    , { kind = Token.Binop "-" Prelude.subtract, start = 1, end = 2 }
                    , { kind = non_mut_name "a", start = 2, end = 3 }
                    ]
            }
        , simpleTest
            { name = "Arrow ->"
            , run = lexTokens "->"
            , expected =
                Ok
                    [ { kind = Token.NewSiblingLine, start = 0, end = 0 }
                    , { kind = Token.Arrow { mutable = False }, start = 0, end = 2 }
                    ]
            }
        , simpleTest
            { name = "-="
            , run = lexTokens "-="
            , expected =
                Ok
                    [ { kind = Token.NewSiblingLine, start = 0, end = 0 }
                    , { kind = Token.Binop "-=" Prelude.mutableSubtract, start = 0, end = 2 }
                    ]
            }
            |> Test.NotNow
        ]


indentation : Test
indentation =
    Test.Group "Blocks, sibling lines, indentation"
        [ simpleTest
            { name = "1"
            , run = lexTokens """
a =
 1
b = 1
"""
            , expected =
                Ok
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
            }
        ]



----
--- Comments
--


comments : Test
comments =
    Test.Group "Comments"
        [ simpleTest
            { name = "[reg] statement after comment"
            , run = lexTokens """
#
a = 1
                """
            , expected =
                Ok
                    [ { kind = Token.Comment, start = 1, end = 2 }
                    , { kind = Token.NewSiblingLine, start = 3, end = 3 }
                    , { kind = non_mut_name "a", start = 3, end = 4 }
                    , { kind = Token.Defop { mutable = False }, start = 5, end = 6 }
                    , { kind = Token.NumberLiteral "1", start = 7, end = 8 }
                    ]
            }
        , simpleTest
            { name = "Single line"
            , run = lexTokens "# hello"
            , expected =
                Ok
                    [ { kind = Token.Comment, start = 0, end = 7 }
                    ]
            }
        , simpleTest
            { name = "Multi line"
            , run = lexTokens """
[# single line #]

a [# inline #] = 1

[#
    multi line
#]

[# [# nested #] #]
"""
            , expected =
                Ok
                    [ { kind = Token.Comment, start = 1, end = 18 }
                    , { kind = Token.NewSiblingLine, start = 20, end = 20 }
                    , { kind = non_mut_name "a", start = 20, end = 21 }
                    , { kind = Token.Comment, start = 22, end = 34 }
                    , { kind = Token.Defop { mutable = False }, start = 35, end = 36 }
                    , { kind = Token.NumberLiteral "1", start = 37, end = 38 }
                    , { kind = Token.Comment, start = 40, end = 60 }
                    , { kind = Token.Comment, start = 62, end = 79 }
                    ]
            }
        ]


underscores : Test
underscores =
    Test.Group "Underscores"
        [ simpleTest
            { name = "'_' is a Name"
            , run = lexTokens "_" >> Result.map (List.drop 1)
            , expected = Ok [ { kind = Token.Name { mutable = False } "_", start = 0, end = 1 } ]
            }
        , simpleTest
            { name = "'_10_20' is a Name"
            , run = lexTokens "_10_20" >> Result.map (List.drop 1)
            , expected = Ok [ { kind = Token.Name { mutable = False } "_10_20", start = 0, end = 6 } ]
            }
        , simpleTest
            { name = "'10_20' is a Number"
            , run = lexTokens "10_20" >> Result.map (List.drop 1)
            , expected = Ok [ { kind = Token.NumberLiteral "10_20", start = 0, end = 5 } ]
            }
        ]
