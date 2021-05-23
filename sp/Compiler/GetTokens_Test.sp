

codeTest =
    Test.codeTest (fn _: "TODO")


lexTokens s _ =
    as Text -> Text -> Result Error (List Token)

    Compiler/GetTokens.lexer s


tests =
    as Test

    Test.Group "StringToTokens"
        [
        , Test.Group "keywords"
            [
            , codeTest
                {
                , name = "[reg] `fn` is a keyword"
                , run = lexTokens "fn = 1"
                , expected =
                    Ok
                        [
                        , { end = 0, kind = Token.NewSiblingLine, start = 0 }
                        , { end = 2, kind = Token.Fn, start = 0 }
                        , { end = 4, kind = Token.Defop { mutable = False }, start = 3 }
                        , { end = 6, kind = Token.NumberLiteral "1", start = 5 }
                        ]
                }
            ]
        ]
