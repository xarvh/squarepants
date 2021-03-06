

codeTest =
    as Text -> Text -> (Text -> Result Text ok) -> Test.CodeExpectation ok -> Test

    Test.codeTest Debug.toHuman


lexTokens s =
    as Text -> Result Text (List Token)

    s
        >> Compiler/GetTokens.lexer "Test"
        >> Result.mapError Debug.toHuman


tests =
    as Test

    Test.Group "StringToTokens"
        [
        , Test.Group "keywords"
            [
            , codeTest "[reg] `fn` is a keyword"
                "fn = 1"
                lexTokens
                (Test.isOkAndEqualTo
                    [
                    , { end = 0, kind = Token.NewSiblingLine, start = 0 }
                    , { end = 2, kind = Token.Fn, start = 0 }
                    , { end = 4, kind = Token.Defop { mutable = False }, start = 3 }
                    , { end = 6, kind = Token.NumberLiteral "1", start = 5 }
                    ]
                )
            ]
        ]
