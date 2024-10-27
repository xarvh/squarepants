valueTest as fn Text, (fn None: a), Test.CodeExpectation a: Test =
    Test.valueTest toHuman __ __ __


#valueTest as fn Text, (fn None: Format.Block), Test.CodeExpectation [Text]: Test =
#    fn title, makeBlock, exp:
#    valueTest_ title (fn None: makeBlock None >> Format.render >> Text.split "\n" __) exp

format1 as fn Bool, Bool, Format.Block, [ Format.Block ]: [ Text ] =
    fn breakFirst, breakRest, f, arg0 :: args:
    Format.spaceSeparatedOrIndentForce breakRest (Format.spaceSeparatedOrIndentForce breakFirst [ f, arg0 ] :: args)
    >> Format.render
    >> Text.split "\n" __
    >> List.filter __ (__ /= "")


format2 as fn Bool, [ Format.Block ]: [ Text ] =
    fn break, first :: rest:
    formatEntry =
        fn open, block:
        Format.prefix 2 (Format.'row (Format.'text_ open) Format.space) block

    Format.spaceSeparatedOrStackForce
        break
        [
        , Format.rowOrStackForce break 'nothing (formatEntry "[" first :: List.map rest (formatEntry "," __))
        , Format.textToBlock "]"
        ]
    >> Format.render
    >> Text.split "\n" __
    >> List.filter __ (__ /= "")


format3 as fn [ Format.Block & Bool & Format.Block ]: [ Text ] =
    fn first :: rest:
    formatEntry =
        fn open, key & break & value:
        Format.spaceSeparatedOrIndentForce
            break
            [
            , Format.spaceSeparatedOrStack [ Format.textToBlock open, key, Format.textToBlock "=" ]
            , value
            ]

    Format.stack
        (List.concat
             [
             , [ formatEntry "{" first ]
             , List.map rest (formatEntry "," __)
             , [ Format.textToBlock "}" ]
             ]
        )
    >> Format.render
    >> Text.split "\n" __
    >> List.filter __ (__ /= "")


format4 as fn Bool, Bool, Format.Block & Format.Block, Format.Block: [ Text ] =
    fn breakCond, breakBodies, ifCond & ifBody, elseBody:
    Format.spaceSeparatedOrStack
        [
        , Format.spaceSeparatedOrIndentForce
            breakBodies
            [
            , Format.rowOrStack
                'nothing
                [
                , Format.rowOrIndentForce
                    breakCond
                    'nothing
                    [
                    , Format.textToBlock "if ("
                    , ifCond
                    ]
                , Format.textToBlock ") {"
                ]
            , ifBody
            ]
        , Format.spaceSeparatedOrIndentForce
            (breakBodies or breakCond)
            [
            , Format.textToBlock << "} else {"
            , elseBody
            ]
        , Format.textToBlock "}"
        ]
    >> Format.render
    >> Text.split "\n" __
    >> List.filter __ (__ /= "")


tests as Test =
    Test.'group
        "Block examples"
        [
        , Test.'group
            "Elm-like examples"
            [
            , Test.'group
                "function application"
                [
                , valueTest
                    """
                    Formats on a single line
                    """
                    (fn 'none:
                         format1 'false 'false (Format.textToBlock "f") (List.map [ "a", "b" ] Format.textToBlock)
                    )
                    (Test.isOkAndEqualTo [ "f a b" ])
                , valueTest
                    "formats with all arguments split"
                    (fn 'none: format1 'true 'false (Format.textToBlock "f") (List.map [ "a", "b" ] Format.textToBlock))
                    (Test.isOkAndEqualTo
                         [
                         , "f"
                         , "    a"
                         , "    b"
                         ]
                    )
                , valueTest
                    "formats with first argument joined"
                    (fn 'none: format1 'false 'true (Format.textToBlock "f") (List.map [ "a", "b" ] Format.textToBlock))
                    (Test.isOkAndEqualTo
                         [
                         , "f a"
                         , "    b"
                         ]
                    )
                ]
            , Test.'group
                "list"
                [
                , valueTest "formats single-line" (fn 'none: format2 'false (List.map [ "a", "b", "c" ] Format.textToBlock)) (Test.isOkAndEqualTo [ "[ a, b, c ]" ])
                , valueTest
                    "formats multiline"
                    (fn 'none: format2 'true (List.map [ "a", "b", "c" ] Format.textToBlock))
                    (Test.isOkAndEqualTo
                         [
                         , "[ a"
                         , ", b"
                         , ", c"
                         , "]"
                         ]
                    )
                ]
            , Test.'group
                "record"
                [
                , valueTest
                    "formats single-line entries"
                    (fn 'none:
                         format3
                             [
                             , Format.textToBlock "a" & 'false & Format.textToBlock "1"
                             , Format.textToBlock "b" & 'false & Format.textToBlock "2"
                             ]
                    )
                    (Test.isOkAndEqualTo
                         [
                         , "{ a = 1"
                         , ", b = 2"
                         , "}"
                         ]
                    )
                , valueTest
                    "formats multiline entries"
                    (fn 'none:
                         format3
                             [
                             , Format.textToBlock "a" & 'true & Format.textToBlock "1"
                             , Format.textToBlock "b" & 'false & Format.textToBlock "2"
                             ]
                    )
                    (Test.isOkAndEqualTo
                         [
                         , "{ a ="
                         , "    1"
                         , ", b = 2"
                         , "}"
                         ]
                    )
                ]
            ]
        , Test.'group
            "Javascript-like examples"
            [
            , Test.'group
                "if-else"
                [
                , valueTest "formats single-line" (fn 'none: format4 'false 'false (Format.textToBlock "p" & Format.textToBlock "a") (Format.textToBlock "b")) (Test.isOkAndEqualTo [ "if (p) { a } else { b }" ])
                , valueTest
                    "formats multiline"
                    (fn 'none: format4 'false 'true (Format.textToBlock "p" & Format.textToBlock "a") (Format.textToBlock "b"))
                    (Test.isOkAndEqualTo
                         [
                         , "if (p) {"
                         , "    a"
                         , "} else {"
                         , "    b"
                         , "}"
                         ]
                    )
                , valueTest
                    "formats multiline condition"
                    (fn 'none: format4 'true 'false (Format.textToBlock "p" & Format.textToBlock "a") (Format.textToBlock "b"))
                    (Test.isOkAndEqualTo
                         [
                         , "if ("
                         , "    p"
                         , ") {"
                         , "    a"
                         , "} else {"
                         , "    b"
                         , "}"
                         ]
                    )
                ]
            ]
        ]
