valueTest as fn Text, (fn None: a), Test.CodeExpectation a: Test =
    Test.valueTest toHuman __ __ __


#valueTest as fn Text, (fn None: Fmt.Block), Test.CodeExpectation [Text]: Test =
#    fn title, makeBlock, exp:
#    valueTest_ title (fn None: makeBlock None >> Fmt.render >> Text.split "\n" __) exp

format1 as fn Bool, Bool, Fmt.Block, [ Fmt.Block ]: [ Text ] =
    fn breakFirst, breakRest, f, arg0 :: args:
    Fmt.spaceSeparatedOrIndentForce breakRest (Fmt.spaceSeparatedOrIndentForce breakFirst [ f, arg0 ] :: args)
    >> Fmt.render
    >> Text.split "\n" __
    >> List.filter __ (__ /= "")


format2 as fn Bool, [ Fmt.Block ]: [ Text ] =
    fn break, first :: rest:
    formatEntry =
        fn open, block:
        Fmt.prefix 2 (Fmt.'row (Fmt.'text_ open) Fmt.space) block

    Fmt.spaceSeparatedOrStackForce
        break
        [
        , Fmt.rowOrStackForce break 'nothing (formatEntry "[" first :: List.map rest (formatEntry "," __))
        , Fmt.textToBlock "]"
        ]
    >> Fmt.render
    >> Text.split "\n" __
    >> List.filter __ (__ /= "")


format3 as fn [ Fmt.Block & Bool & Fmt.Block ]: [ Text ] =
    fn first :: rest:
    formatEntry =
        fn open, key & break & value:
        Fmt.spaceSeparatedOrIndentForce
            break
            [
            , Fmt.spaceSeparatedOrStack [ Fmt.textToBlock open, key, Fmt.textToBlock "=" ]
            , value
            ]

    Fmt.stack
        (List.concat
             [
             , [ formatEntry "{" first ]
             , List.map rest (formatEntry "," __)
             , [ Fmt.textToBlock "}" ]
             ]
        )
    >> Fmt.render
    >> Text.split "\n" __
    >> List.filter __ (__ /= "")


format4 as fn Bool, Bool, Fmt.Block & Fmt.Block, Fmt.Block: [ Text ] =
    fn breakCond, breakBodies, ifCond & ifBody, elseBody:
    Fmt.spaceSeparatedOrStack
        [
        , Fmt.spaceSeparatedOrIndentForce
            breakBodies
            [
            , Fmt.rowOrStack
                'nothing
                [
                , Fmt.rowOrIndentForce
                    breakCond
                    'nothing
                    [
                    , Fmt.textToBlock "if ("
                    , ifCond
                    ]
                , Fmt.textToBlock ") {"
                ]
            , ifBody
            ]
        , Fmt.spaceSeparatedOrIndentForce
            (breakBodies or breakCond)
            [
            , Fmt.textToBlock << "} else {"
            , elseBody
            ]
        , Fmt.textToBlock "}"
        ]
    >> Fmt.render
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
                         format1 'false 'false (Fmt.textToBlock "f") (List.map [ "a", "b" ] Fmt.textToBlock)
                    )
                    (Test.isOkAndEqualTo [ "f a b" ])
                , valueTest
                    "formats with all arguments split"
                    (fn 'none: format1 'true 'false (Fmt.textToBlock "f") (List.map [ "a", "b" ] Fmt.textToBlock))
                    (Test.isOkAndEqualTo
                         [
                         , "f"
                         , "    a"
                         , "    b"
                         ]
                    )
                , valueTest
                    "formats with first argument joined"
                    (fn 'none: format1 'false 'true (Fmt.textToBlock "f") (List.map [ "a", "b" ] Fmt.textToBlock))
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
                , valueTest "formats single-line" (fn 'none: format2 'false (List.map [ "a", "b", "c" ] Fmt.textToBlock)) (Test.isOkAndEqualTo [ "[ a, b, c ]" ])
                , valueTest
                    "formats multiline"
                    (fn 'none: format2 'true (List.map [ "a", "b", "c" ] Fmt.textToBlock))
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
                             , Fmt.textToBlock "a" & 'false & Fmt.textToBlock "1"
                             , Fmt.textToBlock "b" & 'false & Fmt.textToBlock "2"
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
                             , Fmt.textToBlock "a" & 'true & Fmt.textToBlock "1"
                             , Fmt.textToBlock "b" & 'false & Fmt.textToBlock "2"
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
                , valueTest "formats single-line" (fn 'none: format4 'false 'false (Fmt.textToBlock "p" & Fmt.textToBlock "a") (Fmt.textToBlock "b")) (Test.isOkAndEqualTo [ "if (p) { a } else { b }" ])
                , valueTest
                    "formats multiline"
                    (fn 'none: format4 'false 'true (Fmt.textToBlock "p" & Fmt.textToBlock "a") (Fmt.textToBlock "b"))
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
                    (fn 'none: format4 'true 'false (Fmt.textToBlock "p" & Fmt.textToBlock "a") (Fmt.textToBlock "b"))
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
