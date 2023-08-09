
valueTest as fn Text, (fn None: a), Test.CodeExpectation a: Test =
    Test.valueTest toHuman __ __ __

#valueTest as fn Text, (fn None: Fmt.Block), Test.CodeExpectation [Text]: Test =
#    fn title, makeBlock, exp:
#    valueTest_ title (fn None: makeBlock None >> Fmt.render >> Text.split "\n" __) exp


format1 as fn Bool, Bool, Fmt.Block, [Fmt.Block]: [Text] =
    fn breakFirst, breakRest, f, (arg0 :: args):

    Fmt.spaceSeparatedOrIndentForce
        breakRest
        ( Fmt.spaceSeparatedOrIndentForce breakFirst [f, arg0] :: args)

    >> Fmt.render
    >> Text.split "\n" __
    >> List.filter (__ /= "") __


format2 as fn Bool, [Fmt.Block]: [Text] =
    fn break, (first :: rest):

    formatEntry =
      fn open, block:
      Fmt.prefix 2 (Fmt.Row (Fmt.Text_ open) (Fmt.space)) block

    Fmt.spaceSeparatedOrStackForce
      break
      [ Fmt.rowOrStackForce
          break
          Nothing
          ( formatEntry "[" first :: List.map (formatEntry "," __) rest)
      , Fmt.textToBlock ("]")
      ]
    >> Fmt.render
    >> Text.split "\n" __
    >> List.filter (__ /= "") __



format3 as fn [Fmt.Block & Bool & Fmt.Block]: [Text] =
    fn first :: rest:

    formatEntry =
        fn open, (key & break & value):
        Fmt.spaceSeparatedOrIndentForce
          break
          [ Fmt.spaceSeparatedOrStack [Fmt.textToBlock open, key, Fmt.textToBlock "="],
            value
          ]

    Fmt.stack
      (List.concat [
        [ formatEntry "{" first ]
        , List.map (formatEntry "," __) rest
        , [Fmt.textToBlock "}"]
        ])
    >> Fmt.render
    >> Text.split "\n" __
    >> List.filter (__ /= "") __



format4 as fn Bool, Bool, Fmt.Block & Fmt.Block, Fmt.Block: [Text] =
    fn breakCond, breakBodies, (ifCond & ifBody), elseBody:

    Fmt.spaceSeparatedOrStack
      [ Fmt.spaceSeparatedOrIndentForce
          breakBodies
          [ Fmt.rowOrStack
              Nothing
              [ Fmt.rowOrIndentForce
                  breakCond
                  Nothing
                  [ Fmt.textToBlock "if (",
                    ifCond
                  ]
              , Fmt.textToBlock ") {"
              ]
          , ifBody
          ],
        Fmt.spaceSeparatedOrIndentForce
          (breakBodies or breakCond)
          [ Fmt.textToBlock << "} else {",
            elseBody
          ],
        Fmt.textToBlock "}"
      ]
    >> Fmt.render
    >> Text.split "\n" __
    >> List.filter (__ /= "") __





tests as Test =
  Test.Group "Block examples" [
    , Test.Group "Elm-like examples" [
      , Test.Group "function application" [

        , valueTest
            """
            Formats on a single line
            """
            (fn None:
                format1 False False
                   (Fmt.textToBlock "f")
                   (List.map (Fmt.textToBlock) ["a", "b"])
            )
            (Test.isOkAndEqualTo ["f a b"])

        , valueTest "formats with all arguments split"
            (fn None: format1 True False (Fmt.textToBlock  "f") (List.map (Fmt.textToBlock) ["a", "b"]))
            (Test.isOkAndEqualTo [ "f",
                               "    a",
                               "    b"
                             ]
            )
        , valueTest "formats with first argument joined"
            (fn None: format1 False True (Fmt.textToBlock  "f") (List.map (Fmt.textToBlock) ["a", "b"]))
            (Test.isOkAndEqualTo [ "f a",
                               "    b"
                             ]
            )
        ]

      , Test.Group "list" [

        , valueTest "formats single-line"
          (fn None: format2 False (List.map (Fmt.textToBlock) ["a", "b", "c"]))
          (Test.isOkAndEqualTo ["[ a, b, c ]"])

        , valueTest "formats multiline"
          (fn None: format2 True (List.map (Fmt.textToBlock) ["a", "b", "c"]))
          (Test.isOkAndEqualTo [ "[ a",
                               ", b",
                               ", c",
                               "]"
                             ]
          )
        ]

      , Test.Group "record" [
        , valueTest "formats single-line entries"
          (fn None: format3
            [ (Fmt.textToBlock  "a" & False & Fmt.textToBlock  "1"),
              (Fmt.textToBlock  "b" & False & Fmt.textToBlock  "2")
            ]
          )
          (Test.isOkAndEqualTo [ "{ a = 1",
                               ", b = 2",
                               "}"
                             ]
          )
        , valueTest "formats multiline entries"
          (fn None: format3
            [ (Fmt.textToBlock  "a" & True & Fmt.textToBlock  "1"),
              (Fmt.textToBlock  "b" & False & Fmt.textToBlock  "2")
            ]
          )
          (Test.isOkAndEqualTo [ "{ a =",
                               "    1",
                               ", b = 2",
                               "}"
                             ]
          )
        ]
      ]

    , Test.Group "Javascript-like examples" [
      , Test.Group "if-else" [
        , valueTest "formats single-line"
          (fn None: format4 False False (Fmt.textToBlock  "p" & Fmt.textToBlock  "a") (Fmt.textToBlock  "b"))
          (Test.isOkAndEqualTo ["if (p) { a } else { b }"])
        , valueTest "formats multiline"
          (fn None: format4 False True (Fmt.textToBlock  "p" & Fmt.textToBlock  "a") (Fmt.textToBlock  "b"))
          (Test.isOkAndEqualTo [ "if (p) {",
                               "    a",
                               "} else {",
                               "    b",
                               "}"
                             ]
          )
        , valueTest "formats multiline condition"
          (fn None: format4 True False (Fmt.textToBlock  "p" & Fmt.textToBlock  "a") (Fmt.textToBlock  "b"))
          (Test.isOkAndEqualTo [ "if (",
                               "    p",
                               ") {",
                               "    a",
                               "} else {",
                               "    b",
                               "}"
                             ]
          )
        ]
      ]
    ]

