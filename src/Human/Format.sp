
#blah =
#    """ blah "a" """
#    >> formatLiteralText
#    >> Fmt.render
#    >> log __ "###"


formatExpression as fn FA.Expression: Fmt.Block =
    fn FA.Expression pos e_:
    try e_ as

        , FA.LiteralText text:
            formatLiteralText text

        , FA.LiteralNumber hasPercentage text:
            formatLiteralNumber hasPercentage text

        , FA.ArgumentPlaceholder:
            Fmt.textToBlock "__"

        , FA.Statements stats:
            formatStatements stats

        , FA.List unpacksAndExprs:
            formatList unpacksAndExprs

        , FA.Record { maybeExtension, attrs }:
            # TODO Missing info on whether we have `as` or `=`
            formatRecord maybeExtension attrs

        , FA.Variable { maybeType, word }:
            Fmt.textToBlock ""

        , FA.Fn pars body:
            Fmt.textToBlock ""

        , FA.Unop unopId:
            Fmt.textToBlock ""

        , FA.UnopCall unopId expr:
            Fmt.textToBlock ""

        , FA.Binop binop:
            Fmt.textToBlock ""

        , FA.BinopChain int binopChain:
            Fmt.textToBlock ""

        , FA.Call ref args:
            Fmt.textToBlock ""

        , FA.Poly text expression:
            Fmt.textToBlock ""

        , FA.If { condition, true, false }:
            Fmt.textToBlock ""

        , FA.Try { value, patterns }:
            Fmt.textToBlock ""




formatLiteralText as fn Text: Fmt.Block =

    singleQuote = "\""

    tripleQuote = Fmt.textToBlock "\"\"\""

    matchNewlinesOrQuotes =
        Text.startsWithRegex """.*([^\]["])|\n"""

    escape as fn Text: Text =
        # TODO
        identity

    fn text:

    if matchNewlinesOrQuotes text == "" then
        [#

            "text"

        #]
        singleQuote .. escape text .. singleQuote
        >> Fmt.textToBlock
    else
        [# TODO we need a `multiline` flag to distinguish between these two?

            """text"""

            """
            text
            """

        #]
      rows =
          text
          >> Text.split "\n" __
          >> List.map (fn x: x >> escape >> Fmt.textToBlock) __

      [ [tripleQuote], rows, [tripleQuote] ]
      >> List.concat
      >> Fmt.stack



formatLiteralNumber as fn Bool, Text: Fmt.Block =
    fn hasPercentage, numberAsText:

    # TODO break it in chunks of 3 around the decimal dot, intersperse with `_`
    if hasPercentage then
        Fmt.textToBlock (numberAsText .. "%")
    else
        Fmt.textToBlock numberAsText


formatStatements as fn [FA.Statements]: Fmt.Block =
    fn stats:
    stats
    >> List.map formatStatement __
    >> List.intersperse Fmt.blankLine __
    >> Fmt.stack


formatStatement as fn FA.Statements: Fmt.Block =
    fn stat:
    try stat as
        , FA.Evaluation expression: formatExpression expression
        , FA.ValueDef valueDef: formatValueDef valueDef
        , FA.AliasDef aliasDef: Fmt.textToBlock "TODO alias"
        , FA.UnionDef unionDef: Fmt.textToBlock "TODO union"


formatValueDef as fn FA.ValueDef: Fmt.Block =
    fn { pattern, nonFn, body }:

    Fmt.stack [
        , spaceSeparatedOrIndent [
            , formatPattern pattern
            , formatNonFn nonFn
            , Fmt.textToBlock "="
            ]
        , Fmt.indent (formatExpression body)
        ]


formatList as fn [Bool & FA.Expression]: Fmt.Block =
    fn unpacksAndExprs:

    formatListItem =
        fn isUnpacked & expr:

        if isUnpacked then
            expr
            >> formatExpression
            >> exprParensProtectSpaces
            >> Fmt.prefix 3 (Fmt.textToBlock "...") __
        else
            formatExpr expr

    unpacksAndExprs
    >> List.map formatListItem __
    >> groupWithBlankLines '[' ',' ']' True __


formatRecord as fn Maybe (Maybe FA.Expression), [FA.RecordAttribute]: Fmt.Block =
    fn maybeMaybeExt, attrs:

    open =
        try maybeMaybeExt as
            Nothing: Fmt.textToBlock "{"
            Just Nothing: Fmt.textToBlock "{ with"
            Just (Just ext):
                ext
                >> formatExpression
                >> Fmt.prefix 1 (Fmt.textToBlock "{") __
                >> Fmt.suffix 4 (Fmt.textToBlock "with") __

    formatRecordAttribute =
        fn { name, maybeExpr }:

        try maybeExpr as
            , Nothing: formatExpression name
            , Just expr: [
              , spaceSeparatedOrIndent [
                  , formatExpression name
                  , Fmt.textToBlock "="
                  ]
              , formatExpression expr
              ]
              >> rowOrIndent Nothing __

    attrs
    # TODO sort by attribute name
    >> List.map formatRecordAttribute __
    >> groupWithBlankLines open "," "}" True __








