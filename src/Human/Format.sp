
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
            Fmt.textToBlock ""

        , FA.Record { maybeExtension, attrs }:
            Fmt.textToBlock ""

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
    List.map formatStatement stats
    >> Fmt.stack


formatStatement as fn FA.Statements: Fmt.Block =
    fn stat:
    try stat as

        , FA.Evaluation expression:
            formatExpression expression

        , FA.ValueDef valueDef:
            formatValueDef valueDef

        , FA.AliasDef aliasDef: Fmt.textToBlock "TODO"
        , FA.UnionDef unionDef: Fmt.textToBlock "TODO"


formatValueDef as fn FA.ValueDef: Fmt.Block =
    fn def:
    ...

