



groupWithBlankLines as fn Fmt.Block, Text, Text, Bool, [Fmt.Block]: Fmt.Block =
    fn open, sep, close, bool, blocks:

    todo ""



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
            formatVariable maybeType word

        , FA.Fn pars body:
            formatFunction pars body

        , FA.Unop unopId:
            formatUnop unopId

        , FA.UnopCall unopId expr:
            formatUnopCall unopId expr

        , FA.Binop binop:
            Fmt.textToBlock binop.symbol

        , FA.BinopChain priority binopChain:
            formatBinopChain priority binopChain

        , FA.Call ref args:
            formatCall ref args

        , FA.Poly text expression:
            Fmt.textToBlock "TODO: Poly"

        , FA.If { condition, true, false }:
            formatIf condition true false

        , FA.Try { value, patterns }:
            Fmt.textToBlock ""




formatLiteralText as fn Text: Fmt.Block =

    singleQuote =
        "\""

    tripleQuote =
        Fmt.textToBlock "\"\"\""

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


formatStatements as fn [FA.Statement]: Fmt.Block =
    fn stats:
    stats
    >> List.map formatStatement __
    >> List.intersperse Fmt.blankLine __ []
    >> Fmt.stack


formatStatement as fn FA.Statement: Fmt.Block =
    fn stat:
    try stat as
        , FA.Evaluation expression: formatExpression expression
        , FA.ValueDef valueDef: formatValueDef valueDef
        , FA.AliasDef aliasDef: Fmt.textToBlock "TODO alias"
        , FA.UnionDef unionDef: Fmt.textToBlock "TODO union"


formatValueDef as fn FA.ValueDef: Fmt.Block =
    fn { pattern, nonFn, body }:

    Fmt.stack [
        , Fmt.spaceSeparatedOrIndent [
            , formatExpression pattern
            , formatNonFn nonFn
            , Fmt.textToBlock "="
            ]
        , Fmt.indent (formatExpression body)
        ]


formatNonFn as fn [At Token.Word]: Fmt.Block =
    fn atWords:
    todo "formatNonFn"


formatList as fn [Bool & FA.Expression]: Fmt.Block =
    fn unpacksAndExprs:

    formatListItem =
        fn isUnpacked & expr:

        if isUnpacked then
            expr
            >> formatExpression
            >> Fmt.prefix 3 (Fmt.Text_ "...") __
        else
            formatExpression expr

    unpacksAndExprs
    >> List.map formatListItem __
    >> groupWithBlankLines (Fmt.textToBlock "[") "," "]" True __


formatRecord as fn Maybe (Maybe FA.Expression), [FA.RecordAttribute]: Fmt.Block =
    fn maybeMaybeExt, attrs:

    open =
        try maybeMaybeExt as
            , Nothing: Fmt.textToBlock "{"
            , Just Nothing: Fmt.textToBlock "{ with"
            , Just (Just ext):
                ext
                >> formatExpression
                >> Fmt.prefix 1 (Fmt.Text_ "{") __
                >> Fmt.addSuffix (Fmt.Text_ "with") __

    formatRecordAttribute =
        fn { name, maybeExpr }:

        try maybeExpr as
            , Nothing: formatExpression name
            , Just expr: [
              , Fmt.spaceSeparatedOrIndent [
                  , formatExpression name
                  , Fmt.textToBlock "="
                  ]
              , formatExpression expr
              ]
              >> Fmt.rowOrIndent Nothing __

    attrs
    # TODO sort by attribute name
    >> List.map formatRecordAttribute __
    >> groupWithBlankLines open "," "}" True __


formatVariable as fn Maybe FA.Expression, Token.Word: Fmt.Block =
    fn maybeType, { modifier, isUpper, maybeModule, name, attrPath }:

    word as Fmt.Block =
        [
        , try modifier as
            , Token.NameNoModifier: []
            , Token.NameStartsWithDot: [ "." ]

        , try maybeModule as
            , Nothing: []
            , Just module: [ module, "." ]

        , [ name ]

        , List.map (fn p: "." .. p) attrPath
        ]
        >> List.concat
        >> Text.join "" __
        >> Fmt.textToBlock

    try maybeType as
        , Nothing:
            word

        , Just type:
            [
            , word
            , Fmt.textToBlock "as"
            , formatExpression type
            ]
            >> Fmt.rowOrIndent Nothing __


formatFunction as fn [FA.Expression], FA.Expression: Fmt.Block =
    fn pars, body:

    [
    , pars
      >> List.map formatExpression __
      >> groupWithBlankLines (Fmt.textToBlock "fn") "," ":" True __
    , formatExpression body
    ]
    >> Fmt.rowOrIndent Nothing __


unopToText as fn Op.UnopId: Text =
    fn unopId:
    try unopId as
        , Op.UnopPlus: "+"
        , Op.UnopMinus: "-"
        , Op.UnopUnique: "!"
        , Op.UnopRecycle: "@"


formatUnop as fn Op.UnopId: Fmt.Block =
    fn unopId:
    unopId >> unopToText >> Fmt.textToBlock


formatUnopCall as fn Op.UnopId, FA.Expression: Fmt.Block =
    fn unopId, expr:

    unop =
        unopToText unopId

    expr
    >> formatExpression
    >> Fmt.prefix (Text.length unop) (Fmt.Text_ unop) __


formatBinopChain as fn Int, FA.SepList Op.Binop FA.Expression: Fmt.Block =
    fn priority, left & opsAndRights:

    formatOpAndRight =
        fn binop & expr:
        [ Fmt.textToBlock binop.symbol, formatExpression expr ]

    formatExpression left :: List.concatMap formatOpAndRight opsAndRights
    >> Fmt.rowOrIndent Nothing __


formatCall as fn FA.Expression, [FA.Expression]: Fmt.Block =
    fn ref, args:

    ref :: args
    >> List.map formatExpression __
    >> Fmt.rowOrIndent Nothing __


formatIf as fn FA.Expression, FA.Expression, FA.Expression: Fmt.Block =
    fn condition, true, false:

    todo "if"


