

commaSeparatedList as fn Fmt.Block, Text, Bool, [Fmt.Block]: Fmt.Block =
    fn open, close, closeHasAPrecedingSpace, items:

    z =
        open :: items
        >> Fmt.maybeAllSingleLines
        >> Maybe.map (Tuple.mapFirst List.reverse __ ) __

    try z as

        , Just ([openLine, ...itemLines] & mkLine):
            [#
                $open item1, item2, item3 $close
            #]
            closeLine =
                if closeHasAPrecedingSpace then
                    Fmt.Row Fmt.Space (Fmt.Text_ close)
                else
                    Fmt.Text_ close
            itemLines
            >> List.intersperse (Fmt.Text_ ", ") __ []
            >> List.for (Fmt.Row openLine Fmt.Space) __ (fn a, b: Fmt.Row b a)
            >> mkLine
            >> Fmt.addSuffix closeLine __

        , Nothing:
            [#
                $open
                , item1
                , item2
                $close
            #]
            [
            , [ open ]
            , List.map (Fmt.prefix 2 (Fmt.Text_ ", ") __) items
            , [ Fmt.textToBlock close ]
            ]
            >> List.concat
            >> Fmt.stack



chainPrecedence as fn [Op.Binop & a]: Int =
    fn ls:
    try ls as
        , []: 0
        , [op & _, ..._]: op.precedence



parens as fn Fmt.Block: Fmt.Block =
    fn block:

    [
    , Fmt.prefix 1 (Fmt.Text_ "(") block
    , Fmt.textToBlock ")"
    ]
    >> Fmt.rowOrStack Nothing __



formatExpressionNoPrecedence as fn FA.Expression: Fmt.Block =
    fn x: formatExpression x >> Tuple.second


formatExpression as fn FA.Expression: Int & Fmt.Block =
    fn FA.Expression pos e_:

    noParensNeeded = 10

    try e_ as

        , FA.LiteralText text:
            noParensNeeded & formatLiteralText text

        , FA.LiteralNumber hasPercentage text:
            noParensNeeded & formatLiteralNumber hasPercentage text

        , FA.ArgumentPlaceholder:
            noParensNeeded & Fmt.textToBlock "__"

        , FA.Statements stats:
            0 & formatStatements stats

        , FA.List unpacksAndExprs:
            noParensNeeded & formatList unpacksAndExprs

        , FA.Record { maybeExtension, attrs }:
            # TODO Missing info on whether we have `as` or `=`
            noParensNeeded & formatRecord maybeExtension attrs

        , FA.Variable { maybeType, word }:
            noParensNeeded & formatVariable maybeType word

        , FA.Fn pars body:
            0 & formatFunction pars body

        , FA.Unop unopId:
            noParensNeeded & formatUnop unopId

        , FA.UnopCall unopId expr:
            noParensNeeded & formatUnopCall unopId expr

        , FA.Binop binop:
            noParensNeeded & Fmt.textToBlock binop.symbol

        , FA.BinopChain priority binopChain:
            chainPrecedence binopChain.second & formatBinopChain priority binopChain

        , FA.Call ref args:
            noParensNeeded & formatCall ref args

        , FA.Poly text expression:
            9 & Fmt.textToBlock "TODO: Poly"

        , FA.If { condition, true, false }:
            9 & formatIf condition true false

        , FA.Try { value, patterns }:
            9 & Fmt.textToBlock ""





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
        , FA.Evaluation expression: formatExpressionNoPrecedence expression
        , FA.ValueDef valueDef: formatValueDef valueDef
        , FA.AliasDef aliasDef: Fmt.textToBlock "TODO alias"
        , FA.UnionDef unionDef: Fmt.textToBlock "TODO union"


formatValueDef as fn FA.ValueDef: Fmt.Block =
    fn { pattern, nonFn, body }:

        [
        , [
            , [formatExpressionNoPrecedence pattern]
            , if nonFn == [] then [] else [ formatNonFn nonFn ]
            , [Fmt.textToBlock "="]
            ]
            >> List.concat
            >> Fmt.spaceSeparatedOrIndent
        , Fmt.indent (formatExpressionNoPrecedence body)
        ]
        >> Fmt.stack


formatNonFn as fn [At Token.Word]: Fmt.Block =
    fn atWords:

    atWords
    >> List.map (fn x: x >> Pos.drop >> formatWord) __
    >> commaSeparatedList (Fmt.textToBlock "with") "NonFunction" True __


formatList as fn [Bool & FA.Expression]: Fmt.Block =
    fn unpacksAndExprs:

    formatListItem =
        fn isUnpacked & expr:

        if isUnpacked then
            expr
            >> formatExpressionNoPrecedence
            >> Fmt.prefix 3 (Fmt.Text_ "...") __
        else
            formatExpressionNoPrecedence expr

    unpacksAndExprs
    >> List.map formatListItem __
    >> commaSeparatedList (Fmt.textToBlock "[") "]" True __


formatRecord as fn Maybe (Maybe FA.Expression), [FA.RecordAttribute]: Fmt.Block =
    fn maybeMaybeExt, attrs:

    open =
        try maybeMaybeExt as
            , Nothing: Fmt.textToBlock "{"
            , Just Nothing: Fmt.textToBlock "{ with"
            , Just (Just ext):
                ext
                >> formatExpressionNoPrecedence
                >> Fmt.prefix 1 (Fmt.Text_ "{") __
                >> Fmt.addSuffix (Fmt.Text_ "with") __

    formatRecordAttribute =
        fn { name, maybeExpr }:

        try maybeExpr as
            , Nothing: formatExpressionNoPrecedence name
            , Just expr: [
              , Fmt.spaceSeparatedOrIndent [
                  , formatExpressionNoPrecedence name
                  , Fmt.textToBlock "="
                  ]
              , formatExpressionNoPrecedence expr
              ]
              >> Fmt.rowOrIndent Nothing __

    attrs
    # TODO sort by attribute name
    >> List.map formatRecordAttribute __
    >> commaSeparatedList open "}" True __


formatWord as fn Token.Word: Fmt.Block =
    fn { modifier, isUpper, maybeModule, name, attrPath }:

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


formatVariable as fn Maybe FA.Expression, Token.Word: Fmt.Block =
    fn maybeType, word:

    try maybeType as
        , Nothing:
            formatWord word

        , Just type:
            [
            , Fmt.addSuffix (Fmt.Text_ " as") (formatWord word)
            , formatExpressionNoPrecedence type
            ]
            >> Fmt.spaceSeparatedOrIndent __


formatFunction as fn [FA.Expression], FA.Expression: Fmt.Block =
    fn pars, body:

    [
    , pars
      >> List.map formatExpressionNoPrecedence __
      >> commaSeparatedList (Fmt.textToBlock "fn") ":" False __
    , formatExpressionNoPrecedence body
    ]
    >> Fmt.rowOrStack Nothing __


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
    >> formatExpressionNoPrecedence
    >> Fmt.prefix (Text.length unop) (Fmt.Text_ unop) __


formatBinopChain as fn Int, FA.SepList Op.Binop FA.Expression: Fmt.Block =
    fn priority, left & opsAndRights:

    formatOpAndRight =
        fn binop & expr:
            precedence & block =
                formatExpression expr
            [
            , Fmt.textToBlock binop.symbol
            , if precedence > binop.precedence then
                block
              else
                parens block
            ]

    binopPrecedence =
        chainPrecedence opsAndRights

    expPrecedence & leftBlockRaw =
        formatExpression left

    leftBlock =
      if expPrecedence > binopPrecedence then
        leftBlockRaw
      else
        parens leftBlockRaw

    leftBlock :: List.concatMap formatOpAndRight opsAndRights
    >> Fmt.rowOrIndent Nothing __


formatCall as fn FA.Expression, [FA.Expression]: Fmt.Block =
    fn ref, args:

    ref :: args
    >> List.map formatExpressionNoPrecedence __
    >> Fmt.rowOrIndent Nothing __


formatIf as fn FA.Expression, FA.Expression, FA.Expression: Fmt.Block =
    fn condition, true, false:

    Fmt.textToBlock "-> TODO if <-"


