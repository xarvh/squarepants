
alias Env = {
    , isRoot as Bool
    , originalContent as Text
    }




#
# TODO This all thing is a dumpster fire and right now I don't have enough brain to write it any better
#
prefixToFirstNonComment as fn Text: fn Fmt.Block: Fmt.Block =
    fn prefix:
    fn block:

    try block as
        , Fmt.Empty:
            block

        , Fmt.SingleLine _ _:
            Fmt.prefix (Text.length prefix) (Fmt.Text_ prefix) block

        , Fmt.Stack head tail:
            !done = False

            doLine =
                fn indentedLine:
                if cloneUni @done then
                    indentedLine
                else
                    try indentedLine as
                        , Fmt.Indented i line:
                            isComment =
                                try line as
                                    , Fmt.Text_ t: Text.startsWith "#" t
                                    , _: False

                            if isComment then
                                indentedLine
                            else
                                @done := True
                                Fmt.Row (Fmt.Text_ prefix) line
                                >> Fmt.Indented i __

                        , _:
                              indentedLine

            try (head :: tail) >> List.reverse >> List.map doLine __ >> List.reverse as
                , []: block
                , h :: t:
                    if cloneUni @done then
                        Fmt.Stack h t
                    else
                        Fmt.prefix (Text.length prefix) (Fmt.Text_ prefix) block




commaSeparatedList as fn Bool, Fmt.Block, Text, Bool, [Fmt.Block]: Fmt.Block =
    fn forceMultiline, open, close, closeHasAPrecedingSpace, items:

    if items == [] then
        Fmt.addSuffix (Fmt.Text_ close) open
    else
        z =
            if forceMultiline then
              Nothing
            else
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
                , List.map (prefixToFirstNonComment ", ") items
                , [ Fmt.textToBlock close ]
                ]
                >> List.concat
                >> Fmt.stack


chainPrecedence as fn [FA.Binop & a]: Int =
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


expressionPrecedence as fn FA.Expression: Int =
    fn FA.Expression _ _ e_:

    try e_ as

        , FA.Statements stats:
            0

        , FA.Fn _ pars body:
            0

        , FA.BinopChain priority binopChain:
            chainPrecedence binopChain.second

        , FA.Poly text expression:
            9

        , FA.If _:
            9

        , FA.Try _:
            9

        , _:
            # No parens needed
            10


formatExpressionAndMaybeAddParens as fn Env, Int, FA.Expression: Fmt.Block =
    fn env, binopPrecedence, expression:

    block =
        formatExpression env expression

    if expressionPrecedence expression > binopPrecedence then
        block
    else
        parens block



formatExpression as fn Env, FA.Expression: Fmt.Block =
    fn env, faExpression:

    FA.Expression comments _ e_ =
        faExpression

    b =
      try e_ as

        , FA.LiteralText text:
            formatLiteralText text

        , FA.LiteralNumber hasPercentage text:
            formatLiteralNumber hasPercentage text

        , FA.ArgumentPlaceholder:
            Fmt.textToBlock "__"

        , FA.Statements stats:
            formatStatements env stats

        , FA.List isMultiline unpacksAndExprs:
            formatList env isMultiline unpacksAndExprs

        , FA.Record { isMultiline, maybeExtension, attrs }:
            formatRecord env isMultiline maybeExtension attrs

        , FA.Variable { maybeType, tokenWord }:
            formatVariable env maybeType tokenWord

        , FA.Fn bodyIsBelow pars body:
            formatFunction env bodyIsBelow pars body

        , FA.UnopCall unopId expr:
            formatUnopCall env unopId expr

        , FA.BinopChain priority binopChain:
            formatBinopChain env priority binopChain

        , FA.Call ref args:
            formatCall env ref args

        , FA.Poly text expression:
            prefix =
                text .. "?"

            expression
            >> formatExpression env __
            >> Fmt.prefix (Text.length prefix) (Fmt.Text_ prefix) __

        , FA.If { isMultiline, condition, true, false }:
            formatIf env isMultiline faExpression

        , FA.Try { value, patterns }:
            formatTry env value patterns

    if comments == [] then
        b
    else
        [
        , List.map (formatComment env __) comments
        , [ b ]
        ]
        >> List.concat
        >> Fmt.stack



lineIsNonEmpty as fn Text: Bool =
    fn s: Text.trimLeft s /= ""


unindentBlockComment as fn Int, Text: [Text] =
    fn indent, content:

    try Text.split "\n" content as

        , []:
            []

        , head :: tail:

            getLeadingSpaces =
                Text.startsWithRegex "[ ]*"

            countLeadingSpaces as fn Text: Int =
                fn l: l >> getLeadingSpaces >> Text.length

            minLead as Int =
                # `head` contains a "[#", we already know its lead is `indent`
                tail
                >> List.filter lineIsNonEmpty __
                >> List.for indent __ fn line, length:
                    min (countLeadingSpaces line) length

            head :: List.map (Text.dropLeft minLead __) tail


formatComment as fn Env, FA.Comment: Fmt.Block =
    fn env, { start, end, isBlock, indent, isFollowedByBlank }:

    content =
        Text.slice start end env.originalContent

#    log ("```\n" .. content .. "\n```") {isBlock, indent}

    blockOrBlank as fn Text: Fmt.Block =
        fn text:
        if lineIsNonEmpty text then
            Fmt.textToBlock text
        else
            Fmt.blankLine

    comment =
        if indent == 0 then
            content
            >> unindentBlockComment indent __
            >> List.map (fn l: l >> Fmt.NoIndent >> Fmt.lineToBlock) __
            >> Fmt.stack

        else if isBlock then
            content
            >> unindentBlockComment indent __
            >> List.map blockOrBlank __
            >> Fmt.stack

        else
            Fmt.textToBlock content

    if isFollowedByBlank then
        Fmt.stack [ comment, Fmt.blankLine ]
    else
        comment


formatLiteralText as fn Text: Fmt.Block =

    singleQuote =
        "\""

    tripleQuote =
        Fmt.textToBlock "\"\"\""

    matchNewlinesOrQuotes =
        Text.startsWithRegex """.*([^\]["])|\n"""

    escape as fn Text: Text =
        fn x:
        x
        #>> Text.replace "\\" "\\\\" __
        >> Text.replace "\"" "\\\"" __

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


# TODO create a formatRootStatements that takes only a file content instead than a full Env
formatStatements as fn Env, [FA.Statement]: Fmt.Block =
    fn env, sss:

    rec =
        fn maybePrevious, stats, acc:

        try stats as
            , []: List.reverse acc
            , head :: tail:
                try maybePrevious as
                    , Nothing: acc
                    , Just (FA.CommentStatement _): acc
                    , Just _:
                        if env.isRoot then
                            Fmt.blankLine :: Fmt.blankLine :: acc
                        else
                            Fmt.blankLine :: acc
                >> (formatStatement { env with isRoot = False } head) :: __
                >> rec (Just head) tail __

    rec Nothing sss []
    >> Fmt.stack


formatStatement as fn Env, FA.Statement: Fmt.Block =
    fn env, stat:
    try stat as
        , FA.CommentStatement comment: formatComment env comment
        , FA.Evaluation expression: formatExpression env expression
        , FA.ValueDef valueDef: formatValueDef env valueDef
        , FA.AliasDef aliasDef: formatAliasDef env aliasDef
        , FA.UnionDef unionDef: formatUnionDef env unionDef


formatValueDef as fn Env, FA.ValueDef: Fmt.Block =
    fn env, { pattern, nonFn, body }:

        [
        , [
            , [formatExpression env pattern]
            , if nonFn == [] then [] else [ formatNonFn nonFn ]
            , [Fmt.textToBlock "="]
            ]
            >> List.concat
            >> Fmt.spaceSeparatedOrIndent
        , Fmt.indent (formatExpression env body)
        ]
        >> Fmt.stack


formatNonFn as fn [FA.Word]: Fmt.Block =
    fn words:

    words
    >> List.map formatFaWord __
    >> commaSeparatedList False (Fmt.textToBlock "with") "NonFunction" True __



formatDef as fn Text, FA.Word, [FA.Word]: Fmt.Block =
    fn keyword, name, args:

    formattedArgs =
        if args == [] then
          Nothing
        else
          args
          >> List.map formatFaWord __
          >> Fmt.spaceSeparatedOrIndent
          >> Just

    [
    , Just << Fmt.textToBlock keyword
    , Just << formatFaWord name
    , formattedArgs
    , Just << Fmt.textToBlock "="
    ]
    >> List.filterMap identity __
    >> Fmt.spaceSeparatedOrIndent


formatAliasDef as fn Env, FA.AliasDef: Fmt.Block =
    fn env, { name, args, type }:

    [
    , formatDef "alias" name args
    , Fmt.indent (formatExpression env type)
    ]
    >> Fmt.stack


formatUnionDef as fn Env, FA.UnionDef: Fmt.Block =
    fn env, { name, args, constructors }:

    [
    , formatDef "union" name args
    , constructors
      >> List.map (fn c: Fmt.prefix 2 (Fmt.Text_ ", ") (formatExpression env c)) __
      >> Fmt.stack
      >> Fmt.indent
    ]
    >> Fmt.stack


formatList as fn Env, Bool, [Bool & FA.Expression]: Fmt.Block =
    fn env, isMultiline, unpacksAndExprs:

    formatListItem =
        fn isUnpacked & expr:

        if isUnpacked then
            expr
            >> formatExpression env __
            >> Fmt.prefix 3 (Fmt.Text_ "...") __
        else
            formatExpression env expr

    unpacksAndExprs
    >> List.map formatListItem __
    >> commaSeparatedList isMultiline (Fmt.textToBlock "[") "]" True __


formatRecord as fn Env, Bool, Maybe (Maybe FA.Expression), [FA.RecordAttribute]: Fmt.Block =
    fn env, isMultiline, maybeMaybeExt, attrs:

    open =
        try maybeMaybeExt as
            , Nothing: Fmt.textToBlock "{"
            , Just Nothing: Fmt.textToBlock "{ with"
            , Just (Just ext):
                ext
                >> formatExpression env __
                >> Fmt.prefix 1 (Fmt.Text_ "{") __
                >> Fmt.addSuffix (Fmt.Text_ "with") __

    formatRecordAttribute as fn FA.RecordAttribute: Fmt.Block =
        fn { name, maybeExpr }:

        try maybeExpr as

            , Nothing:
                formatExpression env name

            , Just expr:
                [
                , formatExpression env name >> Fmt.addSuffix (Fmt.Text_ " =") __
                , formatExpression env expr
                ]
                >> Fmt.rowOrIndent (Just Fmt.Space) __

    attributeName =
        fn nameExpr:
        try nameExpr.name as
            , FA.Expression _ _ (FA.Variable pas): pas.tokenWord.name
            , _: ""

    attrs
    >> List.sortBy attributeName __
    >> List.map formatRecordAttribute __
    >> commaSeparatedList isMultiline open "}" True __





formatTokenWord as fn Token.Word: Fmt.Block =
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


formatFaWord as fn FA.Word: Fmt.Block =
    fn FA.Word _ tokenWord: formatTokenWord tokenWord


formatVariable as fn Env, Maybe FA.Expression, Token.Word: Fmt.Block =
    fn env, maybeType, word:

    try maybeType as
        , Nothing:
            formatTokenWord word

        , Just type:
            [
            , Fmt.addSuffix (Fmt.Text_ " as") (formatTokenWord word)
            , formatExpression env type
            ]
            >> Fmt.spaceSeparatedOrIndent __


formatFunction as fn Env, Bool, [FA.Expression], FA.Expression: Fmt.Block =
    fn env, bodyIsBelow, pars, body:

    [
    , pars
      >> List.map (formatExpression env __) __
      >> commaSeparatedList False (Fmt.textToBlock "fn") ":" False __
    , formatExpression env body
    ]
    >> Fmt.spaceSeparatedOrStackForce bodyIsBelow __


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


formatUnopCall as fn Env, Op.UnopId, FA.Expression: Fmt.Block =
    fn env, unopId, expr:

    unop =
        unopToText unopId

    expr
    >> formatExpression env __
    >> Fmt.prefix (Text.length unop) (Fmt.Text_ unop) __


formatBinopChain as fn Env, Int, FA.BinopChain: Fmt.Block =
    fn env, priority, left & opsAndRights:

    formatOpAndRight =
        fn binop & expr:

        e =
            formatExpressionAndMaybeAddParens env binop.precedence expr
            >> Fmt.prefix (Text.length binop.symbol + 1) (Fmt.Text_ (binop.symbol .. " ")) __

        if binop.comments == [] then
            e
        else
            [List.map (formatComment env __) binop.comments
            , [ e ]
            ]
            >> List.concat
            >> Fmt.stack


    forceMultiline =
        try opsAndRights as
            , []: False
            , [ first & _, ...rest]:

                # op.symbol == ">>"
                last as FA.Binop = List.for first rest fn opX & _, acc: opX
                last.line > first.line

    #log "FM" { forceMultiline }


    [
    , formatExpressionAndMaybeAddParens env (chainPrecedence opsAndRights) left
    , ...List.map formatOpAndRight opsAndRights
    ]
    >> Fmt.spaceSeparatedOrStackForce forceMultiline __


formatCall as fn Env, FA.Expression, [FA.Expression]: Fmt.Block =
    fn env, ref, args:

    ref :: args
    >> List.map (formatExpression env __) __
    >> Fmt.spaceSeparatedOrIndent __



extractIfElses as fn Env, FA.Expression: [Fmt.Block & Fmt.Block] & Fmt.Block =
    fn env, x:

    rec =
        fn acc, expr:
        try expr as
            , FA.Expression comments _ (FA.If args):
                # TODO ADD COMMENTS <--------------------
                rec [ formatExpression env args.condition & formatExpression env args.true, ...acc ] args.false

            , _:
                List.reverse acc & formatExpression env expr

    rec [] x


formatIf as fn Env, Bool, FA.Expression: Fmt.Block =
    fn env, isMultiline, expr:

    conditionsAndValues & default =
        extractIfElses env expr

    maybeSingleLine =
        if isMultiline then
            Nothing
        else
            try conditionsAndValues as
                , [ condition & valueIfTrue ]:
                    l = Fmt.blockAsLine
                    try l condition & l valueIfTrue & l default as
                        , Just conditionLine & Just trueLine & Just falseLine:
                            Just (conditionLine & trueLine & falseLine)
                        , _:
                            Nothing

                , _:
                    Nothing

    try maybeSingleLine as
        , Just (conditionLine & trueLine & falseLine):
            [
            , Fmt.Text_ "if"
            , conditionLine
            , Fmt.Text_ "then"
            , trueLine
            , Fmt.Text_ "else"
            , falseLine
            ]
            >> List.intersperse Fmt.Space __ []
            >> Fmt.for1 __ (fn item, acc: Fmt.Row acc item)
            >> Fmt.lineToBlock

        , Nothing:

            formatCAndV as fn Int, Fmt.Block & Fmt.Block: Fmt.Block =
                fn index, condition & value:

                Fmt.stack
                    [
                    , Fmt.spaceSeparatedOrStack
                        [
                        , if index == 0 then Fmt.textToBlock "if" else Fmt.textToBlock "else if"
                        , condition
                        , Fmt.textToBlock "then"
                        ]
                    , Fmt.indent value
                    ]

            Fmt.stack
                [
                , conditionsAndValues
                  >> List.indexedMap formatCAndV __
                  >> Fmt.stack

                , Fmt.textToBlock "else"
                , Fmt.indent default
                ]



formatTry as fn Env, FA.Expression, [FA.Expression & FA.Expression]: Fmt.Block =
    fn env, value, patterns:

    formatted as [Fmt.Block & Fmt.Block] =
        List.map (fn pattern & block: formatExpression env pattern & formatExpression env block) patterns

    tryOneLine as fn Fmt.Block & Fmt.Block: Result None (Fmt.Line & Fmt.Line) =
        fn pa & bl:

        Fmt.blockAsLine pa
        >> Maybe.toResult None __
        >> onOk fn paLine:

        Fmt.blockAsLine bl
        >> Maybe.toResult None __
        >> onOk fn blockLine:

        Ok (paLine & blockLine)

    blocks as [Fmt.Block] =
        try List.mapRes tryOneLine formatted as

            , Ok lines:

                formatInline =
                    fn paLine & blockLine:

                    Fmt.Row
                        (Fmt.Row (Fmt.Text_ ", ") paLine)
                        (Fmt.Row (Fmt.Text_ ": ") blockLine)
                    >> Fmt.lineToBlock

                List.map formatInline lines

            # TODO restore `None` here once it doesn't break JS any more
            , Err _:

                formatIndented =
                    fn paBlock & blockBlock:

                    [
                    , Fmt.blankLine
                    , paBlock
                      >> Fmt.addSuffix (Fmt.Text_ ":") __
                      >> prefixToFirstNonComment ", " #Fmt.prefix 2 (Fmt.Text_ ", ") __
                    , Fmt.indent blockBlock
                    ]
                    >> Fmt.stack

                List.map formatIndented formatted

    [
    , Fmt.spaceSeparatedOrIndent
      [
      , Fmt.textToBlock "try"
      , formatExpression env value
      , Fmt.textToBlock "as"
      ]
    , blocks
      >> Fmt.stack
      >> Fmt.indent
    ]
    >> Fmt.stack

