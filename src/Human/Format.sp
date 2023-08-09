alias Env =
    {
    , isRoot as Bool
    , originalContent as Text
    }


#
# TODO This all thing is a dumpster fire and right now I don't have enough brain to write it any better
#
prefixToFirstNonBlank as fn Text: fn Fmt.Block: Fmt.Block =
    fn prefix:
    fn block:
    try block as

        , Fmt.Empty:
            block

        , Fmt.SingleLine _ _:
            Fmt.prefix (Text.length prefix) (Fmt.Text_ prefix) block

        , Fmt.Stack head tail:
            !done =
                False

            doLine =
                fn indentedLine:
                if cloneUni @done then
                    indentedLine
                else
                    try indentedLine as

                        , Fmt.Indented i line:
                            # TODO rename to "should we prefix a comma here"?
                            isDecoration =
                                try line as
                                    , Fmt.CommentWithIndent _: True
                                    , Fmt.CommentIgnoreIndent _: True
                                    , Fmt.Blank: True
                                    , _: False

                            if isDecoration then
                                indentedLine
                            else
                                @done := True

                                Fmt.Row (Fmt.Text_ prefix) line >> Fmt.Indented i __

                        , _:
                            indentedLine

            try head :: tail >> List.reverse >> List.map doLine __ >> List.reverse as

                , []:
                    block

                , h :: t:
                    if cloneUni @done then
                        Fmt.Stack h t
                    else
                        Fmt.prefix (Text.length prefix) (Fmt.Text_ prefix) block


commaSeparatedList as fn Bool, Fmt.Block, Text, Bool, [ Fmt.Block ]: Fmt.Block =
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
                >> Maybe.map (Tuple.mapFirst List.reverse __) __

        try z as

            , Just ([ openLine, ...itemLines ] & mkLine):
                [#
                    $open item1, item2, item3 $close
                #]
                closeLine =
                    if closeHasAPrecedingSpace then
                        Fmt.Row Fmt.Space (Fmt.Text_ close)
                    else
                        Fmt.Text_ close

                itemLines
                >> List.intersperse (Fmt.Text_ ", ") __
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
                , List.map (prefixToFirstNonBlank ", ") items
                , [ Fmt.textToBlock close ]
                ]
                >> List.concat
                >> Fmt.stack


chainPrecedence as fn [ FA.Binop & a ]: Int =
    fn ls:
    try ls as
        , []: 0
        , [ op & _, ..._ ]: op.precedence


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

        , FA.BinopChain priority binopChain:
            chainPrecedence binopChain.second

        , FA.Poly text expression:
            9

        , FA.If _:
            9

        , FA.Try _:
            9

        , FA.Call _ _:
            Op.precedence_application

        , FA.Fn _ pars body:
            Op.precedence_function

        , FA.Lowercase { with  maybeType = Just _ }:
            Op.precedence_tuple - 1

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

    try e_ as

        , FA.LiteralText singleOrTriple text:
            formatLiteralText singleOrTriple text

        , FA.LiteralNumber hasPercentage text:
            formatLiteralNumber hasPercentage text

        , FA.ArgumentPlaceholder:
            Fmt.textToBlock "__"

        , FA.Statements stats:
            formatStatements env stats

        , FA.List isMultiline unpacksAndExprs:
            formatList env isMultiline unpacksAndExprs

        , FA.Record { attrs, isMultiline, maybeExtension }:
            formatRecord env isMultiline maybeExtension attrs

        , FA.Lowercase { attrPath, maybeModule, maybeType, name }:
            formatLowercase env maybeType maybeModule name attrPath

        , FA.Uppercase { maybeModule, name }:
            formatUppercase env maybeModule name

        , FA.Constructor { maybeModule, name }:
            formatConstructor env maybeModule name

        , FA.RecordShorthand { attrPath, name }:
            formatRecordShorthand env name attrPath

        , FA.Fn layout pars body:
            formatFunction env layout pars body

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

        , FA.If { condition, false, isMultiline, true }:
            formatIf env isMultiline faExpression

        , FA.Try { patterns, value }:
            formatTry env value patterns
    >> stackWithComments env comments __


#
# Comments
#
lineIsNonEmpty as fn Text: Bool =
    fn s: Text.trimLeft s /= ""


unindentBlockComment as fn Int, Text: [ Text ] =
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
    fn env, { end, indent, isBlock, isFollowedByBlank, start }:
    content =
        Text.slice start end env.originalContent

#    log ("```\n" .. content .. "\n```") {isBlock, indent}

    blockOrBlank as fn Text: Fmt.Block =
        fn text:
        if lineIsNonEmpty text then
            Fmt.CommentWithIndent text >> Fmt.lineToBlock
        else
            Fmt.blankLine

    comment =
        if indent == 0 then
            content
            >> unindentBlockComment indent __
            >> List.map (fn l: l >> Fmt.CommentIgnoreIndent >> Fmt.lineToBlock) __
            >> Fmt.stack
        else if isBlock then
            content
            >> unindentBlockComment indent __
            >> List.map blockOrBlank __
            >> Fmt.stack
        else
            Fmt.CommentWithIndent content >> Fmt.lineToBlock

    if isFollowedByBlank then
        Fmt.stack [ comment, Fmt.blankLine ]
    else
        comment


formatComments as fn Env, [ FA.Comment ]: Fmt.Block =
    fn env, comments:
    comments
    >> List.map (formatComment env __) __
    >> Fmt.stack


extractComments as fn FA.Expression: FA.Expression & [ FA.Comment ] =
    fn FA.Expression comments pos expr:
    FA.Expression [] pos expr & comments


stackWithComments as fn Env, [ FA.Comment ], Fmt.Block: Fmt.Block =
    fn env, comments, block:
    if comments == [] then
        block
    else
        Fmt.stack [ formatComments env comments, block ]


#
#
#
formatLiteralText as fn Token.SingleOrTriple, Text: Fmt.Block =
    singleQuote =
        "\""

    tripleQuote =
        Fmt.textToBlock "\"\"\""

    fn singleOrTriple, text:
    try singleOrTriple as

        , Token.SingleQuote:
            [#

                "text"

            #]
            singleQuote .. text .. singleQuote >> Fmt.textToBlock

        , Token.TripleQuote:
            [# TODO we need a `multiline` flag to distinguish between these two?

                """text"""

                """
                text
                """

            #]
            rows =
                text
                >> Text.split "\n" __
                >> List.map Fmt.textToBlock __

            [ [ tripleQuote ], rows, [ tripleQuote ] ]
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
formatStatements as fn Env, [ FA.Statement ]: Fmt.Block =
    fn env, sss:
    rec =
        fn maybePrevious, stats, acc:
        try stats as

            , []:
                List.reverse acc

            , head :: tail:
                try maybePrevious as

                    , Nothing:
                        acc

                    , Just (FA.CommentStatement _):
                        acc

                    , Just _:
                        if env.isRoot then
                            Fmt.blankLine :: Fmt.blankLine :: acc
                        else
                            Fmt.blankLine :: acc
                >> formatStatement { env with isRoot = False } head :: __
                >> rec (Just head) tail __

    rec Nothing sss [] >> Fmt.stack


formatStatement as fn Env, FA.Statement: Fmt.Block =
    fn env, stat:
    try stat as
        , FA.CommentStatement comment: formatComment env comment
        , FA.Evaluation expression: formatExpression env expression
        , FA.ValueDef valueDef: formatValueDef env valueDef
        , FA.AliasDef aliasDef: formatAliasDef env aliasDef
        , FA.UnionDef unionDef: formatUnionDef env unionDef


formatValueDef as fn Env, FA.ValueDef: Fmt.Block =
    fn env, { body, nonFn, pattern }:
        [
        , [
        , [ formatExpression env pattern ]
        , if nonFn == [] then [] else [ formatNonFn nonFn ]
        , [ Fmt.textToBlock "=" ]
        ]
        >> List.concat
        >> Fmt.spaceSeparatedOrIndent
        , Fmt.indent (formatExpression env body)
        ]
        >> Fmt.stack


formatNonFn as fn [ Pos & Name ]: Fmt.Block =
    fn words:
    words
    >> List.map (fn pos & name: Fmt.textToBlock name) __
    >> commaSeparatedList False (Fmt.textToBlock "with") "NonFunction" True __


formatDef as fn Text, Pos & Name, [ Pos & Name ]: Fmt.Block =
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
    fn env, { args, name, type }:
    [
    , formatDef "alias" name args
    , Fmt.indent (formatExpression env type)
    ]
    >> Fmt.stack


formatUnionDef as fn Env, FA.UnionDef: Fmt.Block =
    fn env, { args, constructors, name }:
    [
    , formatDef "var" name args
    , constructors
    >> List.map (fn c: Fmt.prefix 2 (Fmt.Text_ ", ") (formatExpression env c)) __
    >> Fmt.stack
    >> Fmt.indent
    ]
    >> Fmt.stack


formatList as fn Env, Bool, [ Bool & FA.Expression ]: Fmt.Block =
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


formatRecord as fn Env, Bool, Maybe (Maybe FA.Expression), [ FA.RecordAttribute ]: Fmt.Block =
    fn env, isMultiline, maybeMaybeExt, attrs:
    open =
        try maybeMaybeExt as

            , Nothing:
                Fmt.textToBlock "{"

            , Just Nothing:
                Fmt.textToBlock "{ with "

            , Just (Just ext):
                ext
                >> formatExpression env __
                >> Fmt.prefix 1 (Fmt.Text_ "{ ") __
                >> Fmt.addSuffix (Fmt.Text_ " with") __

    formatRecordAttribute as fn FA.RecordAttribute: Fmt.Block =
        fn { maybeExpr, name }:
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
            , FA.Expression _ _ (FA.Lowercase { with name }): name
            , _: ""

    attrs
    >> List.sortBy attributeName __
    >> List.map formatRecordAttribute __
    >> commaSeparatedList isMultiline open "}" True __


#formatTokenWord as fn Token.Word: Fmt.Block =
#    fn { modifier, isUpper, maybeModule, name, attrPath }:
#
#    [
#    , try maybeModule as
#        , Nothing: []
#        , Just module: [ module, "." ]
#
#    , [ name ]
#
#    , List.map (fn p: "." .. p) attrPath
#    ]
#    >> List.concat
#    >> Text.join "" __
#    >> Fmt.textToBlock

formatFaWord as fn Pos & Name: Fmt.Block =
    fn pos & name: Fmt.textToBlock name


formatLowercase as fn Env, Maybe FA.Expression, Maybe Name, Name, [ Name ]: Fmt.Block =
    fn env, maybeType, maybeModule, name, attrPath:
    word =
        [
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
            , Fmt.addSuffix (Fmt.Text_ " as") word
            , formatExpression env type
            ]
            >> Fmt.spaceSeparatedOrIndent __


formatUppercase as fn Env, Maybe Name, Name: Fmt.Block =
    fn env, maybeModule, name:
        [
        , try maybeModule as
            , Nothing: []
            , Just module: [ module, "." ]
        , [ name ]
        ]
        >> List.concat
        >> Text.join "" __
        >> Fmt.textToBlock


formatConstructor as fn Env, Maybe Name, Name: Fmt.Block =
    fn env, maybeModule, name:
        [
        , try maybeModule as
            , Nothing: []
            , Just module: [ module, "." ]
        , [ name ]
        ]
        >> List.concat
        >> Text.join "" __
        >> Fmt.textToBlock


formatRecordShorthand as fn Env, Name, [ Name ]: Fmt.Block =
    fn env, name, attrPath:
        name :: List.map (fn p: "." .. p) attrPath
        >> Text.join "" __
        >> Fmt.textToBlock


formatFunctionHeader as fn Env, [ FA.Expression ]: Fmt.Block =
    fn env, pars:
    pars
    >> List.map (formatExpression env __) __
    >> commaSeparatedList False (Fmt.textToBlock "fn") ":" False __


formatFunction as fn Env, FA.Layout, [ FA.Expression ], FA.Expression: Fmt.Block =
    fn env, layout, pars, body:
    forceStack =
        layout /= FA.Inline

    [
    , formatFunctionHeader env pars
    , formatExpression env body >> applyIf (layout == FA.Indented) Fmt.indent
    ]
    >> Fmt.spaceSeparatedOrStackForce forceStack __


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
        formatExpressionAndMaybeAddParens env binop.precedence expr
        >> Fmt.prefix 0 (Fmt.Text_ (binop.symbol .. " ")) __
        >> stackWithComments env binop.comments __

    forceMultiline =
        try opsAndRights as

            , []:
                False

            , [ first & _, ...rest ]:
                # op.symbol == ">>"
                last as FA.Binop =
                    List.for first rest (fn opX & _, acc: opX)

                last.line > first.line

    [
    , formatExpressionAndMaybeAddParens env (chainPrecedence opsAndRights) left
    , ...List.map formatOpAndRight opsAndRights
    ]
    >> Fmt.spaceSeparatedOrStackForce forceMultiline __


formatCall as fn Env, FA.Expression, [ FA.Expression ]: Fmt.Block =
    fn env, ref, args:
    lastIndex =
        List.length args - 1

    asContinuingFn =
        fn index, arg:
        try arg as

            , FA.Expression _ _ (FA.Fn layout params body):
                if index == lastIndex and layout /= FA.Inline then
                    Just (layout & params & body)
                else
                    Nothing

            , _:
                Nothing

    formatArgument as fn Int, FA.Expression: Fmt.Block =
        fn index, arg:
        try asContinuingFn index arg as
            , Nothing: formatExpressionAndMaybeAddParens env Op.precedence_application arg
            , Just (layout & params & body): formatFunctionHeader env params

    maybeContinuing =
        args
        >> List.last
        >> Maybe.onJust (asContinuingFn lastIndex __)

    refNoComments & refComments =
        extractComments ref

    call =
        Fmt.spaceSeparatedOrIndent
            [
            , formatExpressionAndMaybeAddParens env Op.precedence_application refNoComments
            , ...List.indexedMap formatArgument args
            ]

    try maybeContinuing as

        , Nothing:
            call

        , Just (layout & params & body):
            Fmt.stack
                [
                , call
                , formatExpression env body >> applyIf (layout == FA.Indented) Fmt.indent
                ]
    >> stackWithComments env refComments __


extractIfElses as fn Env, FA.Expression: [ [ FA.Comment ] & Fmt.Block & Fmt.Block ] & Fmt.Block =
    fn env, x:
    rec =
        fn acc, expr:
        try expr as
            , FA.Expression comments _ (FA.If args): rec [ comments & formatExpression env args.condition & formatExpression env args.true, ...acc ] args.false
            , _: List.reverse acc & formatExpression env expr

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

                , [ [] & condition & valueIfTrue ]:
                    l =
                        Fmt.blockAsLine

                    try l condition & l valueIfTrue & l default as
                        , Just conditionLine & Just trueLine & Just falseLine: Just (conditionLine & trueLine & falseLine)
                        , _: Nothing

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
            >> List.intersperse Fmt.Space __
            >> Fmt.for1 __ (fn item, acc: Fmt.Row acc item)
            >> Fmt.lineToBlock

        , Nothing:
            formatCAndV as fn Int, [ FA.Comment ] & Fmt.Block & Fmt.Block: Fmt.Block =
                fn index, comments & condition & value:
                Fmt.stack
                    [
                    , Fmt.spaceSeparatedOrStack
                        [
                        , if index == 0 then Fmt.textToBlock "if" else Fmt.textToBlock "else if"
                        , condition
                        , Fmt.textToBlock "then"
                        ]
                    >> stackWithComments env comments __
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


formatTry as fn Env, FA.Expression, [ FA.Expression & FA.Expression ]: Fmt.Block =
    fn env, value, patterns:
    formatted as [ Fmt.Block & Fmt.Block ] =
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

    blocks as [ Fmt.Block ] =
        try List.mapRes tryOneLine formatted as

            , Ok lines:
                formatInline =
                    fn paLine & blockLine:
                    Fmt.Row paLine (Fmt.Row (Fmt.Text_ ": ") blockLine) >> Fmt.lineToBlock

                List.map formatInline lines

            # TODO restore `None` here once it doesn't break JS any more
            , Err _:
                formatIndented =
                    fn paBlock & blockBlock:
                    [
                    , Fmt.blankLine
                    , paBlock
                    >> Fmt.addSuffix (Fmt.Text_ ":") __
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
