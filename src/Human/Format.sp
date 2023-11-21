Env =
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

        Fmt.'empty:
            block

        Fmt.'singleLine _ _:
            Fmt.prefix (Text.length prefix) (Fmt.'text_ prefix) block

        Fmt.'stack head tail:
            !done =
                'false

            doLine =
                fn indentedLine:
                if cloneUni @done then
                    indentedLine
                else
                    try indentedLine as

                        Fmt.'indented i line:
                            # TODO rename to "should we prefix a comma here"?
                            isDecoration =
                                try line as
                                    Fmt.'commentWithIndent _: 'true
                                    Fmt.'commentIgnoreIndent _: 'true
                                    Fmt.'blank: 'true
                                    _: 'false

                            if isDecoration then
                                indentedLine
                            else
                                @done := 'true

                                Fmt.'row (Fmt.'text_ prefix) line >> Fmt.'indented i __

                        _:
                            indentedLine

            try head :: tail >> List.reverse >> List.map doLine __ >> List.reverse as

                []:
                    block

                h :: t:
                    if cloneUni @done then
                        Fmt.'stack h t
                    else
                        Fmt.prefix (Text.length prefix) (Fmt.'text_ prefix) block


commaSeparatedList as fn Bool, Fmt.Block, Text, Bool, [ Fmt.Block ]: Fmt.Block =
    fn forceMultiline, open, close, closeHasAPrecedingSpace, items:
    if items == [] then
        Fmt.addSuffix (Fmt.'text_ close) open
    else
        z =
            if forceMultiline then
                'nothing
            else
                open :: items
                >> Fmt.maybeAllSingleLines
                >> Maybe.map (Tuple.mapFirst List.reverse __) __

        try z as

            'just ([ openLine, itemLines... ] & mkLine):
                [#
                    $open item1, item2, item3 $close
                #]
                closeLine =
                    if closeHasAPrecedingSpace then
                        Fmt.'row Fmt.'space (Fmt.'text_ close)
                    else
                        Fmt.'text_ close

                itemLines
                >> List.intersperse (Fmt.'text_ ", ") __
                >> List.for (Fmt.'row openLine Fmt.'space) __ (fn a, b: Fmt.'row b a)
                >> mkLine
                >> Fmt.addSuffix closeLine __

            'nothing:
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
        []: 0
        [ op & _, _... ]: op.precedence


parens as fn Fmt.Block: Fmt.Block =
    fn block:
    [
    , Fmt.prefix 1 (Fmt.'text_ "(") block
    , Fmt.textToBlock ")"
    ]
    >> Fmt.rowOrStack 'nothing __


expressionPrecedence as fn FA.Expression: Int =
    fn FA.'expression _ _ e_:
    try e_ as

        FA.'statements stats:
            0

        FA.'binopChain priority binopChain:
            chainPrecedence binopChain.second

        FA.'poly text expression:
            9

        FA.'if _:
            9

        FA.'try _:
            9

        FA.'call _ _:
            Op.precedence_application

        FA.'fn _ pars body:
            Op.precedence_function

        FA.'lowercase { with  maybeType = 'just _ }:
            Op.precedence_tuple - 1

        _:
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
    FA.'expression comments _ e_ =
        faExpression

    try e_ as

        FA.'literalText singleOrTriple text:
            formatLiteralText singleOrTriple text

        FA.'literalNumber hasPercentage text:
            formatLiteralNumber hasPercentage text

        FA.'argumentPlaceholder:
            Fmt.textToBlock "__"

        FA.'statements stats:
            formatStatements env stats

        FA.'list isMultiline unpacksAndExprs:
            formatList env isMultiline unpacksAndExprs

        FA.'record { attrs, isMultiline, maybeExtension }:
            formatRecord env isMultiline maybeExtension attrs

        FA.'lowercase { attrPath, maybeModule, maybeType, name }:
            formatLowercase env maybeType maybeModule name attrPath

        FA.'uppercase { maybeModule, name }:
            formatUppercase env maybeModule name

        FA.'constructor { maybeModule, name }:
            formatConstructor env maybeModule name

        FA.'recordShorthand { attrPath, name }:
            formatRecordShorthand env name attrPath

        FA.'fn layout pars body:
            formatFunction env layout pars body

        FA.'unopCall unopId expr:
            formatUnopCall env unopId expr

        FA.'binopChain priority binopChain:
            formatBinopChain env priority binopChain

        FA.'call ref args:
            formatCall env ref args

        FA.'poly text expression:
            prefix =
                text .. "?"

            expression
            >> formatExpression env __
            >> Fmt.prefix (Text.length prefix) (Fmt.'text_ prefix) __

        FA.'if { condition, false, isMultiline, true }:
            formatIf env isMultiline faExpression

        FA.'try { patterns, value }:
            formatTry env value patterns

        FA.'native:
            Fmt.textToBlock "this_is_sp_native"

    >> stackWithComments env comments __


#
# Comments
#
lineIsNonEmpty as fn Text: Bool =
    fn s: Text.trimLeft s /= ""


unindentBlockComment as fn Int, Text: [ Text ] =
    fn indent, content:
    try Text.split "\n" content as

        []:
            []

        head :: tail:
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
            Fmt.'commentWithIndent text >> Fmt.lineToBlock
        else
            Fmt.blankLine

    comment =
        if indent == 0 then
            content
            >> unindentBlockComment indent __
            >> List.map (fn l: l >> Fmt.'commentIgnoreIndent >> Fmt.lineToBlock) __
            >> Fmt.stack
        else if isBlock then
            content
            >> unindentBlockComment indent __
            >> List.map blockOrBlank __
            >> Fmt.stack
        else
            Fmt.'commentWithIndent content >> Fmt.lineToBlock

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
    fn FA.'expression comments pos expr:
    FA.'expression [] pos expr & comments


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

        Token.'singleQuote:
            [#

                "text"

            #]
            singleQuote .. text .. singleQuote >> Fmt.textToBlock

        Token.'tripleQuote:
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

            []:
                List.reverse acc

            head :: tail:
                try maybePrevious as

                    'nothing:
                        acc

                    'just (FA.'commentStatement _):
                        acc

                    'just _:
                        if env.isRoot then
                            Fmt.blankLine :: Fmt.blankLine :: acc
                        else
                            Fmt.blankLine :: acc
                >> formatStatement { env with isRoot = 'false } head :: __
                >> rec ('just head) tail __

    rec 'nothing sss [] >> Fmt.stack


formatStatement as fn Env, FA.Statement: Fmt.Block =
    fn env, stat:
    try stat as
        FA.'commentStatement comment: formatComment env comment
        FA.'evaluation expression: formatExpression env expression
        FA.'valueDef valueDef: formatValueDef env valueDef
        FA.'aliasDef aliasDef: formatAliasDef env aliasDef
        FA.'unionDef unionDef: formatUnionDef env unionDef


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
    >> commaSeparatedList 'false (Fmt.textToBlock "with") "NonFunction" 'true __


formatDef as fn Maybe Text, Pos & Name, [ Pos & Name ]: Fmt.Block =
    fn maybeKeyword, name, args:
    formattedArgs =
        if args == [] then
            'nothing
        else
            args
            >> List.map formatFaWord __
            >> Fmt.spaceSeparatedOrIndent
            >> 'just

    [
    , Maybe.map Fmt.textToBlock maybeKeyword
    , 'just << formatFaWord name
    , formattedArgs
    , 'just << Fmt.textToBlock "="
    ]
    >> List.filterMap identity __
    >> Fmt.spaceSeparatedOrIndent


formatAliasDef as fn Env, FA.AliasDef: Fmt.Block =
    fn env, { args, name, type }:
    [
    , formatDef 'nothing name args
    , Fmt.indent (formatExpression env type)
    ]
    >> Fmt.stack


formatUnionDef as fn Env, FA.VariantTypeDef: Fmt.Block =
    fn env, { args, constructors, name }:
    [
    , formatDef ('just "var") name args
    , constructors
    >> List.map (fn c: Fmt.prefix 2 (Fmt.'text_ ", ") (formatExpression env c)) __
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
            >> Fmt.addSuffix (Fmt.'text_ "...") __
        else
            formatExpression env expr

    unpacksAndExprs
    >> List.map formatListItem __
    >> commaSeparatedList isMultiline (Fmt.textToBlock "[") "]" 'true __


formatRecord as fn Env, Bool, Maybe (Maybe FA.Expression), [ FA.RecordAttribute ]: Fmt.Block =
    fn env, isMultiline, maybeMaybeExt, attrs:
    open =
        try maybeMaybeExt as

            'nothing:
                Fmt.textToBlock "{"

            'just 'nothing:
                Fmt.textToBlock "{ with "

            'just ('just ext):
                ext
                >> formatExpression env __
                >> Fmt.prefix 1 (Fmt.'text_ "{ ") __
                >> Fmt.addSuffix (Fmt.'text_ " with") __

    formatRecordAttribute as fn FA.RecordAttribute: Fmt.Block =
        fn { maybeExpr, name }:
        try maybeExpr as

            'nothing:
                formatExpression env name

            'just expr:
                [
                , formatExpression env name >> Fmt.addSuffix (Fmt.'text_ " =") __
                , formatExpression env expr
                ]
                >> Fmt.rowOrIndent ('just Fmt.'space) __

    attributeName =
        fn nameExpr:
        try nameExpr.name as
            FA.'expression _ _ (FA.'lowercase { with  name }): name
            _: ""

    attrs
    >> List.sortBy attributeName __
    >> List.map formatRecordAttribute __
    >> commaSeparatedList isMultiline open "}" 'true __


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
            'nothing: []
            'just module: [ module, "." ]
        , [ name ]
        , List.map (fn p: "." .. p) attrPath
        ]
        >> List.concat
        >> Text.join "" __
        >> Fmt.textToBlock

    try maybeType as

        'nothing:
            word

        'just type:
            [
            , Fmt.addSuffix (Fmt.'text_ " as") word
            , formatExpression env type
            ]
            >> Fmt.spaceSeparatedOrIndent __


formatUppercase as fn Env, Maybe Name, Name: Fmt.Block =
    fn env, maybeModule, name:
        [
        , try maybeModule as
            'nothing: []
            'just module: [ module, "." ]
        , [ name ]
        ]
        >> List.concat
        >> Text.join "" __
        >> Fmt.textToBlock


formatConstructor as fn Env, Maybe Name, Name: Fmt.Block =
    fn env, maybeModule, name:
        [
        , try maybeModule as
            'nothing: []
            'just module: [ module, "." ]
        , [ name ]
        ]
        >> List.concat
        >> Text.join "" __
        >> Fmt.textToBlock


formatRecordShorthand as fn Env, Name, [ Name ]: Fmt.Block =
    fn env, name, attrPath:
        List.map (fn p: "." .. p) (name :: attrPath)
        >> Text.join "" __
        >> Fmt.textToBlock


formatFunctionHeader as fn Env, [ FA.Expression ]: Fmt.Block =
    fn env, pars:
    pars
    >> List.map (formatExpression env __) __
    >> commaSeparatedList 'false (Fmt.textToBlock "fn") ":" 'false __


formatFunction as fn Env, FA.Layout, [ FA.Expression ], FA.Expression: Fmt.Block =
    fn env, layout, pars, body:
    forceStack =
        layout /= FA.'inline

    [
    , formatFunctionHeader env pars
    , formatExpression env body >> applyIf (layout == FA.'indented) Fmt.indent
    ]
    >> Fmt.spaceSeparatedOrStackForce forceStack __


unopToText as fn Op.UnopId: Text =
    fn unopId:
    try unopId as
        Op.'unopPlus: "+"
        Op.'unopMinus: "-"
        Op.'unopUnique: "!"
        Op.'unopRecycle: "@"


formatUnop as fn Op.UnopId: Fmt.Block =
    fn unopId:
    unopId >> unopToText >> Fmt.textToBlock


formatUnopCall as fn Env, Op.UnopId, FA.Expression: Fmt.Block =
    fn env, unopId, expr:
    unop =
        unopToText unopId

    expr
    >> formatExpression env __
    >> Fmt.prefix (Text.length unop) (Fmt.'text_ unop) __


formatBinopChain as fn Env, Int, FA.BinopChain: Fmt.Block =
    fn env, priority, left & opsAndRights:
    formatOpAndRight =
        fn binop & expr:
        formatExpressionAndMaybeAddParens env binop.precedence expr
        >> Fmt.prefix 0 (Fmt.'text_ (binop.symbol .. " ")) __
        >> stackWithComments env binop.comments __

    forceMultiline =
        try opsAndRights as

            []:
                'false

            [ first & _, rest... ]:
                # op.symbol == ">>"
                last as FA.Binop =
                    List.for first rest (fn opX & _, acc: opX)

                last.line > first.line

    [
    , formatExpressionAndMaybeAddParens env (chainPrecedence opsAndRights) left
    , List.map formatOpAndRight opsAndRights...
    ]
    >> Fmt.spaceSeparatedOrStackForce forceMultiline __


formatCall as fn Env, FA.Expression, [ FA.Expression ]: Fmt.Block =
    fn env, ref, args:
    lastIndex =
        List.length args - 1

    asContinuingFn =
        fn index, arg:
        try arg as

            FA.'expression _ _ (FA.'fn layout params body):
                if index == lastIndex and layout /= FA.'inline then
                    'just (layout & params & body)
                else
                    'nothing

            _:
                'nothing

    formatArgument as fn Int, FA.Expression: Fmt.Block =
        fn index, arg:
        try asContinuingFn index arg as
            'nothing: formatExpressionAndMaybeAddParens env Op.precedence_application arg
            'just (layout & params & body): formatFunctionHeader env params

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
            , List.indexedMap formatArgument args...
            ]

    try maybeContinuing as

        'nothing:
            call

        'just (layout & params & body):
            Fmt.stack
                [
                , call
                , formatExpression env body >> applyIf (layout == FA.'indented) Fmt.indent
                ]
    >> stackWithComments env refComments __


extractIfElses as fn Env, FA.Expression: [ [ FA.Comment ] & Fmt.Block & Fmt.Block ] & Fmt.Block =
    fn env, x:
    rec =
        fn acc, expr:
        try expr as
            FA.'expression comments _ (FA.'if args): rec [ comments & formatExpression env args.condition & formatExpression env args.true, acc... ] args.false
            _: List.reverse acc & formatExpression env expr

    rec [] x


formatIf as fn Env, Bool, FA.Expression: Fmt.Block =
    fn env, isMultiline, expr:
    conditionsAndValues & default =
        extractIfElses env expr

    maybeSingleLine =
        if isMultiline then
            'nothing
        else
            try conditionsAndValues as

                [ [] & condition & valueIfTrue ]:
                    l =
                        Fmt.blockAsLine

                    try l condition & l valueIfTrue & l default as
                        'just conditionLine & 'just trueLine & 'just falseLine: 'just (conditionLine & trueLine & falseLine)
                        _: 'nothing

                _:
                    'nothing

    try maybeSingleLine as

        'just (conditionLine & trueLine & falseLine):
            [
            , Fmt.'text_ "if"
            , conditionLine
            , Fmt.'text_ "then"
            , trueLine
            , Fmt.'text_ "else"
            , falseLine
            ]
            >> List.intersperse Fmt.'space __
            >> Fmt.for1 __ (fn item, acc: Fmt.'row acc item)
            >> Fmt.lineToBlock

        'nothing:
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
        >> Maybe.toResult 'none __
        >> onOk fn paLine:
        Fmt.blockAsLine bl
        >> Maybe.toResult 'none __
        >> onOk fn blockLine:
        'ok (paLine & blockLine)

    blocks as [ Fmt.Block ] =
        try List.mapRes tryOneLine formatted as

            'ok lines:
                formatInline =
                    fn paLine & blockLine:
                    Fmt.'row paLine (Fmt.'row (Fmt.'text_ ": ") blockLine) >> Fmt.lineToBlock

                List.map formatInline lines

            # TODO restore `None` here once it doesn't break JS any more
            'err _:
                formatIndented =
                    fn paBlock & blockBlock:
                    [
                    , Fmt.blankLine
                    , paBlock >> Fmt.addSuffix (Fmt.'text_ ":") __
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
