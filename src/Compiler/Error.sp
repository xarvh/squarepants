Module =
    {
    , content as Text
    , fsPath as Text
    }


var Error =
    , 'raw [ Text ]
    , 'simple Module Pos [ Text ]
    , 'nested [ Error ]


Res a =
    Result Error a


res as fn Module, Pos, [ Text ]: Res a =
    fn mod, pos, desc:
    'err << 'simple mod pos desc


#
# Formatted text
#
var FormattedText =
    , 'formattedText_Default Text
    , 'formattedText_Emphasys Text
    , 'formattedText_Warning Text
    , 'formattedText_Decoration Text


toFormattedText as fn Error: [ FormattedText ] =
    flatten __ []


flatten as fn Error, [ FormattedText ]: [ FormattedText ] =
    fn e, accum:
    try e as
        'simple mod pos desc: List.concat [ accum, simpleToText mod pos desc ]
        'raw desc: List.concat [ accum, rawToText desc ]
        'nested ls: List.for accum ls flatten


#
# It would be more reliable to declare errors diectly in FormattedText, but I tried it and it was a pain.
# So until I have a better idea, errors are declared as strings and we'll need to transform them back to FormattedText
#
formatSeparator as Text =
    "$|$|$"


formatSuffix as Text =
    "$`$`$"


formatWrap as fn Text, Text: Text =
    fn fmtName, text: formatSeparator .. fmtName .. formatSuffix .. text .. formatSeparator


breakDownText as fn Text: [ FormattedText ] =
    fn text:
    formatSnippet as fn Int, Text: FormattedText =
        fn index, snippet:
        if modBy 2 index == 0 then
            'formattedText_Default snippet
        else
            try Text.split formatSuffix snippet as
                [ "emphasys", s ]: 'formattedText_Emphasys s
                [ "warning", s ]: 'formattedText_Warning s
                [ "decoration", s ]: 'formattedText_Decoration s
                _: 'formattedText_Default snippet

    text
    >> Text.split formatSeparator __
    >> List.indexedMap formatSnippet __


emph as fn Text: Text =
    formatWrap "emphasys" __


warn as fn Text: Text =
    formatWrap "warning" __


deco as fn Text: Text =
    formatWrap "decoration" __


#
#
#

var Highlight =
    , 'highlightWord { colEnd as Number, colStart as Number, line as Number }
    , 'highlightBlock { lineEnd as Number, lineStart as Number }


#
# simpleToText
#

positionToLineAndColumn as fn Text, Int: { col as Int, line as Int } =
    fn s, index:
    before =
        Text.slice 0 index s

    lines =
        Text.split "\n" before

    lineNumber =
        List.length lines

    colNumber =
        lines
        >> List.last
        >> Maybe.map Text.length __
        >> Maybe.withDefault 0 __

    { col = colNumber, line = lineNumber }


highlightSplit as fn Highlight, Dict Int (Int & Int) & Set Int: Dict Int (Int & Int) & Set Int =
    fn h, x:
    words & lines =
        x

    try h as
        'highlightWord { colEnd, colStart, line }: Dict.insert line (colStart & colEnd) words & lines
        'highlightBlock { lineEnd, lineStart }: words & List.for lines (List.range lineStart lineEnd) Set.insert


fmtBlock as fn Int, [ Highlight ], [ Text ]: Text =
    fn start, highlights, ls:
    highlightedWords & highlightedLines =
        List.for (Dict.empty & Set.empty) highlights highlightSplit

    pad =
        start + List.length ls
        >> Text.fromNumber
        >> Text.length

    wordHighlight =
        fn lineNumber:
        try Dict.get lineNumber highlightedWords as

            'nothing:
                ""

            'just (s & e):
                "\n"
                .. Text.repeat pad " "
                .. "    "
                .. Text.repeat (s - 1) " "
                .. warn (Text.repeat (max 1 (e - s)) "^")

    lineDem =
        fn lineIndex:
        if Set.member lineIndex highlightedLines then
            warn " > "
        else
            " | "

    fmtLine =
        fn i, line:
        index =
            i + start

        s =
            index
            >> Text.fromNumber
            >> Text.padLeft pad " " __

        s
        .. lineDem index
        .. line
        .. wordHighlight index

    ls
    >> List.indexedMap fmtLine __
    >> Text.join "\n" __
    >> (fn s: s .. "\n")


showCodeBlock as fn Text, { col as Int, line as Int }, { col as Int, line as Int }: Text =
    fn code, start, end:
    if end.line < 0 then
        ""
    else
        highlight =
            if start.line /= end.line then
                'highlightBlock { lineEnd = end.line, lineStart = start.line }
            else
                'highlightWord { colEnd = end.col, colStart = start.col, line = start.line }

        extraLines =
            2

        lines =
            Text.split "\n" code

        maxLines =
            List.length lines

        startLine =
            clamp 0 (maxLines - 1) (start.line - extraLines - 1)

        endLine =
            clamp 0 (maxLines - 1) (end.line + extraLines)

        size =
            max 1 (endLine - startLine)

        lines
        >> List.drop startLine __
        >> List.take size __
        >> fmtBlock (startLine + 1) [ highlight ] __


posToHuman as fn Module, Pos: { block as Text, location as Text } =
    fn mod, pos:
    noBlock =
        fn loc:
        {
        , block = ""
        , location = loc
        }

    try pos as

        Pos.'p startAsInt endAsInt:
            start =
                positionToLineAndColumn mod.content startAsInt

            end =
                positionToLineAndColumn mod.content endAsInt

            {
            , block = showCodeBlock mod.content start end
            , location = mod.fsPath .. " " .. Text.fromNumber start.line .. ":" .. Text.fromNumber start.col
            }

        Pos.'end:
            end =
                positionToLineAndColumn mod.content (Text.length mod.content - 1)

            start =
                {
                , col = 0
                , line = end.line - 8
                }

            {
            , block = showCodeBlock mod.content start end
            , location = mod.fsPath .. " " .. Text.fromNumber end.line .. ":0 (end of file)"
            }

        Pos.'n:
            noBlock "<native code>"

        Pos.'s:
            noBlock "<the location information has been stripped>"

        Pos.'t:
            noBlock "<defined in test modules>"

        Pos.'i n:
            noBlock << "<inferred " .. Text.fromNumber n .. ">"

        Pos.'g:
            noBlock "<generated>"


simpleToText as fn Module, Pos, [ Text ]: [ FormattedText ] =
    fn mod, pos, desc:
    { block, location } =
        posToHuman mod pos

    description =
        [ block, desc... ]
        >> List.concatMap (Text.split "\n" __) __
        >> List.map (fn s: "  " .. s) __
        >> Text.join "\n" __

    [
    , ""
    , ""
    , deco << Text.padRight 50 "-" (location .. " ")
    , ""
    , description
    , ""
    ]
    >> Text.join "\n" __
    >> breakDownText


rawToText as fn [ Text ]: [ FormattedText ] =
    fn desc:
    description =
        desc
        >> List.concatMap (Text.split "\n" __) __
        >> List.map (fn s: "  " .. s) __
        >> Text.join "\n" __

    [
    , ""
    , ""
    , description
    , ""
    ]
    >> Text.join "\n" __
    >> breakDownText
