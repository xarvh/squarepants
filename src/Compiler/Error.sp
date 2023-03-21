

alias Module =
    {
    , fsPath as Text
    , content as Text
    }


union Error =
    , Raw [Text]
    , Simple Module Pos [Text]
    , Nested [ Error ]


alias Res a =
    Result Error a


res as fn Module, Pos, [Text]: Res a =
    fn mod, pos, desc:
    Err << Simple mod pos desc


#
# Formatted text
#
union FormattedText =
    , FormattedText_Default Text
    , FormattedText_Emphasys Text
    , FormattedText_Warning Text
    , FormattedText_Decoration Text


toFormattedText as fn Error: [FormattedText] =
    flatten __ []


flatten as fn Error, [FormattedText]: [FormattedText] =
    fn e, accum:

    try e as
        , Simple mod pos desc:
            List.concat [ accum, simpleToText mod pos desc ]

        , Raw desc:
            List.concat [ accum, rawToText desc ]

        , Nested ls:
            List.for accum ls flatten


#
# It would be more reliable to declare errors diectly in FormattedText, but I tried it and it was a pain.
# So until I have a better idea, errors are declared as strings and we'll need to transform them back to FormattedText
#
formatSeparator as Text = "$|$|$"
formatSuffix as Text = "$`$`$"
formatWrap as fn Text, Text: Text = fn fmtName, text: formatSeparator .. fmtName .. formatSuffix .. text .. formatSeparator

breakDownText as fn Text: [FormattedText] =
  fn text:

  formatSnippet as fn Int, Text: FormattedText =
      fn index, snippet:

      if modBy 2 index == 0 then
          FormattedText_Default snippet
      else
          try Text.split formatSuffix snippet as
              , ["emphasys", s]: FormattedText_Emphasys s
              , ["warning", s]: FormattedText_Warning s
              , ["decoration", s]: FormattedText_Decoration s
              , _: FormattedText_Default snippet

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

union Highlight =
    , HighlightWord { line as Number, colStart as Number, colEnd as Number }
    , HighlightBlock { lineStart as Number, lineEnd as Number }



#
# simpleToText
#


positionToLineAndColumn as fn Text, Int: { line as Int, col as Int } =
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

    { line = lineNumber, col = colNumber }


highlightSplit as fn Highlight, ( Dict Int ( Int & Int ) & Set Int ): ( Dict Int ( Int & Int ) & Set Int ) =
    fn h, x:
    words & lines = x
    try h as
        , HighlightWord { line, colStart, colEnd }:
            ( Dict.insert line ( colStart & colEnd ) words & lines)

        , HighlightBlock { lineStart, lineEnd }:
            ( words & (List.for lines (List.range lineStart lineEnd) Set.insert))


fmtBlock as fn Int, [Highlight], [Text]: Text =
    fn start, highlights, ls:

    ( highlightedWords & highlightedLines ) =
        List.for ( Dict.empty & Set.empty ) highlights highlightSplit

    pad =
        (start + List.length ls)
        >> Text.fromNumber
        >> Text.length

    wordHighlight =
        fn lineNumber:
        try Dict.get lineNumber highlightedWords as
            , Nothing:
                ""

            , Just ( s & e ):
                "\n"
                .. Text.repeat pad " "
                .. "   "
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


showCodeBlock as fn Text, { line as Int, col as Int }, { line as Int, col as Int }: Text =
    fn code, start, end:

    if end.line < 0 then
        ""

    else
        highlight =
            if start.line /= end.line then
                HighlightBlock { lineStart = start.line, lineEnd = end.line }

            else
                HighlightWord { line = start.line, colStart = start.col, colEnd = end.col }

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


posToHuman as fn Module, Pos: { location as Text, block as Text } =
    fn mod, pos:

    noBlock =
        fn loc:
        { location = loc
        , block = ""
        }

    try pos as
        , Pos.P startAsInt endAsInt:

            start =
                positionToLineAndColumn mod.content startAsInt

            end =
                positionToLineAndColumn mod.content endAsInt

            { location = mod.fsPath .. " " .. Text.fromNumber start.line .. ":" .. Text.fromNumber start.col
            , block = showCodeBlock mod.content start end
            }


        , Pos.End:

            end =
                positionToLineAndColumn mod.content (Text.length mod.content - 1)

            start =
                { line = end.line - 8
                , col = 0
                }

            { location = mod.fsPath .. " " .. Text.fromNumber end.line .. ":0 (end of file)"
            , block = showCodeBlock mod.content start end
            }


        , Pos.N:
            noBlock "<native code>"

        , Pos.S:
            noBlock "<the location information has been stripped>"

        , Pos.T:
            noBlock "<defined in test modules>"

        , Pos.I n:
            noBlock << "<inferred " .. Text.fromNumber n .. ">"

        , Pos.G:
            noBlock "<generated>"



simpleToText as fn Module, Pos, [Text]: [FormattedText] =
    fn mod, pos, desc:

    { location, block } =
        posToHuman mod pos

    description =
        [block, ...desc]
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


rawToText as fn [Text]: [FormattedText] =
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

