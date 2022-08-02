

# TODO: rewrite the whole thing so that each module has all the info and this type is not needed
alias Env = {
    , moduleByName as Dict Text { fsPath as Text, content as Text }
    }


alias Description =
    Env: [Text]


union Error =
    , Simple Pos Description
    , Nested [ Error ]


alias Res a =
    Result Error a


res as Pos: Description: Res a =
    pos: desc:
    Err << Simple pos desc


#
# Formatted text
#
union FormattedText =
    , FormattedText_Default Text
    , FormattedText_Emphasys Text
    , FormattedText_Warning Text
    , FormattedText_Decoration Text


toFormattedText as Env: Error: [FormattedText] =
    eenv: e:

    newline =
        FormattedText_Default ""

    tupleToFormattedText =
        x:
        pos & descr = x
        toText eenv pos descr

    flatten e []
        >> List.concatMap tupleToFormattedText


#
# It would be more reliable to declare errors diectly in FormattedText, but I tried it and it was a pain.
# So until I have a better idea, errors are declared as strings and we'll need to transform them back to FormattedText
#
formatSeparator as Text = "$|$|$"
formatSuffix as Text = "$`$`$"
formatWrap as Text: Text: Text = fmtName: text: formatSeparator .. fmtName .. formatSuffix .. text .. formatSeparator

breakDownText as Text: [FormattedText] =
  text:

  formatSnippet as Int: Text: FormattedText =
      index: snippet:

      if modBy 2 index == 0 then
          FormattedText_Default snippet
      else
          try Text.split formatSuffix snippet as
              ["emphasys", s]: FormattedText_Emphasys s
              ["warning", s]: FormattedText_Warning s
              ["decoration", s]: FormattedText_Decoration s
              _: FormattedText_Default snippet

  text
      >> Text.split formatSeparator
      >> List.indexedMap formatSnippet

emph as Text: Text =
    formatWrap "emphasys"

warn as Text: Text =
    formatWrap "warning"

deco as Text: Text =
    formatWrap "decoration"


#
#
#

union Highlight =
    , HighlightWord { line as Number, colStart as Number, colEnd as Number }
    , HighlightBlock { lineStart as Number, lineEnd as Number }



###


flatten as Error: [Pos & Description]: [Pos & Description] =
    e: accum:
    try e as
        Simple pos descr:
            pos & descr :: accum

        Nested ls:
            List.for ls flatten accum


#
# toText
#


positionToLineAndColumn as Text: Int: { line as Int, col as Int } =
    s: index:

    before =
        Text.slice 0 index s

    lines =
       Text.split "\n" before

    lineNumber =
        List.length lines

    colNumber =
        lines
            >> List.last
            >> Maybe.map Text.length
            >> Maybe.withDefault 0

    { line = lineNumber, col = colNumber }


highlightSplit as Highlight: ( Dict Int ( Int & Int ) & Set Int ): ( Dict Int ( Int & Int ) & Set Int ) =
    h: x:
    words & lines = x
    try h as
        HighlightWord { line, colStart, colEnd }:
            ( Dict.insert line ( colStart & colEnd ) words & lines)

        HighlightBlock { lineStart, lineEnd }:
            ( words & (List.for (List.range lineStart lineEnd) Set.insert lines))


fmtBlock as Int: [Highlight]: [Text]: Text =
    start: highlights: ls:

    ( highlightedWords & highlightedLines ) =
        List.for highlights highlightSplit ( Dict.empty & Set.empty )

    pad =
        (start + List.length ls)
            >> Text.fromNumber
            >> Text.length

    wordHighlight =
        lineNumber:
        try Dict.get lineNumber highlightedWords as
            Nothing:
                ""

            Just ( s & e ):
                "\n"
                    .. Text.repeat pad " "
                    .. "   "
                    .. Text.repeat s " "
                    .. warn (Text.repeat (max 1 << e - s) "^")

    lineDem =
        lineIndex:
        if Set.member lineIndex highlightedLines then
            warn " > "

        else
            " | "

    fmtLine =
        i: line:

        index =
            i + start

        s =
            index
                >> Text.fromNumber
                >> Text.padLeft pad " "

        s
            .. lineDem index
            .. line
            .. wordHighlight index

    ls
        >> List.indexedMap fmtLine
        >> Text.join "\n"
        >> (s: s .. "\n")


showCodeBlock as Text: { line as Int, col as Int }: { line as Int, col as Int }: Text =
    code: start: end:

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
            start.line - extraLines - 1 >> clamp 0 (maxLines - 1)

        endLine =
            end.line + extraLines >> clamp 0 (maxLines - 1)

        size =
            max 1 << endLine - startLine

        lines
            >> List.drop startLine
            >> List.take size
            >> fmtBlock (startLine + 1) [ highlight ]


posToHuman as Env: Pos: { location as Text, block as Text } =
    eEnv: pos:

    noBlock =
        loc:
        { location = loc
        , block = ""
        }

    try pos as
        Pos.P moduleName startAsInt endAsInt:
            try Dict.get moduleName eEnv.moduleByName as
                Just mod:
                    start =
                        positionToLineAndColumn mod.content startAsInt

                    end =
                        positionToLineAndColumn mod.content endAsInt

                    { location = mod.fsPath .. " " .. Text.fromNumber start.line .. ":" .. Text.fromNumber start.col
                    , block = showCodeBlock mod.content start end
                    }

                Nothing:
                    noBlock << "<The module name is `" .. moduleName .. "` but I can't find it. This as a compiler bug.>"

        Pos.End moduleName:
            try Dict.get moduleName eEnv.moduleByName as
                Just mod:

                    end =
                        positionToLineAndColumn mod.content << Text.length mod.content - 1

                    start =
                        { line = end.line - 8
                        , col = 0
                        }

                    { location = mod.fsPath .. " " .. Text.fromNumber end.line .. ":0 (end of file)"
                    , block = showCodeBlock mod.content start end
                    }

                Nothing:
                    noBlock << "<The module name is `" .. moduleName .. "` but I can't find it. This as a compiler bug.>"

        Pos.N:
            noBlock "<native code>"

        Pos.S:
            noBlock "<the location information has been stripped>"

        Pos.T:
            noBlock "<defined in test modules>"

        Pos.I n:
            noBlock << "<inferred " .. Text.fromNumber n .. ">"

        Pos.G:
            noBlock "<generated>"



toText as Env: Pos: Description: [FormattedText] =
    env: pos: desc:

    { location, block } =
        posToHuman env pos

    description =
        env
            >> desc
            >> (d: block :: d)
            >> List.concatMap (Text.split "\n")
            >> List.map (s: "  " .. s)
            >> Text.join "\n"

    [
    , ""
    , ""
    , deco << Text.padRight 50 "-" (location .. " ")
    , ""
    , description
    , ""
    ]
        >> Text.join "\n"
        >> breakDownText

