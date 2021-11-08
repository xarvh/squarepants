
alias Env =
    {
#    , metaFile as MetaFile
    , moduleByName as Dict Text { fsPath as Text, content as Text }
    }

alias Description = Env: [Text]

union Error =
    , Simple Pos Description
    , Nested [ Error ]


alias Res a =
    Result Error a


res pos desc =
    as Pos: Description: Res a

    Err << Simple pos desc


#
# Formatted text
#
union FormattedText =
    , FormattedText_Default Text
    , FormattedText_Emphasys Text
    , FormattedText_Warning Text
    , FormattedText_Decoration Text


toFormattedText eenv e =
    as Env: Error: [FormattedText]

    newline =
        FormattedText_Default ""

    tupleToFormattedText (pos & descr) =
        toText eenv pos descr

    flatten e []
        >> List.concatMap tupleToFormattedText


#
# It would be more reliable to declare errors diectly in FormattedText, but I tried it and it was a pain.
# So until I have a better idea, errors are declared as strings and we'll need to transform them back to FormattedText
#
formatSeparator = "$|$|$"
formatSuffix = "$`$`$"
formatWrap fmtName text = formatSeparator .. fmtName .. formatSuffix .. text .. formatSeparator

breakDownText text =
  as Text: [FormattedText]

  formatSnippet index snippet =
      as Int: Text: FormattedText

      if modBy 2 index == 0:
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

emph =
    formatWrap "emphasys"

warn =
    formatWrap "warning"

deco =
    formatWrap "decoration"


#
#
#

union Highlight =
    , HighlightWord { line as Number, colStart as Number, colEnd as Number }
    , HighlightBlock { lineStart as Number, lineEnd as Number }



###


flatten e accum =
    as Error: [Pos & Description]: [Pos & Description]
    try e as
        Simple pos descr:
            pos & descr :: accum

        Nested ls:
            List.foldl flatten ls accum


#
# toText
#


positionToLineAndColumn s index =
    as Text: Int: { line as Int, col as Int }

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


highlightSplit h ( words & lines ) =
    as Highlight: ( Dict Int ( Int & Int ) & Set Int ): ( Dict Int ( Int & Int ) & Set Int )
    try h as
        HighlightWord { line, colStart, colEnd }:
            ( Dict.insert line ( colStart & colEnd ) words
             & lines
            )

        HighlightBlock { lineStart, lineEnd }:
            ( words
             & (List.foldl Set.insert (List.range lineStart lineEnd) lines)
            )


fmtBlock start highlights ls =
    as Int: [Highlight]: [Text]: Text

    ( highlightedWords & highlightedLines ) =
        List.foldl highlightSplit highlights ( Dict.empty & Set.empty )

    pad =
        (start + List.length ls)
            >> Text.fromInt
            >> Text.length

    wordHighlight lineNumber =
        try Dict.get lineNumber highlightedWords as
            Nothing:
                ""

            Just ( s & e ):
                "\n"
                    .. Text.repeat pad " "
                    .. "   "
                    .. Text.repeat (s - 1) " "
                    .. warn (Text.repeat (max 1 << e - s) "^")

    lineDem lineIndex =
        if Set.member lineIndex highlightedLines:
            warn " > "

        else
            " | "

    fmtLine i line =
        index =
            i + start

        (index
            >> Text.fromInt
            >> Text.padLeft pad " "
        )
            .. lineDem index
            .. line
            .. wordHighlight index

    ls
        >> List.indexedMap fmtLine
        >> Text.join "\n"
        >> (fn s: s .. "\n")


showCodeBlock code start end =
    as Text: { line as Int, col as Int }: { line as Int, col as Int }: Text
    if end.line < 0:
        ""

    else
        highlight =
            if start.line /= end.line:
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





posToHuman eEnv pos =
    as Env: Pos: { location as Text, block as Text }

    noBlock loc =
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

                    { location = mod.fsPath .. " " .. Text.fromInt start.line .. ":" .. Text.fromInt start.col
                    , block = showCodeBlock mod.content start end
                    }

                Nothing:
                    noBlock << "<The module name as `" .. moduleName .. "` but I can't find it. This as a compiler bug.>"

        Pos.N:
            noBlock "<native code>"

        Pos.S:
            noBlock "<the location information has been stripped>"

        Pos.T:
            noBlock "<defined in test modules>"

        Pos.I n:
            noBlock << "<inferred " .. Text.fromInt n .. ">"

        Pos.E:
            noBlock "<errorTodo, get rid of me!>"

        Pos.F:
            noBlock "<FormattableToCanonicalAst todo, get rid of me!>"

        Pos.G:
            noBlock "<global value defined in the meta.json>"

        Pos.U:
            noBlock "<union type, get rid of me!>"





toText env pos desc =
    as Env: Pos: Description: [FormattedText]

    { location, block } =
        posToHuman env pos

    description =
        env
            >> desc
            >> (fn d: block :: d)
            >> List.concatMap (Text.split "\n")
            >> List.map (fn s: "  " .. s)
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

