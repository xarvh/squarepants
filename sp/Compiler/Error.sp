
alias Env =
    {
#    , metaFile is MetaFile
    , moduleByName is Dict Text { fsPath is Text, content is Text }
    }

alias Description = Env -> [Text]

union Error =
    , Simple Pos Description
    , Nested [ Error ]


alias Res a =
    Result Error a


res pos desc =
    is Pos -> Description -> Res a

    Err << Simple pos desc


###



union Highlight =
    , HighlightWord { line is Number, colStart is Number, colEnd is Number }
    , HighlightBlock { lineStart is Number, lineEnd is Number }



###


flatten e accum =
    is Error -> [Pos & Description] -> [Pos & Description]
    try e as
        Simple pos descr:
            pos & descr :: accum

        Nested ls:
            List.foldl flatten ls accum


#
# toText
#


positionToLineAndColumn s index =
    is Text -> Int -> { line is Int, col is Int }

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
    is Highlight -> ( Dict Int ( Int & Int ) & Set Int ) -> ( Dict Int ( Int & Int ) & Set Int )
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
    is Int -> [Highlight] -> [Text] -> Text

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
                    .. Text.repeat (max 1 << e - s) "^"

    lineDem lineIndex =
        if Set.member lineIndex highlightedLines:
            " > "

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
    is Text -> { line is Int, col is Int } -> { line is Int, col is Int } -> Text
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
            endLine - startLine

        lines
            >> List.drop startLine
            >> List.take size
            >> fmtBlock (startLine + 1) [ highlight ]





posToHuman eEnv pos =
    is Env -> Pos -> { location is Text, block is Text }

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
                    noBlock << "<The module name is `" .. moduleName .. "` but I can't find it. This is a compiler bug.>"

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
    is Env -> Pos -> Description -> Text

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
    , Text.padRight 50 "-" (location .. " ")
    , ""
    , description
    , ""
    ]
        >> Text.join "\n"



