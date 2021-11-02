#
# Lexer
#
union Mode =
    , Indent
    , Default
    , ContentOpeningQuotes_One
    , ContentOpeningQuotes_Two
    , ContentOpeningBlockComment
    , Dot
    , Mutable
    , Word Token.NameModifier
    , NumberLiteral
    , Squiggles
    , SingleQuote { lastEscape as Int }
    , TripleQuote { lastEscape as Int, closingQuotes as Int }
    , LineComment
    , BlockComment { nesting as Int, previous as Text }


union TabsOrSpaces =
    , NoTabsOrSpacesYet
    , Tabs
    , Spaces


alias ReadState =
    {
    , buffer as Buffer
    , errors as [Error]
    , indentStack as [Int]
    , mode as Mode
    , moduleName as Text
    , start as Int
    , tabsOrSpaces as TabsOrSpaces
    , tokens as [Token]
    }


readStateInit moduleName moduleCode =
    as Text: Text: ReadState

    { buffer = Buffer.init moduleCode
    , errors = []
    , indentStack = []
    , mode = Indent
    , moduleName = moduleName
    , start = 0
    , tabsOrSpaces = NoTabsOrSpacesYet
    , tokens = []
    }


getPos state =
    as ReadState: Int

    state.buffer.nextPos


addError message state =
    as Text: ReadState: ReadState

    end =
        getPos state

    error =
        Error.Simple (Pos.P state.moduleName state.start end) fn _: [ message ]

    { state with
    , errors = error :: .errors
    , start = end
    }


setMode mode state =
    as Mode: ReadState: ReadState

#    log "setMode: " mode
    { state with mode = mode }


absAddToken =
    as Int: Int: Token.Kind: ReadState: ReadState

    fn start end kind state:
#        log "absAddToken" ( Debug.toHuman start .. " " .. Debug.toHuman end .. " " .. Debug.toHuman kind )
        { state with
        , tokens = { start, end, kind } :: .tokens
        , start = end
        }


relAddToken =
    as Int: Int: Token.Kind: ReadState: ReadState
    fn ds de kind state:
        pos = getPos state
        absAddToken (pos + ds) (pos + de) kind state


addOneIndentToken =
    as Token.Kind: ReadState: ReadState
    fn kind state:
        pos = getPos state
        { state with tokens = { start = pos, end = pos, kind } :: .tokens }


getChunk =
    as ReadState: Int & Int & Text
    fn state:
        start = state.start
        end = getPos state
        start & end & Buffer.slice state.start end state.buffer


#
# Words (names, keywords, logical ops)
#
isWordStart char =
    as Text: Bool

    Text.startsWithRegex "[a-zA-Z._]" char /= ""


isWordBody char =
    as Text: Bool

    Text.startsWithRegex "[a-zA-Z./_0-9]" char /= ""


addWordToken modifier state =
    as Token.NameModifier: ReadState: ReadState

    start & end & chunk =
        getChunk state

    maybeKeywordKind =
        try chunk as
            "fn": Just << Token.Fn
            "if": Just << Token.If
            "try": Just << Token.Try
            "as": Just << Token.As
            "else": Just << Token.Else
            "with": Just << Token.With
            "and": Just << Token.Binop Prelude.and_
            "or": Just << Token.Binop Prelude.or_
            "not": Just << Token.Unop Prelude.not_
            _: Nothing

    try maybeKeywordKind & modifier as
        Just kind & Token.NameNoModifier:
            absAddToken start end kind state

        Just kind & _:
            state
              >> addError (chunk .. " as a keyword, you can't really use it this way")

        _:
            state
              >> absAddToken start end (Token.Name modifier chunk)


#
# Number literals
#
isNumber char =
    as Text: Bool

    Text.startsWithRegex "[0-9_.]" char /= ""


addNumberToken state =
    as ReadState: ReadState

    start & end & chunk =
        getChunk state

    # TODO ensure that there as one or zero dots
    # This as probably best done in lexOne

    # TODO ensure that there as at least one actual figure

    # TODO what about exponential notation?

    state
        >> absAddToken start end (Token.NumberLiteral chunk)


#
# Squiggles (ops and symbols)
#
isSquiggle char =
    as Text: Bool

    try char as
       "=": True
       ":": True
       "*": True
       "+": True
       "-": True
       "/": True
       ">": True
       "<": True
       "!": True
       "&": True
       "^": True
       _: False


addSquiggleToken nextIsSpace state =
    as Bool: ReadState: ReadState

    start & end & chunk =
        getChunk state

    add kind =
        absAddToken start end kind state

    try chunk as
        ":": add << Token.Colon
        "=": add << Token.Defop { mutable = False }
        "@=": add << Token.Defop { mutable = True }
        "->": add << Token.Arrow { mutable = False }
        "@>": add << Token.Arrow { mutable = True }
        "-": add << (if nextIsSpace: Token.Binop Prelude.subtract else Token.Unop Prelude.unaryMinus)
        "+": add << (if nextIsSpace: Token.Binop Prelude.add else Token.Unop Prelude.unaryPlus)
        op:
            try Dict.get chunk Prelude.binops as
                Just binop:
                    add << Token.Binop binop
                Nothing:
                    state
                        >> addError ("Invalid operator: `" .. chunk .. "`")


#
# Parens and comma
#
addParenOrCommaToken char state =
    as Text: ReadState: ReadState

    start & end & chunk =
        getChunk state

    add kind =
        absAddToken start end kind state

    try chunk as
        "(": add << Token.RoundParen Token.Open
        ")": add << Token.RoundParen Token.Closed
        "[": add << Token.SquareBracket Token.Open
        "]": add << Token.SquareBracket Token.Closed
        "{": add << Token.CurlyBrace Token.Open
        "}": add << Token.CurlyBrace Token.Closed
        ",": add << Token.Comma
        # This shuld never happen?
        _:
          state
              >> addError ("I can't make sense of this piece of text: `" .. chunk .. "`")


#
# Outer machine state lexer
#
lexOne char state =
    as Text: ReadState: ReadState

    # TODO rewrite the whole thing with ReadState mutable?

    # position of char
    pos =
        getPos state

#    log "-> lexOne: " char

    try state.mode as

        Indent:
          try state.tabsOrSpaces as
            Tabs:
                tryIndent "\t" char state

            Spaces:
                tryIndent " " char state

            NoTabsOrSpacesYet:
              try char as
                  " ": lexOne char { state with tabsOrSpaces = Spaces }
                  "\t": lexOne char { state with tabsOrSpaces = Tabs }
                  _: tryIndent " " char state

        Default:
          try char as
            "":
              state

            ".":
              state
                  >> setMode Dot

            "@":
              state
                  >> setMode Mutable


            "#":
              state
                  >> setMode LineComment

            "[":
              state
                  >> setMode ContentOpeningBlockComment

            "\n":
              { state with start = getPos state + 1 }
                  >> setMode Indent

            " ":
              { state with start = getPos state + 1 }

            _:
              if isWordStart char:
                  state
                      >> setMode (Word Token.NameNoModifier)

              else if isNumber char:
                  state
                      >> setMode NumberLiteral

              else if isSquiggle char:
                  state
                      >> setMode Squiggles

              else:
                  state
                      >> addParenOrCommaToken char

        Dot:
          if char == ".":
              state
                  >> relAddToken (0 - 2) 0 (Token.Binop Prelude.textConcat)

          else if isWordStart char:
              state
                  >> setMode (Word Token.NameStartsWithDot)

          else if isNumber char:
              state
                  >> setMode NumberLiteral
          else
              state
                  >> addError "no idea what this is"

        Mutable:
            if isWordStart char:
                state
                    >> setMode (Word Token.NameMutable)

            else if isSquiggle char:
                state
                    >> setMode Squiggles
            else
                state
                    >> addError "no idea what this is"

        Word modifier:
            if isWordBody char:
              state

            else
              state
                  >> addWordToken modifier
                  >> setMode Default
                  >> lexOne char

        NumberLiteral:
            if isNumber char:
              state

            else
              state
                  >> addNumberToken
                  >> setMode Default
                  >> lexOne char

        Squiggles:
            if isSquiggle char:
              state

            else
              state
                  >> addSquiggleToken (char == " ")
                  >> setMode Default
                  >> lexOne char

        ContentOpeningQuotes_One:
          if char == "\"":
            state
                >> setMode ContentOpeningQuotes_Two

          else if char == "":
              state
                  >> addError "there's no closing quotes"

          else:
            state
                >> setMode (SingleQuote { lastEscape = -1 })
                >> lexOne char

        ContentOpeningQuotes_Two:
          if char == "\"":
            state
                >> setMode (TripleQuote { lastEscape = -1, closingQuotes = 0 })
          else:
            state
                # TODO replace with unary `-` once it works
                >> relAddToken (0 - 2) 0 (Token.TextLiteral "")
                >> setMode Default
                >> lexOne char

        SingleQuote { lastEscape }:
          previousIsEscape =
            pos == lastEscape + 1

          if char == "":
            state
                >> addError "there's no closing quotes"

          else if previousIsEscape:
            state
              >> setMode (SingleQuote { lastEscape })

          else
            try char as
              "\"":
                  start = state.start
                  end = pos

                  value =
                      Buffer.slice start end state.buffer

                  state
                      >> absAddToken start end (Token.TextLiteral value)
                      >> setMode Default

              "\\":
                  state
                      >> setMode (SingleQuote { lastEscape = pos })

              [# This should actually be valid, but fixed by spfmt
              "\n":
                  [
                  , "Single-quoted text must fit in one line."
                  , "Is it possible you forgot a closing quote \"?"
                  , "If you want a Text with multiple lines, use the triple quotes \"\"\" instead."
                  ]
                      >> resError start state
              #]

              _:
                state

        TripleQuote { lastEscape, closingQuotes }:
          previousIsEscape =
            pos == lastEscape + 1

          if char == "":
              state
                  >> addError "unterminated triple quotes"

          else if previousIsEscape:
              state
                  >> setMode (TripleQuote { lastEscape, closingQuotes = 0 })

          else
             try char as
               "\"":
                  if closingQuotes == 2:
                    # TODO maybe move away the finalization code from here?
                    start = state.start
                    end = pos

                    value =
                        # TODO also unindent
                        Buffer.slice start end state.buffer

                    state
                        >> absAddToken start end (Token.TextLiteral value)
                        >> setMode Default
                  else
                    state
                        >> setMode (TripleQuote { lastEscape, closingQuotes = closingQuotes + 1 })
               "\\":
                    state
                        >> setMode (TripleQuote { lastEscape = pos, closingQuotes = 0 })

               _:
                    state
                        >> setMode (TripleQuote { lastEscape, closingQuotes = 0 })

        LineComment:
          if char == "\n" or char == "":
              state
                  >> absAddToken state.start (getPos state) Token.Comment
                  >> setMode Default
                  >> lexOne char
          else
              state

        ContentOpeningBlockComment:
          if char == "#":
            state
                >> setMode (BlockComment { nesting = 1, previous = "" })
          else:
            state
                # TODO replace with -1 once it as supported
                >> relAddToken (0 - 1) 0 (Token.SquareBracket Token.Open)
                >> setMode Default

        BlockComment { nesting, previous }:
          continueWithDeltaNesting dn =
                state >> setMode (BlockComment { nesting = nesting + dn, previous = char })

          try previous & char as
            "[" & "#": continueWithDeltaNesting 1


            "#" & "]":
                if nesting > 1:
                  continueWithDeltaNesting (0 - 1)
                else
                  state
                      >> absAddToken state.start (getPos state) Token.Comment
                      >> setMode Default

            _ & "":
                state
                    >> addError "unterminated block comment"

            _:
                continueWithDeltaNesting 0


tryIndent indentChar char state =
    as Text: Text: ReadState: ReadState

    if char == indentChar or char == "":
        state

    else if char == " " or char == "\t":
        state
            >> addError "mixing tabs and spaces!"

    else if char == "\n":
        # line as empty, ignore
        { state with
        , start = getPos state + 1
        }
            >> setMode Indent

    else
        state
            >> addIndentTokens
            >> setMode Default
            >> lexOne char


addIndentTokens state =
    as ReadState: ReadState

    start =
        state.start

    end =
        getPos state

    indentLength =
        end - start

    lastIndent =
        state.indentStack
            >> List.head
            >> Maybe.withDefault 0

#    log "addIndentTokens" << { indentLength, lastIndent, start, end }

    if indentLength == lastIndent:
        [#
           ```
           lastIndent
           newIndent
           ```
        #]
        state
            >> absAddToken start end Token.NewSiblingLine

    else if indentLength > lastIndent:
        [#
           ```
           lastIndent
             newIndent
           ```
        #]
        { state with indentStack = indentLength :: .indentStack }
            >> absAddToken start end Token.BlockStart
    else
        [#
           Back one or more indents:
           ```
           indent
               indent
                   lastIndent
               newIndent
           ```
        #]
        dropIndentStack indentLength state


dropIndentStack indentLength state =
    as Int: ReadState: ReadState

    lastIndent & rest =
        try state.indentStack as
            []: 0 & []
            head :: tail: head & tail

    if indentLength > lastIndent:
        [#
           This as a bad indent, but we can probably parse it anyway, so that spfmt can fix it
           ```
           indent
               lastIndent
                   indentDroppedByParentRecursion
                 newIndent
           ```
        #]
        { state with indentStack = indentLength :: .indentStack }
            >> addOneIndentToken Token.BadIndent

    else if indentLength == lastIndent:
        [#
           ```
           indent
               lastIndent
                   indentDroppedByParentRecursion
               newIndent
           ```
        #]
        state
            >> addOneIndentToken Token.NewSiblingLine
    else
        [#
           ```
           indent
            ...
                 last
               newIndent
           ```
        #]
        { state with indentStack = rest }
            >> addOneIndentToken Token.BlockEnd
            >> dropIndentStack indentLength


closeOpenBlocks state =
    as ReadState: [ Token ]

    blockEnd =
        as Token
        {
        , kind = Token.BlockEnd
        , start = getPos state
        , end = getPos state
        }

    List.foldl (fn stack accum: blockEnd :: accum) state.indentStack state.tokens


lexerStep state =
    as ReadState: Res [Token]

    try Buffer.readOne state.buffer as
        "" & _:
            exitState =
                lexOne "" state

            if exitState.errors == []:
                exitState
                    >> closeOpenBlocks
                    >> List.reverse
                    >> Ok
            else
                exitState.errors
                    >> Error.Nested
                    >> Err

        char & buffer:
            # first we lex, *then* we update the position
            state
                >> lexOne char
                >> fn b: { b with buffer = buffer }
                >> lexerStep


lexer moduleName moduleCode =
    as Text: Text: Res [Token]

    lexerStep << readStateInit moduleName moduleCode

