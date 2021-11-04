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


getPos @state =
    as ReadState@: Int

    state.buffer.nextPos


addError message @state =
    as Text: ReadState@: None

    end =
        getPos @state

    error =
        Error.Simple (Pos.P state.moduleName state.start end) fn _: [ message ]

    @state.errors := error :: state.errors
    @state.start := end


setMode mode @state =
    as Mode: ReadState@: None

#    log "setMode: " mode
    @state.mode := mode


absAddToken =
    as Int: Int: Token.Kind: ReadState@: None

    fn start end kind @state:
#        log "absAddToken" ( Debug.toHuman start .. " " .. Debug.toHuman end .. " " .. Debug.toHuman kind )
        @state.tokens := Token start end kind :: state.tokens
        @state.start := end


relAddToken =
    as Int: Int: Token.Kind: ReadState@: None
    fn ds de kind @state:
        pos = getPos @state
        absAddToken (pos + ds) (pos + de) kind @state


addOneIndentToken =
    as Token.Kind: ReadState@: None
    fn kind @state:
        pos = getPos @state
        @state.tokens := Token pos pos kind :: state.tokens


getChunk =
    as ReadState@: Int & Int & Text
    fn @state:
        start = state.start
        end = getPos @state
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


addWordToken modifier @state =
    as Token.NameModifier: ReadState@: None

    start = state.start

    end = getPos @state

    ds = (if modifier == Token.NameNoModifier: 0 else 1)

    chunk = Buffer.slice (state.start + ds) end state.buffer

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
            absAddToken start end kind @state

        Just kind & _:
            addError (chunk .. " as a keyword, you can't really use it this way") @state

        _:
            absAddToken start end (Token.Name modifier chunk) @state


#
# Number literals
#
isNumber char =
    as Text: Bool

    Text.startsWithRegex "[0-9_.]" char /= ""


addNumberToken @state =
    as ReadState@: None

    start & end & chunk =
        getChunk @state

    # TODO ensure that there as one or zero dots
    # This as probably best done in lexOne

    # TODO ensure that there as at least one actual figure

    # TODO what about exponential notation?

    @state
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


addSquiggleToken nextIsSpace @state =
    as Bool: ReadState@: None

    start & end & chunk =
        getChunk @state

    add kind =
        absAddToken start end kind @state

    try chunk as
        ":": add << Token.Colon
        "@:": add << Token.MutableColon
        "=": add << Token.Defop { mutable = False }
        "@=": add << Token.Defop { mutable = True }
        "-": add << (if nextIsSpace: Token.Binop Prelude.subtract else Token.Unop Prelude.unaryMinus)
        "+": add << (if nextIsSpace: Token.Binop Prelude.add else Token.Unop Prelude.unaryPlus)
        op:
            try Dict.get chunk Prelude.binops as
                Just binop:
                    add << Token.Binop binop
                Nothing:
                    addError ("Invalid operator: `" .. chunk .. "`") @state


#
# Parens and comma
#
addParenOrCommaToken char @state =
    as Text: ReadState@: None

    add kind =
        relAddToken 0 1 kind @state

    try char as
        "(": add << Token.RoundParen Token.Open
        ")": add << Token.RoundParen Token.Closed
        "[": add << Token.SquareBracket Token.Open
        "]": add << Token.SquareBracket Token.Closed
        "{": add << Token.CurlyBrace Token.Open
        "}": add << Token.CurlyBrace Token.Closed
        ",": add << Token.Comma

        # This shuld never happen?
        _: addError ("I can't make sense of this piece of text: `" .. char .. "`") @state


#
# Outer machine state lexer
#
lexOne char @state =
    as Text: ReadState@: None

    # TODO rewrite the whole thing with ReadState mutable?

    # position of char
    pos =
        getPos @state

#    log "-> lexOne: " char

    try state.mode as

        Indent:
          try state.tabsOrSpaces as
            Tabs:
                tryIndent "\t" char @state

            Spaces:
                tryIndent " " char @state

            NoTabsOrSpacesYet:
              try char as
                  " ":
                      @state.tabsOrSpaces := Spaces
                      lexOne char @state
                  "\t":
                      @state.tabsOrSpaces := Tabs
                      lexOne char @state
                  _:
                      tryIndent " " char @state

        Default:
          try char as
            "":
                None

            ".":
                setMode Dot @state

            "@":
                setMode Mutable @state


            "#":
                setMode LineComment @state

            "[":
                setMode ContentOpeningBlockComment @state

            "\"":
                setMode ContentOpeningQuotes_One @state

            "\n":
                @state.start := getPos @state + 1
                setMode Indent @state

            " ":
                @state.start := getPos @state + 1

            _:
              if isWordStart char:
                  setMode (Word Token.NameNoModifier) @state

              else if isNumber char:
                  setMode NumberLiteral @state

              else if isSquiggle char:
                  setMode Squiggles @state

              else:
                  addParenOrCommaToken char @state

        Dot:
          if char == ".":
              relAddToken (0 - 1) 1 (Token.Binop Prelude.textConcat) @state
              setMode Default @state

          else if isWordStart char:
                  setMode (Word Token.NameStartsWithDot) @state

          else if isNumber char:
                  setMode NumberLiteral @state
          else
                  addError "no idea what this is" @state

        Mutable:
            if isWordStart char:
                    setMode (Word Token.NameMutable) @state

            else if isSquiggle char:
                    setMode Squiggles @state
            else
                    addError "no idea what this is" @state

        Word modifier:
            if isWordBody char:
              None

            else
                  addWordToken modifier @state
                  setMode Default @state
                  lexOne char @state

        NumberLiteral:
            if isNumber char:
              None

            else
                  addNumberToken @state
                  setMode Default @state
                  lexOne char @state

        Squiggles:
            if isSquiggle char:
              None

            else
                  addSquiggleToken (char == " ") @state
                  setMode Default @state
                  lexOne char @state

        ContentOpeningQuotes_One:
          if char == "\"":
                setMode ContentOpeningQuotes_Two @state

          else if char == "":
                  addError "there's no closing quotes" @state

          else:
                setMode (SingleQuote { lastEscape = -1 }) @state
                lexOne char @state

        ContentOpeningQuotes_Two:
          if char == "\"":
                setMode (TripleQuote { lastEscape = -1, closingQuotes = 0 }) @state
          else:
                # TODO replace with unary `-` once it works
                relAddToken (0 - 2) 0 (Token.TextLiteral "") @state
                setMode Default @state
                lexOne char @state

        SingleQuote { lastEscape }:
          previousIsEscape =
            pos == lastEscape + 1

          if char == "":
                addError "there's no closing quotes" @state

          else if previousIsEscape:
              setMode (SingleQuote { lastEscape }) @state

          else
            try char as
              "\"":
                  start = state.start
                  end = pos + 1

                  value =
                      Buffer.slice (start + 1) (end - 1) state.buffer

                  absAddToken start end (Token.TextLiteral value) @state
                  setMode Default @state

              "\\":
                  setMode (SingleQuote { lastEscape = pos }) @state

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
                None

        TripleQuote { lastEscape, closingQuotes }:
          previousIsEscape =
            pos == lastEscape + 1

          if char == "":
                  addError "unterminated triple quotes" @state

          else if previousIsEscape:
                  setMode (TripleQuote { lastEscape, closingQuotes = 0 }) @state

          else
             try char as
               "\"":
                  if closingQuotes == 2:
                    # TODO maybe move away the finalization code from here?
                    start = state.start
                    end = pos + 1

                    value =
                        # TODO also unindent
                        Buffer.slice (start + 3) (end - 3) state.buffer

                    absAddToken start end (Token.TextLiteral value) @state
                    setMode Default @state
                  else
                    setMode (TripleQuote { lastEscape, closingQuotes = closingQuotes + 1 }) @state

               "\\":
                    setMode (TripleQuote { lastEscape = pos, closingQuotes = 0 }) @state

               _:
                    setMode (TripleQuote { lastEscape, closingQuotes = 0 }) @state

        LineComment:
          if char == "\n" or char == "":
                  absAddToken state.start (getPos @state) Token.Comment @state
                  setMode Default @state
                  lexOne char @state
          else
              None

        ContentOpeningBlockComment:
          if char == "#":
                setMode (BlockComment { nesting = 1, previous = "" }) @state
          else:
                # TODO replace with -1 once it as supported
                relAddToken (0 - 1) 0 (Token.SquareBracket Token.Open) @state
                setMode Default @state
                lexOne char @state

        BlockComment { nesting, previous }:
          continueWithDeltaNesting dn =
                setMode (BlockComment { nesting = nesting + dn, previous = char }) @state

          try previous & char as
            "[" & "#":
                continueWithDeltaNesting 1

            "#" & "]":
                if nesting > 1:
                  continueWithDeltaNesting (0 - 1)
                else
                      absAddToken state.start (getPos @state) Token.Comment @state
                      setMode Default @state

            _ & "":
                    addError "unterminated block comment" @state

            _:
                continueWithDeltaNesting 0


tryIndent indentChar char @state =
    as Text: Text: ReadState@: None

    if char == indentChar or char == "":
        None

    else if char == " " or char == "\t":
        @state
            >> addError "mixing tabs and spaces!"

    else if char == "\n":
        # line as empty, ignore
        @state.start := getPos @state + 1
        setMode Indent @state

    else
            addIndentTokens @state
            setMode Default @state
            lexOne char @state


addIndentTokens @state =
    as ReadState@: None

    start =
        state.start

    end =
        getPos @state

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
        @state
            >> absAddToken start end Token.NewSiblingLine

    else if indentLength > lastIndent:
        [#
           ```
           lastIndent
             newIndent
           ```
        #]
        @state.indentStack := indentLength :: state.indentStack
        absAddToken start end Token.BlockStart @state
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
        dropIndentStack indentLength @state


dropIndentStack indentLength @state =
    as Int: ReadState@: None

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
        @state.indentStack := indentLength :: state.indentStack
        addOneIndentToken Token.BadIndent @state

    else if indentLength == lastIndent:
        [#
           ```
           indent
               lastIndent
                   indentDroppedByParentRecursion
               newIndent
           ```
        #]
        @state
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
        @state.indentStack := rest
        addOneIndentToken Token.BlockEnd @state
        dropIndentStack indentLength @state


closeOpenBlocks @state =
    as ReadState@: [ Token ]

    pos =
        getPos @state

    List.foldl (fn stack accum: Token pos pos Token.BlockEnd :: accum) state.indentStack state.tokens


lexer moduleName moduleCode =
    as Text: Text: Res [Token]

    state @= readStateInit moduleName moduleCode

    Text.forEach moduleCode fn char:
        lexOne char @state
        @state.buffer.nextPos += 1

    lexOne "" @state

    if state.errors == []:
        closeOpenBlocks @state
            >> List.reverse
            >> Ok
    else
        state.errors
            >> Error.Nested
            >> Err

