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


alias Indent = {
#    , pos as Int
#    , line as Int
    , indent as Int
    , isBlock as Bool
    }


alias ReadState =
    {
    , buffer as Buffer

    , column as Int
    , line as Int
    , lineIndent as Int

    , errors as [Error]

    # TODO get rid of this
    , moduleName as Text

    # indent / block stuff
    , soFarThereAreNoTokensInThisLine as Bool
    , indentStack as [Indent]
    , indentStartsABlock as Bool

    # state machine
    , mode as Mode
    , tokenStart as Int
    , tabsOrSpaces as TabsOrSpaces

    # accumulator
    , tokens as [Token]
    }


readStateInit moduleName moduleCode =
    as Text: Text: ReadState

    { buffer = Buffer.init moduleCode

    , column = 0
    , line = 0
    , lineIndent = 0

    , errors = []

    , soFarThereAreNoTokensInThisLine = True
    , indentStack = []
    , indentStartsABlock = True

    , mode = Indent
    , moduleName
    , tokenStart = 0
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
        Error.Simple (Pos.P state.moduleName state.tokenStart end) fn _: [ message ]

    @state.errors := error :: state.errors
    @state.tokenStart := end


setMode mode @state =
    as Mode: ReadState@: None

    @state.mode := mode


addIndentToken pos kind @state =
    as Int: Token.Kind: ReadState@: None
    @state.tokens := Token pos pos kind :: state.tokens


updateIndent start end kind @state =
    as Int: Int: Token.Kind: ReadState@: None

    #log "UPD" state.indentStack

    manageIndent head =
        #log "MAN" { li = state.lineIndent, he = head.indent }
        if state.lineIndent > head.indent:

            newIndent = {
                , isBlock = state.indentStartsABlock
                , indent = state.lineIndent
                }

            #log "NEW" newIndent

            @state.indentStack := newIndent :: state.indentStack

            if state.indentStartsABlock:
                addIndentToken start Token.BlockStart @state
            else
                None

        else
            # this means that state.lineIndent == head.indent
            if head.isBlock and kind /= Token.Comment:
                addIndentToken start Token.NewSiblingLine @state
            else
                None


    try state.indentStack as
        head :: tail:
            if state.lineIndent < head.indent:
                @state.indentStack := tail
                if head.isBlock:
                    addIndentToken start Token.BlockEnd @state
                else
                    None

                updateIndent start end kind @state
            else
                manageIndent head

        []:
            manageIndent { indent = 0, isBlock = True }


# TODO Rename to addContentToken
absAddToken =
    as Int: Int: Token.Kind: ReadState@: None
    fn start end kind @state:

        #log "ADD" { kind, col = state.column, li = state.lineIndent }
        if state.soFarThereAreNoTokensInThisLine:
            @state.soFarThereAreNoTokensInThisLine := False
            updateIndent start end kind @state
        else
            None

        indentStartsABlock =
            try kind as
                # maybe start block after these
                Token.Then: True
                Token.Else: True
                Token.As: True
                Token.Colon: True
                Token.MutableColon: True
                Token.Defop _: True
                Token.Comment: state.indentStartsABlock
                _: False

        @state.indentStartsABlock := indentStartsABlock
        @state.tokens := Token start end kind :: state.tokens
        @state.tokenStart := end


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
        start = state.tokenStart
        end = getPos @state
        start & end & Buffer.slice state.tokenStart end state.buffer


unindent raw =
    as Text: Text

    [#
          blah """hello"""

          meh """
              hello
              blah
              """
    #]

    if not Text.startsWith "\n" raw:
        raw
    else
        multilineText =
            Text.dropLeft 1 raw

        lines =
            Text.split "\n" multilineText

        countLeadingSpaces line =
            as Text: Int

            line
                >> Text.startsWithRegex "[ ]*"
                >> Text.length

        minLead =
            lines
                >> List.filter (fn s: Text.trimLeft s /= "")
                >> List.map countLeadingSpaces
                >> List.minimum
                >> Maybe.withDefault 0

        lines
            >> List.map (Text.dropLeft minLead)
            >> Text.join "\n"
            >> Text.replaceRegex "\n[ ]*$" ""


#
# Words (names, keywords, logical ops)
#
isWordStart char =
    as Text: Bool

    Text.startsWithRegex "[a-zA-Z._]" char /= ""


isWordBody char =
    as Text: Bool

    Text.startsWithRegex "[a-zA-Z./_0-9]" char /= ""


startsWithUpperChar s =
    as Text: Bool
    try Text.startsWithRegex "[A-Z]" s as
        "": False
        _: True


addLowerOrUpperWord start end modifier chunk @state =
    as Int: Int: Token.NameModifier: Text: ReadState @: None

    upperName maybeModule name =
        try modifier as
            Token.NameNoModifier:
                absAddToken start end (Token.UpperName maybeModule name) @state

            Token.NameStartsWithDot:
                addError ("Types or constructors can't start with `.` and attribute names can't start with an uppercase letter. =|") @state

            Token.NameMutable:
                addError ("Types or constructors can't be mutable on their own, only variables can!") @state

    lowerName maybeModule name attrs =
        if List.any startsWithUpperChar attrs:
            addError "attribute names must start with a lowercase letter" @state
        else
            if maybeModule /= Nothing and modifier /= Token.NameNoModifier:
                addError "can't use . or @ modifier on an imported value" @state
            else
                absAddToken start end (Token.LowerName modifier maybeModule name attrs) @state

    snips =
        Text.split "." chunk

    if List.any (fn s: s == "") snips:
        addError "use spaces around `..` to concatenate Text" @state
    else
      try snips as
        []:
            Debug.todo "should not happen"

        [ one ]:
            # value
            # Type
            # Constructor
            if startsWithUpperChar one:
                upperName Nothing one
            else
                lowerName Nothing one []

        first :: second :: more:
            # value.attr1
            # value.attr1.attr2
            # Module.value
            # Module.Type
            # Module.Constructor
            try startsWithUpperChar first & startsWithUpperChar second as
                # value.attr1
                # value.attr1.attr2
                False & False:
                    lowerName Nothing first (second :: more)

                # Module.value
                # Module.value.attr
                # Module.value.attr1.attr2
                True & False:
                    lowerName (Just first) second more

                True & True:
                    # Module.Type
                    # Module.Constructor
                    if more /= []:
                        addError ("Types and constructors can't have .attributes") @state
                    else
                        upperName (Just first) second

                False & True:
                    # something.Something
                    addError "Something wrong with uppercases?" @state


addWordToken modifier @state =
    as Token.NameModifier: ReadState@: None

    start = state.tokenStart

    end = getPos @state

    ds = (if modifier == Token.NameNoModifier: 0 else 1)

    chunk = Buffer.slice (state.tokenStart + ds) end state.buffer

    maybeKeywordKind =
        try chunk as
            "if": Just << Token.If
            "then": Just << Token.Then
            "else": Just << Token.Else
            "try": Just << Token.Try
            "as": Just << Token.As
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
            addLowerOrUpperWord start end modifier chunk @state


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
                @state.tokenStart := getPos @state
                setMode Mutable @state

            "#":
                setMode LineComment @state

            "[":
                setMode ContentOpeningBlockComment @state

            "\"":
                setMode ContentOpeningQuotes_One @state

            "\n":
                @state.tokenStart := getPos @state + 1
                @state.soFarThereAreNoTokensInThisLine := True
                setMode Indent @state

            " ":
                @state.tokenStart := getPos @state + 1

            _:
                @state.tokenStart := getPos @state
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
                @state.tokenStart := getPos @state - 1
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
                  start = state.tokenStart
                  end = pos + 1

                  value =
                      state.buffer
                          >> Buffer.slice (start + 1) (end - 1)
                          >> Text.replace "\\\"" "\""

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
                    start = state.tokenStart
                    end = pos + 1

                    value =
                        state.buffer
                            >> Buffer.slice (start + 3) (end - 3)
                            >> unindent

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
                  absAddToken state.tokenStart (getPos @state) Token.Comment @state
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
                      absAddToken state.tokenStart (getPos @state) Token.Comment @state
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
        # line is empty, ignore
        @state.tokenStart := getPos @state + 1
        setMode Indent @state

    else if char == "#":
        setMode LineComment @state
    else
        @state.lineIndent := state.column
        setMode Default @state
        lexOne char @state


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

        if char == "\n":
            @state.line += 1
            @state.column := 0
        else
            @state.column += 1


    lexOne "" @state

    if state.errors == []:
        closeOpenBlocks @state
            >> List.reverse
            >> Ok
    else
        state.errors
            >> Error.Nested
            >> Err

