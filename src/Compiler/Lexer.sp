#
# Lexer
#
union Mode =
    , Indent
    , Default
    , ContentOpeningQuotes_One
    , ContentOpeningQuotes_Two
    , ContentOpeningBlockComment
    , Dot_One
    , Dot_Two
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


alias ReadState = {
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
    , tokens as Array Token
    }


readStateInit as Text: Text: ReadState =
    moduleName: moduleCode: {

    , buffer = Buffer.init moduleCode

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
    , tokens = Array.fromList []
    }


getPos as ReadState@: Int =
    state@:
    state.buffer.nextPos


addError as Text: ReadState@: None =
    message: state@:

    end =
        getPos @state

    error =
        Error.Simple (Pos.P state.moduleName state.tokenStart end) _: [ message ]

    @state.errors := error :: state.errors
    @state.tokenStart := end


setMode as Mode: ReadState@: None =
    mode: state@:

    @state.mode := mode


addIndentToken as Int: Token.Kind: ReadState@: None =
    pos: kind: state@:
    Array.push @state.tokens << Token Token.N pos pos kind


updateIndent as Int: Int: Token.Kind: ReadState@: None =
    start: end: kind: state@:

    #log "UPD" state.indentStack

    manageIndent = head:
        #log "MAN" { li = state.lineIndent, he = head.indent }
        if state.lineIndent > head.indent then

            newIndent = {
                , isBlock = state.indentStartsABlock
                , indent = state.lineIndent
                }

            #log "NEW" newIndent

            @state.indentStack := newIndent :: state.indentStack

            if state.indentStartsABlock then
                addIndentToken start Token.BlockStart @state
            else
                None

        else
            # this means that state.lineIndent == head.indent
#            if head.isBlock then #and kind /= Token.Comment then
#                addIndentToken start Token.NewSiblingLine @state
#            else
                None


    try state.indentStack as
        head :: tail:
            if state.lineIndent < head.indent then
                @state.indentStack := tail
                if head.isBlock then
                    addIndentToken start Token.BlockEnd @state
                else
                    None

                updateIndent start end kind @state
            else
                manageIndent head

        []:
            manageIndent { indent = 0, isBlock = True }


# TODO Rename to addContentToken
absAddToken as Int: Int: Token.Kind: ReadState@: None =
    start: end: kind: state@:

    #log "ADD" { kind, col = state.column, li = state.lineIndent }
    if state.soFarThereAreNoTokensInThisLine then
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
#            Token.ConsumingColon: True
            Token.Defop: True
#            Token.Comment: state.indentStartsABlock
            _: False

    @state.indentStartsABlock := indentStartsABlock
    Array.push @state.tokens << Token Token.N start end kind
    @state.tokenStart := end


relAddToken as Int: Int: Token.Kind: ReadState@: None =
    ds: de: kind: state@:
    pos = getPos @state
    absAddToken (pos + ds) (pos + de) kind @state


addOneIndentToken as Token.Kind: ReadState@: None =
    kind: state@:
    pos = getPos @state
    Array.push @state.tokens << Token Token.N pos pos kind


getChunk as ReadState@: Int & Int & Text =
    state@:
    start = state.tokenStart
    end = getPos @state
    start & end & Buffer.slice state.tokenStart end state.buffer


unindent as Text: Text =
    raw:

    [#
          blah """hello"""

          meh """
              hello
              blah
              """
    #]

    if not << Text.startsWith "\n" raw then
        raw
    else
        multilineText =
            Text.dropLeft 1 raw

        lines =
            Text.split "\n" multilineText

        countLeadingSpaces as Text: Int =
            line:
            line
                >> Text.startsWithRegex "[ ]*"
                >> Text.length

        minLead =
            lines
                >> List.filter (s: Text.trimLeft s /= "")
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
isWordStart as Text: Bool =
    char:
    Text.startsWithRegex "[a-zA-Z._]" char /= ""


isWordBody as Text: Bool =
    char:
    Text.startsWithRegex "[a-zA-Z./_0-9]" char /= ""


startsWithUpperChar as Text: Bool =
    s:
    try Text.startsWithRegex "[A-Z]" s as
        "": False
        _: True


addLowerOrUpperWord as Int: Int: Token.NameModifier: Text: ReadState @: None =
    start: end: modifier: chunk: state@:

    upperName =
        maybeModule: name:
        try modifier as
            Token.NameNoModifier:
                word as Token.Word =
                    {
                    , modifier
                    , isUpper = True
                    , maybeModule
                    , name
                    , attrPath = []
                    }

                absAddToken start end (Token.Word word) @state

            Token.NameStartsWithDot:
                addError ("Types or constructors can't start with `.` and attribute names can't start with an uppercase letter. =|") @state

#            Token.NameMutable:
#                absAddToken start end (Token.UpperName maybeModule name) @state

    lowerName =
        maybeModule: name: attrs:
        if List.any startsWithUpperChar attrs then
            addError "attribute names must start with a lowercase letter" @state
        else
            if maybeModule /= Nothing and modifier /= Token.NameNoModifier then
                addError "can't use . or @ modifier on an imported value" @state
            else
                word as Token.Word =
                    {
                    , modifier
                    , isUpper = False
                    , maybeModule
                    , name
                    , attrPath = attrs
                    }
                absAddToken start end (Token.Word word) @state

    snips =
        Text.split "." chunk

    if List.any (s: s == "") snips then
        addError "use spaces around `..` to concatenate Text" @state
    else
      try snips as
        []:
            todo "should not happen"

        [ one ]:
            # value
            # Type
            # Constructor
            if startsWithUpperChar one then
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
                    if more /= [] then
                        addError ("Types and constructors can't have .attributes") @state
                    else
                        upperName (Just first) second

                False & True:
                    # something.Something
                    addError "Something wrong with uppercases?" @state


addWordToken as Token.NameModifier: ReadState@: None =
    modifier: state@:

    start = state.tokenStart

    end = getPos @state

    ds = (if modifier == Token.NameNoModifier then 0 else 1)

    chunk = Buffer.slice (state.tokenStart + ds) end state.buffer

    maybeKeywordKind =
        try chunk as
            "fn": Just << Token.Fn
            "if": Just << Token.If
            "then": Just << Token.Then
            "else": Just << Token.Else
            "try": Just << Token.Try
            "as": Just << Token.As
            "with": Just << Token.With
            "and": Just << Token.Binop Prelude.and_
            "or": Just << Token.Binop Prelude.or_
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
isNumber as Text: Bool =
    char:
    Text.startsWithRegex "[0-9_.]" char /= ""


addNumberToken as ReadState@: None =
    state@:

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
isSquiggle as Text: Bool =
    char:
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
       "@": True
       _: False


addSquiggleToken as Bool: ReadState@: None =
    nextIsSpace: state@:

    start & end & chunk =
        getChunk @state

    add = kind:
        absAddToken start end kind @state

    try chunk as
        ":": add << Token.Colon
#        ":-": add << Token.ConsumingColon
        "=": add << Token.Defop
#        "@": add << Token.Mutop
        "-": add << (if nextIsSpace then Token.Binop Prelude.subtract else Token.Unop Prelude.unaryMinus)
        "+": add << (if nextIsSpace then Token.Binop Prelude.add else Token.Unop Prelude.unaryPlus)
        op:
            try Dict.get chunk Prelude.binopsBySymbol as
                Just binop:
                    add << Token.Binop binop
                Nothing:
                    addError ("Invalid operator: `" .. chunk .. "`") @state


#
# Parens and comma
#
addParenOrCommaToken as Text: ReadState@: None =
    char: state@:

    add = kind:
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
lexOne as Text: ReadState@: None =
    char: state@:

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
                setMode Dot_One @state

#            "@":
#                @state.tokenStart := getPos @state
#                setMode Mutable @state

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
                if isWordStart char then
                    setMode (Word Token.NameNoModifier) @state

                else if isNumber char then
                    setMode NumberLiteral @state

                else if isSquiggle char then
                    setMode Squiggles @state

                else:
                    addParenOrCommaToken char @state

        Dot_One:
          if char == "." then
              setMode Dot_Two @state
          else if isWordStart char then
              setMode (Word Token.NameStartsWithDot) @state

          else if isNumber char then
              setMode NumberLiteral @state
          else
              addError "no idea what this is" @state

        Dot_Two:
          if char == "." then
              relAddToken (0 - 1) 1 Token.ThreeDots @state
              setMode Default @state

          else
              relAddToken (0 - 1) 1 (Token.Binop Prelude.textConcat) @state
              setMode Default @state
              lexOne char @state

        Mutable:
#            if isWordStart char then
#                setMode (Word Token.NameMutable) @state

            if isSquiggle char then
                setMode Squiggles @state

            else
                addError "no idea what this is" @state

        Word modifier:
            if isWordBody char then
              None

            else
                  addWordToken modifier @state
                  setMode Default @state
                  lexOne char @state

        NumberLiteral:
            if isNumber char then
              None

            else
                  addNumberToken @state
                  setMode Default @state
                  lexOne char @state

        Squiggles:
            if isSquiggle char then
              None

            else
                  addSquiggleToken (char == " ") @state
                  setMode Default @state
                  lexOne char @state

        ContentOpeningQuotes_One:
          if char == "\"" then
                setMode ContentOpeningQuotes_Two @state

          else if char == "" then
                  addError "there's no closing quotes" @state

          else:
                @state.tokenStart := getPos @state - 1
                setMode (SingleQuote { lastEscape = -1 }) @state
                lexOne char @state

        ContentOpeningQuotes_Two:
          if char == "\"" then
                @state.tokenStart := getPos @state - 2
                setMode (TripleQuote { lastEscape = -1, closingQuotes = 0 }) @state
          else:
                # TODO replace with unary `-` once it works
                relAddToken (0 - 2) 0 (Token.TextLiteral "") @state
                setMode Default @state
                lexOne char @state

        SingleQuote { lastEscape }:
          previousIsEscape =
            pos == lastEscape + 1

          if char == "" then
                addError "there's no closing quotes" @state

          else if previousIsEscape then
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

          if char == "" then
                  addError "unterminated triple quotes" @state

          else if previousIsEscape then
                  setMode (TripleQuote { lastEscape, closingQuotes = 0 }) @state

          else
             try char as
               "\"":
                  if closingQuotes == 2 then
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
          if char == "\n" or char == "" then
#                  absAddToken state.tokenStart (getPos @state) Token.Comment @state
                  setMode Default @state
                  lexOne char @state
          else
              None

        ContentOpeningBlockComment:
          if char == "#" then
                setMode (BlockComment { nesting = 1, previous = "" }) @state
          else:
                # TODO replace with -1 once it as supported
                relAddToken (0 - 1) 0 (Token.SquareBracket Token.Open) @state
                setMode Default @state
                lexOne char @state

        BlockComment { nesting, previous }:
          continueWithDeltaNesting = dn:
              setMode (BlockComment { nesting = nesting + dn, previous = char }) @state

          try previous & char as
            "[" & "#":
                continueWithDeltaNesting 1

            "#" & "]":
                if nesting > 1 then
                  continueWithDeltaNesting (0 - 1)
                else
#                      absAddToken state.tokenStart (getPos @state) Token.Comment @state
                      setMode Default @state

            _ & "":
                    addError "unterminated block comment" @state

            _:
                continueWithDeltaNesting 0


tryIndent as Text: Text: ReadState@: None =
    indentChar: char: state@:

    if char == indentChar or char == "" then
        None

    else if char == " " or char == "\t" then
        @state
            >> addError "mixing tabs and spaces!"

    else if char == "\n" then
        # line is empty, ignore
        @state.tokenStart := getPos @state + 1
        setMode Indent @state

    else if char == "#" then
        setMode LineComment @state
    else
        @state.lineIndent := state.column
        setMode Default @state
        lexOne char @state


closeOpenBlocks as ReadState@: None =
    state@:

    pos =
        getPos @state

    List.each state.indentStack _:
        Array.push @state.tokens << Token Token.N pos pos Token.BlockEnd


lexer as Text: Text: Res [Token] =
    moduleName: moduleCode:

    Debug.benchStart None

    state @= readStateInit moduleName moduleCode

    Text.forEach moduleCode char:
        lexOne char @state

        @state.buffer.nextPos += 1

        if char == "\n" then
            @state.line += 1
            @state.column := 0
        else
            @state.column += 1


    lexOne "" @state

    if state.errors == [] then
        closeOpenBlocks @state
        state.tokens
            >> Array.toList
            >> Ok
            >> btw Debug.benchStop "lexer"
    else
        state.errors
            >> Error.Nested
            >> Err
            >> btw Debug.benchStop "lexer"

