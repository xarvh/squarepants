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
    , Word
    , NumberLiteral
    , Squiggles
    , SingleQuote { lastEscape as Int }
    , TripleQuote { lastEscape as Int, closingQuotes as Int }
    , LineComment { start as Int, column as Int }
    , BlockComment { start as Int, column as Int, nesting as Int, previous as Text }


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
    , nextPos as Int

    , column as Int
    , line as Int
    , lineIndent as Int

    , errors as Array (fn Error.Module: Error)

    # indent / block stuff
    , soFarThereAreNoTokensInThisLine as Bool
    , indentStack as Array Indent
    , indentStartsABlock as Bool

    # state machine
    , mode as Mode
    , tokenStart as Int
    , tabsOrSpaces as TabsOrSpaces

    # accumulator
    , sections as Array [Token]
    , tokens as Array Token

    # config
    , keepComments as Bool
    }


readStateInit as fn Bool: !ReadState =
    fn keepComments:
    {
    , nextPos = 0

    , column = 0
    , line = 0
    , lineIndent = 0

    , errors = Array.fromList []

    , soFarThereAreNoTokensInThisLine = True
    , indentStack = Array.fromList []
    , indentStartsABlock = True

    , mode = Indent
    , tokenStart = 0
    , tabsOrSpaces = NoTabsOrSpacesYet
    , sections = Array.fromList []
    , tokens = Array.fromList []

    , keepComments = cloneImm keepComments
    }


getPos as fn @ReadState: !Int =
    fn @state:
    cloneUni @state.nextPos


addError as fn Text, @ReadState: None =
    fn message, @state:

    end =
        getPos @state

    start =
        cloneUni @state.tokenStart

    error as fn Error.Module: Error =
        Error.Simple __ (Pos.P start end) [ message ]

    Array.push @state.errors error
    @state.tokenStart := cloneImm end


addErrorIf as fn Bool, @ReadState, Text: None =
    fn isError, @state, message:
    if isError then addError message @state else None


setMode as fn Mode, @ReadState: None =
    fn mode, @state:

    @state.mode := cloneImm mode


addIndentToken as fn Int, Token.Kind, @ReadState: None =
    fn pos, kind, @state:
    Array.push @state.tokens (Token pos pos kind)


updateIndent as fn Int, Int, Token.Kind, @ReadState: None =
    fn start, end, kind, @state:

    manageIndent =
        fn head:

        lineIndent = cloneUni @state.lineIndent

        if lineIndent > head.indent then

            newIndent =
                {
                , isBlock = cloneUni @state.indentStartsABlock
                , indent = lineIndent
                }

            Array.push @state.indentStack newIndent

            if cloneUni @state.indentStartsABlock then
                addIndentToken start Token.BlockStart @state
            else
                None

        else
            # this means that state.lineIndent == head.indent
            if head.isBlock then #and kind /= Token.Comment then

                list = Array.toList @state.tokens
                if cloneUni @state.lineIndent /= 0 or list == [] then
                    None
                else
                    Array.push @state.sections list
                    @state.tokens := Array.fromList []

                addIndentToken start Token.NewSiblingLine @state
            else
                None


    try Array.pop @state.indentStack as

        , Nothing:
            manageIndent { indent = 0, isBlock = True }

        , Just head:
            if cloneUni @state.lineIndent < head.indent then
                if head.isBlock then
                    addIndentToken start Token.BlockEnd @state
                else
                    None

                updateIndent start end kind @state
            else
                # Ugly... Would probably be better to get the last element instead without popping it
                Array.push @state.indentStack head
                manageIndent head


addContentTokenAbs as fn Int, Int, Token.Kind, @ReadState: None =
    fn start, end, kind, @state:

    if cloneUni @state.soFarThereAreNoTokensInThisLine then
        @state.soFarThereAreNoTokensInThisLine := False
        updateIndent start end kind @state
    else
        None

    !indentStartsABlock =
        try kind as
            # maybe start block after these
            , Token.Then: True
            , Token.Else _: True
            , Token.As: True
            , Token.Colon: True
            , Token.Defop: True
            , _: False

    @state.indentStartsABlock := indentStartsABlock

    Array.push @state.tokens (Token start end kind)
    @state.tokenStart := cloneImm end


addCommentTokenAbs as fn Int, Int, Token.Kind, @ReadState: None =
    fn start, end, kind, @state:

    if cloneUni @state.keepComments then
        Array.push @state.tokens (Token start end kind)
    else
        None

    @state.tokenStart := cloneImm end


addContentTokenRel as fn Int, Int, Token.Kind, @ReadState: None =
    fn ds, de, kind, @state:
    pos = getPos @state
    addContentTokenAbs (pos + ds) (pos + de) kind @state


addOneIndentToken as fn Token.Kind, @ReadState: None =
    fn kind, @state:
    pos = getPos @state
    Array.push @state.tokens (Token pos pos kind)


getChunk as fn Text, @ReadState: Int & Int & Text =
    fn buffer, @state:
    start = cloneUni @state.tokenStart
    end = getPos @state
    start & end & Text.slice start end buffer


unindent as fn Text: Text =
    fn raw:

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

        countLeadingSpaces as fn Text: Int =
            re = Text.startsWithRegex "[ ]*"
            fn line:
            line
            >> re
            >> Text.length

        minLead =
            lines
            >> List.filter (fn s: Text.trimLeft s /= "") __
            >> List.map countLeadingSpaces __
            >> List.minimum
            >> Maybe.withDefault 0 __

        lines
        >> List.map (Text.dropLeft minLead __) __
        >> Text.join "\n" __
        >> (Text.replaceRegex "\n[ ]*$") "" __


#
# Words (names, keywords, logical ops)
#
isWordStart as fn Text: Bool =
    re = Text.startsWithRegex "[a-zA-Z._']"

    fn char:
    re char /= ""


isWordBody as fn Text: Bool =
    re = Text.startsWithRegex "[a-zA-Z./_0-9']"

    fn char:
    re char /= ""


startsWithUpperChar as fn Text: Bool =
    re = Text.startsWithRegex "[A-Z]"

    fn s:
    try re s as
        , "": False
        , _: True


parseNameToWord as fn @ReadState, { main as Name, maybeModule as Maybe Name, attrPath as [Name] }: Token.Kind =
    fn @state, { main, maybeModule, attrPath }:

    try Text.split "'" main as
        , [ "", raw ]:
              addErrorIf (startsWithUpperChar raw) @state "Constructors should start with a lowercase letter"
              addErrorIf (attrPath /= []) @state "Constructors don't have any attribute to access"

              Token.Constructor { name = main, maybeModule }

        , [ name ]:
              if startsWithUpperChar name then
                  addErrorIf (attrPath /= []) @state "WAT... Isn't this a normal qualified value!?"
                  Token.Uppercase { name, maybeModule }
              else
                  Token.Lowercase { name, maybeModule, attrPath }

        , _:
              addError "apostrophes can be used only at the beginning of a constructor name" @state
              Token.Constructor { name = main, maybeModule = Nothing }


parseModule as fn @ReadState, Text: Text =
    fn @state, text:
    "TODO"


parseAttr as fn @ReadState, Text: Text =
    fn @state, text:
    "TODO"


addWord as fn Int, Int, Text, @ReadState: None =
    fn start, end, chunk, @state:

    snips =
        Text.split "." chunk

    addErrorIf (List.any (fn s: s == "") snips) @state
        "use spaces around `..` to concatenate Text"

    try snips as
        , []:
            Token.Lowercase { maybeModule = Nothing, name = "THIS IS NOT SUPPOSED TO HAPPEN", attrPath = [] }

        , [ main ]:
            # value or attribute
            # Module or Type
            # 'constructor
            parseNameToWord @state { main, maybeModule = Nothing, attrPath = [] }

        , [ "", two, ...rest ]:
                # .attr1
                # .attr1.attr2
                Token.RecordShorthand {
                    , name = parseAttr @state two
                    , attrPath = List.map (parseAttr @state __) rest
                    }

        , [ one, two, ...rest ]:
            if startsWithUpperChar one then
                # Module.value
                # Module.value.attr1
                # Module.value.attr1.attr2
                # Module.Type
                # Module.'constructor
                module = parseModule @state one
                attrPath = List.map (parseAttr @state __) rest
                parseNameToWord @state {
                    , main = two
                    , maybeModule = Just module
                    , attrPath
                    }

            else
                # value.attr1
                # value.attr1.attr2
                attrPath = List.map (parseAttr @state __) rest
                parseNameToWord @state {
                    , main = one
                    , maybeModule = Nothing
                    , attrPath
                    }

    >> addContentTokenAbs start end __ @state


addWordToken as fn Text, @ReadState: None =
    fn buffer, @state:

    start = cloneUni @state.tokenStart

    end = getPos @state

    chunk = Text.slice (start) end buffer

    maybeKeywordKind =
        try chunk as
            , "fn": Just << Token.Fn
            , "if": Just << Token.If (cloneUni @state.line)
            , "then": Just << Token.Then
            , "else": Just << Token.Else (cloneUni @state.line)
            , "try": Just << Token.Try
            , "as": Just << Token.As
            , "with": Just << Token.With
            , "and": Just << Token.Binop (cloneUni @state.line) Prelude.and_
            , "or": Just << Token.Binop (cloneUni @state.line) Prelude.or_
            , "__": Just << Token.ArgumentPlaceholder
            , _: Nothing

    try maybeKeywordKind as
        , Just kind:
            addContentTokenAbs start end kind @state

        , _:
            addWord start end chunk @state


#
# Number literals
#
isNumber as fn Text: Bool =
    re = Text.startsWithRegex "[0-9_.]"

    fn char:
    re char /= ""


addNumberToken as fn Bool, Text, @ReadState: None =
    fn isPercent, buffer, @state:

    start & end & chunk =
        getChunk buffer @state

    # TODO ensure that there as one or zero dots
    # This as probably best done in lexOne

    # TODO ensure that there as at least one actual figure

    # TODO what about exponential notation?

    addContentTokenAbs start end (Token.NumberLiteral isPercent chunk) @state


#
# Squiggles (ops and symbols)
#
isSquiggle as fn Text: Bool =
    fn char:
    try char as
       , "=": True
       , ":": True
       , "*": True
       , "+": True
       , "-": True
       , "/": True
       , ">": True
       , "<": True
       , "!": True
       , "?": True
       , "&": True
       , "^": True
       , "@": True
       , "$": True
       , _: False


addSquiggleToken as fn Text, Bool, @ReadState: None =
    fn buffer, nextIsSpace, @state:

    start & end & chunk =
        getChunk buffer @state

    add =
        addContentTokenAbs start end __ @state

    try chunk as
        , ":": add << Token.Colon
        , "=": add << Token.Defop
        , "?": add << Token.UniquenessPolymorphismBinop
        , "!": add << Token.Unop Op.UnopUnique
        , "@": add << Token.Unop Op.UnopRecycle
        , "-": add << (if nextIsSpace then Token.Binop (cloneUni @state.line) Prelude.subtract else Token.Unop Op.UnopMinus)
        , "+": add << (if nextIsSpace then Token.Binop (cloneUni @state.line) Prelude.add else Token.Unop Op.UnopPlus)
        , op:
            try Dict.get chunk Prelude.binopsBySymbol as
                , Just binop:
                    add << Token.Binop (cloneUni @state.line) binop
                , Nothing:
                    addError ("Invalid operator: `" .. chunk .. "`") @state


#
# Parens and comma
#
addParenOrCommaToken as fn Text, @ReadState: None =
    fn char, @state:

    add =
        addContentTokenRel 0 1 __ @state

    line =
        cloneUni @state.line

    try char as
        , "(": add << Token.RoundParen Token.Open
        , ")": add << Token.RoundParen Token.Closed
        , "[": add << Token.SquareBracket line Token.Open
        , "]": add << Token.SquareBracket line Token.Closed
        , "{": add << Token.CurlyBrace line Token.Open
        , "}": add << Token.CurlyBrace line Token.Closed
        , ",": add << Token.Comma

        # This shuld never happen?
        , _: addError ("I can't make sense of this piece of text: `" .. char .. "`") @state


#
# Outer machine state lexer
#
lexOne as fn Text, Text, @ReadState: None =
    fn buffer, char, @state:

    # position of char
    pos =
        getPos @state

    try cloneUni @state.mode as

        , Indent:
          try cloneUni @state.tabsOrSpaces as
            , Tabs:
                tryIndent buffer "\t" char @state

            , Spaces:
                tryIndent buffer " " char @state

            , NoTabsOrSpacesYet:
              try char as
                  , " ":
                      @state.tabsOrSpaces := Spaces
                      lexOne buffer char @state
                  , "\t":
                      @state.tabsOrSpaces := Tabs
                      lexOne buffer char @state
                  , _:
                      tryIndent buffer " " char @state

        , Default:
          try char as
            , "":
                None

            , ".":
                setMode Dot_One @state

#            "@":
#                @state.tokenStart := getPos @state
#                setMode Mutable @state

            , "#":
                start = getPos @state
                column = cloneUni @state.column
                setMode (LineComment { start, column }) @state

            , "[":
                setMode ContentOpeningBlockComment @state

            , "\"":
                setMode ContentOpeningQuotes_One @state

            , "\n":
                @state.tokenStart := getPos @state + 1
                @state.soFarThereAreNoTokensInThisLine := True
                setMode Indent @state

            , " ":
                @state.tokenStart := getPos @state + 1

            , _:
                @state.tokenStart := getPos @state
                if isWordStart char then
                    setMode (Word) @state

                else if isNumber char then
                    setMode NumberLiteral @state

                else if isSquiggle char then
                    setMode Squiggles @state

                else
                    addParenOrCommaToken char @state

        , Dot_One:
          if char == "." then
              setMode Dot_Two @state
          else if isWordStart char then
              @state.tokenStart := getPos @state - 1
              setMode  Word @state

          else if isNumber char then
              setMode NumberLiteral @state
          else
              addError "no idea what this is" @state

        , Dot_Two:
          if char == "." then
              addContentTokenRel -1 1 Token.ThreeDots @state
              setMode Default @state

          else
              addContentTokenRel -1 1 (Token.Binop (cloneUni @state.line) Prelude.textConcat) @state
              setMode Default @state
              lexOne buffer char @state

        , Mutable:
#            if isWordStart char then
#                setMode (Word Token.NameMutable) @state

            if isSquiggle char then
                setMode Squiggles @state

            else
                addError "no idea what this is" @state

        , Word:
            if isWordBody char then
              None

            else
                  addWordToken buffer @state
                  setMode Default @state
                  lexOne buffer char @state

        , NumberLiteral:
            if isNumber char then
                None
            else if char == "%" then
                addNumberToken True buffer @state
                setMode Default @state
            else
                addNumberToken False buffer @state
                setMode Default @state
                lexOne buffer char @state

        , Squiggles:
            if isSquiggle char then
                None
            else
                addSquiggleToken buffer (char == " ") @state
                setMode Default @state
                lexOne buffer char @state

        , ContentOpeningQuotes_One:
          if char == "\"" then
              setMode ContentOpeningQuotes_Two @state

          else if char == "" then
              addError "there's no closing quotes" @state

          else
              @state.tokenStart := getPos @state - 1
              setMode (SingleQuote { lastEscape = -1 }) @state
              lexOne buffer char @state

        , ContentOpeningQuotes_Two:
          if char == "\"" then
              @state.tokenStart := getPos @state - 2
              setMode (TripleQuote { lastEscape = -1, closingQuotes = 0 }) @state
          else
              addContentTokenRel -2 0 (Token.TextLiteral Token.SingleQuote "") @state
              setMode Default @state
              lexOne buffer char @state


        , SingleQuote { lastEscape }:
          previousIsEscape =
              pos == lastEscape + 1

          if char == "" then
              addError "there's no closing quotes" @state

          else if previousIsEscape then
              setMode (SingleQuote { lastEscape }) @state

          else
            try char as
              , "\"":
                  start = cloneUni @state.tokenStart
                  end = pos + 1

                  value =
                      buffer
                      >> Text.slice (start + 1) (end - 1) __
#                      >> Text.replace "\\\"" "\"" __

                  addContentTokenAbs start end (Token.TextLiteral Token.SingleQuote value) @state
                  setMode Default @state

              , "\\":
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

              , _:
                None

        , TripleQuote { lastEscape, closingQuotes }:
          previousIsEscape =
              pos == lastEscape + 1

          if char == "" then
              addError "unterminated triple quotes" @state

          else if previousIsEscape then
              setMode (TripleQuote { lastEscape, closingQuotes = 0 }) @state

          else
             try char as
               , "\"":
                  if closingQuotes == 2 then
                    # TODO maybe move away the finalization code from here?
                    start = cloneUni @state.tokenStart
                    end = pos + 1

                    buffer
                    >> Text.slice (start + 3) (end - 3) __
                    >> unindent
                    >> Token.TextLiteral Token.TripleQuote __
                    >> addContentTokenAbs start end __ @state

                    setMode Default @state
                  else
                    setMode (TripleQuote { lastEscape, closingQuotes = closingQuotes + 1 }) @state

               , "\\":
                    setMode (TripleQuote { lastEscape = pos, closingQuotes = 0 }) @state

               , _:
                    setMode (TripleQuote { lastEscape, closingQuotes = 0 }) @state


        , LineComment { start, column }:
          if char == "\n" or char == "" then

              {
              , isBlock = False
              , indent = column
              , isFollowedByBlank = thereIsABlankAhead 0 buffer @state
              }
              >> Token.Comment
              >> addCommentTokenAbs start (getPos @state) __ @state

              setMode Default @state
              lexOne buffer char @state
          else
              None


        , ContentOpeningBlockComment:
          if char == "#" then
              start = getPos @state - 1
              column = cloneUni @state.column - 1
              setMode (BlockComment { start, column, nesting = 1, previous = "" }) @state
          else
              addContentTokenRel -1 0 (Token.SquareBracket (cloneUni @state.line) Token.Open) @state
              setMode Default @state
              lexOne buffer char @state

        , BlockComment { start, column, nesting, previous }:
          continueWithDeltaNesting =
              fn dn:
              setMode (BlockComment { start, column, nesting = nesting + dn, previous = char }) @state

          try previous & char as
            , "[" & "#":
                continueWithDeltaNesting 1

            , "#" & "]":
                if nesting > 1 then
                    continueWithDeltaNesting -1
                else
                    {
                    , isBlock = True
                    , indent = column
                    , isFollowedByBlank = thereIsABlankAhead 1 buffer @state
                    }
                    >> Token.Comment
                    >> addCommentTokenAbs start (getPos @state + 1) __ @state

                    setMode Default @state

            , _ & "":
                addError "unterminated block comment" @state

            , _:
                continueWithDeltaNesting 0


thereIsABlankAhead as fn Int, Text, @ReadState: Bool =
    fn offset, buffer, @state:

    # This is so bad.... XD
    start = cloneUni @state.nextPos + offset >> Text.fromNumber

    regex = ".{" .. start .. "}\n[ ]*(\n|$)"

#    log "BUFFER" buffer
#    log "REGEX" regex

    (Text.startsWithRegex regex) buffer /= ""



tryIndent as fn Text, Text, Text, @ReadState: None =
    fn buffer, indentChar, char, @state:

    if char == indentChar or char == "" then
        None

    else if char == " " or char == "\t" then
        addError "mixing tabs and spaces!" @state

    else if char == "\n" then
        # line is empty, ignore
        @state.tokenStart := getPos @state + 1
        setMode Indent @state

    else if char == "#" then
        start = getPos @state
        column = cloneUni @state.column
        setMode (LineComment { start, column }) @state
    else
        @state.lineIndent := cloneUni @state.column
        setMode Default @state
        lexOne buffer char @state


closeOpenBlocks as fn @ReadState: None =
    fn @state:

    pos =
        getPos @state

    s = Array.toList @state.indentStack

    List.each s fn _:
        Array.push @state.tokens (Token pos pos Token.BlockEnd)

    Array.push @state.sections (Array.toList @state.tokens)


lexer as fn Bool, Error.Module: Res [[Token]] =
    fn keepComments, module:

    #Debug.benchStart None

    moduleCode =
        module.content

    !state = readStateInit keepComments

    Text.forEach moduleCode fn char:
        lexOne moduleCode char @state

        @state.nextPos += 1

        if char == "\n" then
            @state.line += 1
            @state.column := 0
        else
            @state.column += 1


    lexOne moduleCode "" @state

    #Debug.benchStop "lexer"

    try Array.toList @state.errors as
        , []:
            closeOpenBlocks @state

            Array.toList @state.sections
            >> Ok

        , errors:
            errors
            >> List.map (fn e: e module) __
            >> Error.Nested
            >> Err

