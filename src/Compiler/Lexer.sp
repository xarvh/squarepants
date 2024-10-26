#
# Lexer
#
var Mode =
    , 'indent
    , 'default
    , 'contentOpeningQuotes_One
    , 'contentOpeningQuotes_Two
    , 'contentOpeningBlockComment
    , 'dot_One
    , 'dot_Two
    , 'mutable
    , 'word
    , 'numberLiteral
    , 'squiggles
    , 'singleQuote { lastEscape as Int }
    , 'tripleQuote { closingQuotes as Int, lastEscape as Int }
    , 'lineComment { column as Int, start as Int }
    , 'blockComment { column as Int, nesting as Int, previous as Text, start as Int }


var TabsOrSpaces =
    , 'noTabsOrSpacesYet
    , 'tabs
    , 'spaces


Indent =
    {
#    , pos as Int
#    , line as Int
    , indent as Int
    , isBlock as Bool
    }


ReadState =
    {
    , column as Int
    , errors as Array (fn Error.Module: Error)
    , indentStack as Array Indent
    , indentStartsABlock as Bool
    # config
    , keepComments as Bool
    , line as Int
    , lineIndent as Int
    # state machine
    , mode as Mode
    , nextPos as Int
    # accumulator
    , sections as Array [ Token ]
    # indent / block stuff
    , soFarThereAreNoTokensInThisLine as Bool
    , tabsOrSpaces as TabsOrSpaces
    , tokenStart as Int
    , tokens as Array Token
    }


readStateInit as fn Bool: !ReadState =
    fn keepComments:
    {
    , column = 0
    , errors = Array.fromList []
    , indentStack = Array.fromList []
    , indentStartsABlock = 'true
    , keepComments = cloneImm keepComments
    , line = 0
    , lineIndent = 0
    , mode = 'indent
    , nextPos = 0
    , sections = Array.fromList []
    , soFarThereAreNoTokensInThisLine = 'true
    , tabsOrSpaces = 'noTabsOrSpacesYet
    , tokenStart = 0
    , tokens = Array.fromList []
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
        Error.'simple __ (Pos.'p start end) [ message ]

    Array.push @state.errors error

    @state.tokenStart := cloneImm end


addErrorIf as fn Bool, @ReadState, Text: None =
    fn isError, @state, message:
    if isError then addError message @state else 'none


setMode as fn Mode, @ReadState: None =
    fn mode, @state:
    @state.mode := cloneImm mode


addIndentToken as fn Int, Token.Kind, @ReadState: None =
    fn pos, kind, @state:
    Array.push @state.tokens ('token pos pos kind)


updateIndent as fn Int, Int, Token.Kind, @ReadState: None =
    fn start, end, kind, @state:
    manageIndent =
        fn head:
        lineIndent =
            cloneUni @state.lineIndent

        if lineIndent > head.indent then
            newIndent =
                {
                , indent = lineIndent
                , isBlock = cloneUni @state.indentStartsABlock
                }

            Array.push @state.indentStack newIndent

            if cloneUni @state.indentStartsABlock then
                addIndentToken start Token.'blockStart @state
            else
                'none
        else
            # this means that state.lineIndent == head.indent
            if head.isBlock then
                #and kind /= Token.Comment then

                list =
                    Array.toList @state.tokens

                if cloneUni @state.lineIndent /= 0 or list == [] then
                    'none
                else
                    Array.push @state.sections list

                    @state.tokens := Array.fromList []

                addIndentToken start Token.'newSiblingLine @state
            else
                'none

    try Array.pop @state.indentStack as

        'nothing:
            manageIndent { indent = 0, isBlock = 'true }

        'just head:
            if cloneUni @state.lineIndent < head.indent then
                if head.isBlock then
                    addIndentToken start Token.'blockEnd @state
                else
                    'none

                updateIndent start end kind @state
            else
                # Ugly... Would probably be better to get the last element instead without popping it
                Array.push @state.indentStack head

                manageIndent head


addContentTokenAbs as fn Int, Int, Token.Kind, @ReadState: None =
    fn start, end, kind, @state:
    if cloneUni @state.soFarThereAreNoTokensInThisLine then
        @state.soFarThereAreNoTokensInThisLine := 'false

        updateIndent start end kind @state
    else
        'none

    !indentStartsABlock =
        try kind as

            # maybe start block after these
            Token.'then:
                'true

            Token.'else _:
                'true

            Token.'as:
                'true

            Token.'colon:
                'true

            Token.'defop:
                'true

            _:
                'false

    @state.indentStartsABlock := indentStartsABlock

    Array.push @state.tokens ('token start end kind)

    @state.tokenStart := cloneImm end


addCommentTokenAbs as fn Int, Int, Token.Kind, @ReadState: None =
    fn start, end, kind, @state:
    if cloneUni @state.keepComments then
        Array.push @state.tokens ('token start end kind)
    else
        'none

    @state.tokenStart := cloneImm end


addContentTokenRel as fn Int, Int, Token.Kind, @ReadState: None =
    fn ds, de, kind, @state:
    pos =
        getPos @state

    addContentTokenAbs (pos + ds) (pos + de) kind @state


addOneIndentToken as fn Token.Kind, @ReadState: None =
    fn kind, @state:
    pos =
        getPos @state

    Array.push @state.tokens ('token pos pos kind)


getChunk as fn Text, @ReadState: Int & Int & Text =
    fn buffer, @state:
    start =
        cloneUni @state.tokenStart

    end =
        getPos @state

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
            re =
                Text.startsWithRegex "[ ]*"

            fn line:
            line
            >> re
            >> Text.length

        minLead =
            lines
            >> List.filter __ (fn s: Text.trimLeft s /= "")
            >> List.map __ countLeadingSpaces
            >> List.minimum
            >> Maybe.withDefault __ 0

        lines
        >> List.map __ (Text.dropLeft minLead __)
        >> Text.join "\n" __
        >> (Text.replaceRegex "\n[ ]*$") "" __


#
# Words (names, keywords, logical ops)
#
isWordStart as fn Text: Bool =
    re =
        Text.startsWithRegex "[a-zA-Z._']"

    fn char:
    re char /= ""


isWordBody as fn Text: Bool =
    re =
        Text.startsWithRegex "[a-zA-Z./_0-9']"

    fn char:
    re char /= ""


startsWithUpperChar as fn Text: Bool =
    re =
        Text.startsWithRegex "[A-Z]"

    fn s:
    try re s as
        "": 'false
        _: 'true


parseNameToWord as fn @ReadState, { attrPath as [ Name ], main as Name, maybeModule as Maybe Name }: Token.Kind =
    fn @state, { attrPath, main, maybeModule }:
    try Text.split "'" main as

        [ "", raw ]:
            addErrorIf (attrPath /= []) @state "Constructors don't have any attribute to access"

            Token.'constructor { maybeModule, name = main }

        [ name ]:
            if startsWithUpperChar name then
                addErrorIf (attrPath /= []) @state "WAT... Type names don't have attributes to access. =|"

                Token.'uppercase { maybeModule, name }
            else
                Token.'lowercase { attrPath, maybeModule, name }

        _:
            addError "apostrophes can be used only at the beginning of a constructor name" @state

            Token.'constructor { maybeModule = 'nothing, name = main }


parseModule as fn @ReadState, Text: Text =
    fn @state, text:
    # TODO should be only [A-Z][A-Za-z_]
    text


parseAttr as fn @ReadState, Text: Text =
    fn @state, text:
    # TODO should be only [a-z][A-Za-z_]
    addErrorIf (startsWithUpperChar text) @state "record attributes must start with a lowercase letter"

    text


parseAttrs as fn @ReadState, [ Text ]: [ Text ] =
    fn @state, ts:
    List.map ts (parseAttr @state __)


addWord as fn Int, Int, Text, @ReadState: None =
    fn start, end, chunk0, @state:
    chunk1 & trailingThreeDots =
        try Text.split "..." chunk0 as
            [ c, "" ]: c & 'true
            [ c ]: c & 'false
            _: chunk0 & 'false

    snips =
        Text.split "." chunk1

    try snips as

        []:
            Token.'lowercase { attrPath = [], maybeModule = 'nothing, name = "THIS IS NOT SUPPOSED TO HAPPEN" }

        [ main ]:
            # value or attribute
            # Module or Type
            # 'constructor
            parseNameToWord @state { attrPath = [], main, maybeModule = 'nothing }

        [ "", two, rest... ]:
            addErrorIf (List.any (two :: rest) (__ == "")) @state "use spaces around `..` to concatenate Text"

            # .attr1
            # .attr1.attr2
            Token.'recordShorthand
                {
                , attrPath = parseAttrs @state rest
                , name = parseAttr @state two
                }

        [ one, two, rest... ]:
            addErrorIf (List.any snips (__ == "")) @state "use spaces around `..` to concatenate Text"

            if startsWithUpperChar one then
                # Module.value
                # Module.value.attr1
                # Module.value.attr1.attr2
                # Module.Type
                # Module.'constructor
                module =
                    parseModule @state one

                parseNameToWord
                    @state
                    {
                    , attrPath = parseAttrs @state rest
                    , main = two
                    , maybeModule = 'just module
                    }
            else
                # value.attr1
                # value.attr1.attr2
                parseNameToWord
                    @state
                    {
                    , attrPath = parseAttrs @state (two :: rest)
                    , main = one
                    , maybeModule = 'nothing
                    }
    >> addContentTokenAbs start end __ @state

    if trailingThreeDots then
        addContentTokenAbs (end - 3) end Token.'threeDots @state
    else
        'none


addWordToken as fn Text, @ReadState: None =
    fn buffer, @state:
    start =
        cloneUni @state.tokenStart

    end =
        getPos @state

    chunk =
        Text.slice start end buffer

    maybeKeywordKind =
        try chunk as
            "fn": 'just << Token.'fn
            "if": 'just << Token.'if (cloneUni @state.line)
            "then": 'just << Token.'then
            "else": 'just << Token.'else (cloneUni @state.line)
            "try": 'just << Token.'try
            "as": 'just << Token.'as
            "with": 'just << Token.'with
            "and": 'just << Token.'binop (cloneUni @state.line) CoreDefs.and_
            "or": 'just << Token.'binop (cloneUni @state.line) CoreDefs.or_
            "__": 'just << Token.'argumentPlaceholder
            "this_is_sp_native": 'just Token.'this_is_sp_native
            "sp_introspect_value": 'just (Token.'sp_introspect Token.'value)
            "sp_introspect_type": 'just (Token.'sp_introspect Token.'type)
            "sp_introspect_type_open": 'just (Token.'sp_introspect Token.'typeOpen)
            _: 'nothing

    try maybeKeywordKind as
        'just kind: addContentTokenAbs start end kind @state
        _: addWord start end chunk @state


#
# Number literals
#
isNumber as fn Text: Bool =
    re =
        Text.startsWithRegex "[0-9_.]"

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

    addContentTokenAbs start end (Token.'numberLiteral isPercent chunk) @state


#
# Squiggles (ops and symbols)
#
isSquiggle as fn Text: Bool =
    fn char:
    try char as
        "=": 'true
        ":": 'true
        "*": 'true
        "+": 'true
        "-": 'true
        "/": 'true
        ">": 'true
        "<": 'true
        "!": 'true
        "?": 'true
        "&": 'true
        "^": 'true
        "@": 'true
        "$": 'true
        _: 'false


addSquiggleToken as fn Text, Bool, @ReadState: None =
    fn buffer, nextIsSpace, @state:
    start & end & chunk =
        getChunk buffer @state

    add =
        addContentTokenAbs start end __ @state

    try chunk as

        ":":
            add << Token.'colon

        "=":
            add << Token.'defop

        "?":
            add << Token.'uniquenessPolymorphismBinop

        "!":
            add << Token.'unop Op.'unopUnique

        "@":
            add << Token.'unop Op.'unopRecycle

        "-":
            add << if nextIsSpace then Token.'binop (cloneUni @state.line) CoreDefs.subtract else Token.'unop Op.'unopMinus

        "+":
            add << if nextIsSpace then Token.'binop (cloneUni @state.line) CoreDefs.add else Token.'unop Op.'unopPlus

        op:
            try Dict.get chunk CoreDefs.binopsBySymbol as
                'just binop: add << Token.'binop (cloneUni @state.line) binop
                'nothing: addError ("Invalid operator: `" .. chunk .. "`") @state


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

        "(":
            add << Token.'roundParen Token.'open

        ")":
            add << Token.'roundParen Token.'closed

        "[":
            add << Token.'squareBracket line Token.'open

        "]":
            add << Token.'squareBracket line Token.'closed

        "{":
            add << Token.'curlyBrace line Token.'open

        "}":
            add << Token.'curlyBrace line Token.'closed

        ",":
            add << Token.'comma

        # This shuld never happen?
        _:
            addError ("I can't make sense of this piece of text: `" .. char .. "`") @state


#
# Outer machine state lexer
#
lexOne as fn Text, Text, @ReadState: None =
    fn buffer, char, @state:
    # position of char
    pos =
        getPos @state

    try cloneUni @state.mode as

        'indent:
            try cloneUni @state.tabsOrSpaces as

                'tabs:
                    tryIndent buffer "\t" char @state

                'spaces:
                    tryIndent buffer " " char @state

                'noTabsOrSpacesYet:
                    try char as

                        " ":
                            @state.tabsOrSpaces := 'spaces

                            lexOne buffer char @state

                        "\t":
                            @state.tabsOrSpaces := 'tabs

                            lexOne buffer char @state

                        _:
                            tryIndent buffer " " char @state

        'default:
            try char as

                "":
                    'none

                ".":
                    setMode 'dot_One @state

#            "@":
#                @state.tokenStart := getPos @state
#                setMode Mutable @state

                "#":
                    start =
                        getPos @state

                    column =
                        cloneUni @state.column

                    setMode ('lineComment { column, start }) @state

                "[":
                    setMode 'contentOpeningBlockComment @state

                "\"":
                    setMode 'contentOpeningQuotes_One @state

                "\n":
                    @state.tokenStart := getPos @state + 1

                    @state.soFarThereAreNoTokensInThisLine := 'true

                    setMode 'indent @state

                " ":
                    @state.tokenStart := getPos @state + 1

                _:
                    @state.tokenStart := getPos @state

                    if isWordStart char then
                        setMode 'word @state
                    else if isNumber char then
                        setMode 'numberLiteral @state
                    else if isSquiggle char then
                        setMode 'squiggles @state
                    else
                        addParenOrCommaToken char @state

        'dot_One:
            if char == "." then
                setMode 'dot_Two @state
            else if isWordStart char then
                @state.tokenStart := getPos @state - 1

                setMode 'word @state
            else if isNumber char then
                setMode 'numberLiteral @state
            else
                addError "no idea what this is" @state

        'dot_Two:
            if char == "." then
                addContentTokenRel -1 1 Token.'threeDots @state

                setMode 'default @state
            else
                addContentTokenRel -1 1 (Token.'binop (cloneUni @state.line) CoreDefs.textConcat) @state

                setMode 'default @state

                lexOne buffer char @state

        'mutable:
#            if isWordStart char then
#                setMode (Word Token.NameMutable) @state

            if isSquiggle char then
                setMode 'squiggles @state
            else
                addError "no idea what this is" @state

        'word:
            if isWordBody char then
                'none
            else
                addWordToken buffer @state

                setMode 'default @state

                lexOne buffer char @state

        'numberLiteral:
            if isNumber char then
                'none
            else if char == "%" then
                addNumberToken 'true buffer @state

                setMode 'default @state
            else
                addNumberToken 'false buffer @state

                setMode 'default @state

                lexOne buffer char @state

        'squiggles:
            if isSquiggle char then
                'none
            else
                addSquiggleToken buffer (char == " ") @state

                setMode 'default @state

                lexOne buffer char @state

        'contentOpeningQuotes_One:
            if char == "\"" then
                setMode 'contentOpeningQuotes_Two @state
            else if char == "" then
                addError "there's no closing quotes" @state
            else
                @state.tokenStart := getPos @state - 1

                setMode ('singleQuote { lastEscape = -1 }) @state

                lexOne buffer char @state

        'contentOpeningQuotes_Two:
            if char == "\"" then
                @state.tokenStart := getPos @state - 2

                setMode ('tripleQuote { closingQuotes = 0, lastEscape = -1 }) @state
            else
                addContentTokenRel -2 0 (Token.'textLiteral Token.'singleQuote "") @state

                setMode 'default @state

                lexOne buffer char @state

        'singleQuote { lastEscape }:
            previousIsEscape =
                pos == lastEscape + 1

            if char == "" then
                addError "there's no closing quotes" @state
            else if previousIsEscape then
                setMode ('singleQuote { lastEscape }) @state
            else
                try char as

                    "\"":
                        start =
                            cloneUni @state.tokenStart

                        end =
                            pos + 1

                        value =
                            buffer >> Text.slice (start + 1) (end - 1) __

#                      >> Text.replace "\\\"" "\"" __

                        addContentTokenAbs start end (Token.'textLiteral Token.'singleQuote value) @state

                        setMode 'default @state

                    "\\":
                        setMode ('singleQuote { lastEscape = pos }) @state

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
                        'none

        'tripleQuote { closingQuotes, lastEscape }:
            previousIsEscape =
                pos == lastEscape + 1

            if char == "" then
                addError "unterminated triple quotes" @state
            else if previousIsEscape then
                setMode ('tripleQuote { closingQuotes = 0, lastEscape }) @state
            else
                try char as

                    "\"":
                        if closingQuotes == 2 then
                            # TODO maybe move away the finalization code from here?
                            start =
                                cloneUni @state.tokenStart

                            end =
                                pos + 1

                            buffer
                            >> Text.slice (start + 3) (end - 3) __
                            >> unindent
                            >> Token.'textLiteral Token.'tripleQuote __
                            >> addContentTokenAbs start end __ @state

                            setMode 'default @state
                        else
                            setMode ('tripleQuote { closingQuotes = closingQuotes + 1, lastEscape }) @state

                    "\\":
                        setMode ('tripleQuote { closingQuotes = 0, lastEscape = pos }) @state

                    _:
                        setMode ('tripleQuote { closingQuotes = 0, lastEscape }) @state

        'lineComment { column, start }:
            if char == "\n" or char == "" then
                {
                , indent = column
                , isBlock = 'false
                , isFollowedByBlank = thereIsABlankAhead 0 buffer @state
                }
                >> Token.'comment
                >> addCommentTokenAbs start (getPos @state) __ @state

                setMode 'default @state

                lexOne buffer char @state
            else
                'none

        'contentOpeningBlockComment:
            if char == "#" then
                start =
                    getPos @state - 1

                column =
                    cloneUni @state.column - 1

                setMode ('blockComment { column, nesting = 1, previous = "", start }) @state
            else
                addContentTokenRel -1 0 (Token.'squareBracket (cloneUni @state.line) Token.'open) @state

                setMode 'default @state

                lexOne buffer char @state

        'blockComment { column, nesting, previous, start }:
            continueWithDeltaNesting =
                fn dn:
                setMode ('blockComment { column, nesting = nesting + dn, previous = char, start }) @state

            try previous & char as

                "[" & "#":
                    continueWithDeltaNesting 1

                "#" & "]":
                    if nesting > 1 then
                        continueWithDeltaNesting -1
                    else
                        {
                        , indent = column
                        , isBlock = 'true
                        , isFollowedByBlank = thereIsABlankAhead 1 buffer @state
                        }
                        >> Token.'comment
                        >> addCommentTokenAbs start (getPos @state + 1) __ @state

                        setMode 'default @state

                _ & "":
                    addError "unterminated block comment" @state

                _:
                    continueWithDeltaNesting 0


thereIsABlankAhead as fn Int, Text, @ReadState: Bool =
    fn offset, buffer, @state:
    # This is so bad.... XD
    start =
        cloneUni @state.nextPos + offset >> Text.fromNumber

    regex =
        ".{" .. start .. "}\n[ ]*(\n|$)"

#    log "BUFFER" buffer
#    log "REGEX" regex

    (Text.startsWithRegex regex) buffer /= ""


tryIndent as fn Text, Text, Text, @ReadState: None =
    fn buffer, indentChar, char, @state:
    if char == indentChar or char == "" then
        'none
    else if char == " " or char == "\t" then
        addError "mixing tabs and spaces!" @state
    else if char == "\n" then
        # line is empty, ignore
        @state.tokenStart := getPos @state + 1

        setMode 'indent @state
    else if char == "#" then
        start =
            getPos @state

        column =
            cloneUni @state.column

        setMode ('lineComment { column, start }) @state
    else
        @state.lineIndent := cloneUni @state.column

        setMode 'default @state

        lexOne buffer char @state


closeOpenBlocks as fn @ReadState: None =
    fn @state:
    pos =
        getPos @state

    s =
        Array.toList @state.indentStack

    List.each s fn _:
        Array.push @state.tokens ('token pos pos Token.'blockEnd)

    Array.push @state.sections (Array.toList @state.tokens)


lexer as fn Bool, Error.Module: Res [ [ Token ] ] =
    fn keepComments, module:
    #Debug.benchStart None

    moduleCode =
        module.content

    !state =
        readStateInit keepComments

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

        []:
            closeOpenBlocks @state

            Array.toList @state.sections >> 'ok

        errors:
            errors
            >> List.map __ (fn e: e module)
            >> Error.'nested
            >> 'err
