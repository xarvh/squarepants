#
# Buffer
#
# I don't know yet how I want to implement text parsing, so I'll keep it abstract for now
#
alias Buffer =
    {
    , tail as Text
    , pos as Number
    }


init s =
    as Text -> Buffer
    {
    , tail = s
    , pos = 0
    }


pos b =
    as Buffer -> Number

    b.pos


consume l b =
    as Number -> Buffer -> Buffer

    {
    , tail = Text.slice 0 l b.tail
    , pos = b.pos + l
    }


startsWith sub b =
    as Text -> Buffer -> Maybe Buffer

    if Text.startsWith sub b.tail then
        Just << consume (Text.length sub) b
    else
        Nothing


regexMatch regex b =
    as Text -> Buffer -> Maybe (Text & Buffer)

    # TODO use try..as once it is fixed
    match = Text.startsWithRegex regex b.tail
    if match == "" then
        Nothing
    else
        Just << match & consume (Text.length match) b

atEnd b =
    as Buffer -> Bool

    b.tail == ""



#
# Lexer
#
alias ReadState =
    {
    , buffer as Buffer
    #, codeAsString as String
    , moduleName as Text
    #, code as List Char
    , multiCommentDepth as Number
    , indentStack as [ Number ]
    , maybeIndentToAdd as Maybe Number
    , accum as [ Token ]
    }


lexer moduleName moduleCode =
    as Text -> Text -> Res [ Token ]

    { buffer = init << "\n" .. moduleCode
    #, codeAsString = moduleCode
    , moduleName = moduleName
    , multiCommentDepth = 0
    , indentStack = []
    , maybeIndentToAdd = Just 0
    , accum = []
    }
        >> lexerStep


lexerStep state =
    as ReadState -> Res [Token]

    if atEnd state.buffer then
        state
            >> closeOpenBlocks
            >> List.reverse
            >> Ok
    else
        # TODO assert that each iteration eats at least one char
        state
            >> lexContent pos state.buffer
            >> Result.map lexerStep


closeOpenBlocks state =
    as ReadState -> [ Token ]

    blockEnd =
        as Token
        {
        , kind = Token.BlockEnd
        , start = pos state.buffer
        , end = pos state.buffer
        }

    List.foldl (fn stack accum: blockEnd :: accum) state.indentStack state.accum


lexContent startPos state =
    as Number -> ReadState -> Res ReadState


    tryString string contentAhead updateState =
        as Bool -> Text -> Number -> (ReadState -> Res ReadState) -> Maybe (Res ReadState)

        p = pos state.buffer

        state.buffer
            >> startsWith string
            >> Maybe.map fn newBuffer:
                Ok state
                    >> chainIf (contentAhead or p > startPos) (addIndentTokens startPos)
                    >> chainIf (p > startPos) (contentLineToTokens startPos)
                    >> Result.andThen fn newState:
                        updateState { newState with buffer = newBuffer }


    maybeSuccessfulTry =
        [
        , fn _: tryString "\"\"\"" True (lexHardQuotedString p)
        , fn _: tryString "\"" True (lexSoftQuotedString p)
        , fn _: tryString "#" False (lexSingleLineComment p)
        , fn _: tryString "[#" False (lexMultiLineComment p)
        , fn _: tryString "\n" False (lexIndent >> Ok)
        , fn _:
            if atEnd state.buffer then
                Nothing
            else
                lexContent startPos { state with buffer = consume 1 .buffer }
        , fn _: tryString "" False Ok
        ]
            >> List.mapFirst (fn f: f None)

    try maybeSuccessfulTry as
      Just result:
          result

      Nothing:
          Debug.todo "rewrite this function because it's terrible"


chainIf predicate f result =
    as Bool -> (state -> Result err state) -> Result err state -> Result err state

    result >> Result.andThen fn state:
        if predicate then
            f state

        else
            Ok state



addIndentTokens endPos state =
    as Number -> ReadState -> Res ReadState

    try state.maybeIndentToAdd as
        Nothing:
            Ok state

        Just newIndent:
            addIndentTokensRec endPos newIndent True { state with maybeIndentToAdd = Nothing } state.indentStack


addIndentTokensRec endPos newIndent isFirstRecursion state stack =
    as Number -> Number -> Bool -> ReadState -> List Number -> Res ReadState

    lastIndent & poppedStack =
        try stack as
            []:
                0 & []

            head :: tail:
                head & tail

    makeToken kind =
        { kind = kind
        , start = endPos - newIndent
        , end = endPos
        }

    if newIndent == lastIndent then
        [#
           ```
           lastIndent
           newIndent
           ```

           ```
           previousRecursionIndent
             lastIndent
           newIndent
           ```
        #]
        Ok { state with accum = makeToken Token.NewSiblingLine :: state.accum, indentStack = stack }

    else if newIndent > lastIndent then
        if isFirstRecursion then
            [#
               ```
               lastIndent
                 newIndent
               ```
            #]
            Ok
                { state with
                , accum = makeToken Token.BlockStart :: state.accum
                , indentStack = newIndent :: state.indentStack
                }

        else
            [# This is an error:
               ```
               previousRecursionIndent
                   lastIndent
                 newIndent
               ```
            #]
            Error.res
                {
                , moduleName = state.moduleName
                , start = pos state.buffer
                , end = endPos
                , description =
                    fn _:
                        [
                        , Error.text << "last indent was at row " .. Text.fromInt lastIndent
                        , Error.text << "but this new indent is at row " .. Text.fromInt newIndent
                        ]
                }






    else
        addIndentTokensRec endPos newIndent False { state with accum = makeToken Token.BlockEnd :: state.accum } poppedStack



contentLineToTokens startPos state =
    as Number -> ReadState -> Res ReadState

    contentLine =
        Text.slice startPos (pos state.buffer) state.codeAsString

    state.accum
        # TODO (horrible) I'm adding a space in front so that indent will not
        # eat all of the spaces in front of "  -a", so that `-` can be recognised as Unop
        >> contentLineToTokensRec (" " .. contentLine) (startPos - 1)
        >> Result.mapError (fn stateToError: stateToError state)
        >> Result.map (fn tokens: { state with accum = tokens })


contentLineToTokensRec untrimmedBlock untrimmedPos tokenAccu =
    as Text -> Number -> [ Token ] -> Result (ReadState -> Error) [ Token ]

    try Text.trimLeft untrimmedBlock as
        "":
            Ok tokenAccu

        codeBlock:
            spaces =
                Text.length untrimmedBlock - Text.length codeBlock

            start =
                untrimmedPos + spaces

            tryMatch ( regex & constructor ) =
                try Regex.find regex untrimmedBlock as
                    match :: tail:
                        Just << match & constructor

                    []:
                        Nothing

            try mapFind tryMatch recognisedTokens as
                Nothing:
                    Err << errorInvalidToken start codeBlock

                Just ( match & constructor ):
                    try constructor match.match as
                        Err stateToError:
                            Err << stateToError start

                        Ok ( tokenKind & charsConsumed ):
                            tokenStart =
                                # TODO maybe should just assert that match.index is 0?
                                start + match.index

                            tokenEnd =
                                tokenStart + charsConsumed - spaces

                            token =
                                { kind = tokenKind
                                , start = tokenStart
                                , end = tokenEnd
                                }

                            newBlock =
                                Text.dropLeft charsConsumed untrimmedBlock

                            accu =
                                token :: tokenAccu

                            contentLineToTokensRec newBlock tokenEnd accu
