#
# Buffer
#
# I don't know yet how I want to implement text parsing, so I'll keep it abstract for now
#
alias Buffer =
    {
    , tail as Text
    , pos as Int
    }


init s =
    as Text -> Buffer
    {
    , tail = s
    , pos = 0
    }


pos b =
    as Buffer -> Int

    b.pos


consume l b =
    as Int -> Buffer -> Buffer

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
    , codeAsString as Text
    , moduleName as Text
    #, code as List Char
    , multiCommentDepth as Int
    , indentStack as [ Int ]
    , maybeIndentToAdd as Maybe Int
    , accum as [ Token ]
    }


lexer moduleName moduleCode =
    as Text -> Text -> Res [ Token ]

    { buffer = init << "\n" .. moduleCode
    , codeAsString = moduleCode
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
            >> lexContent (pos state.buffer)
            >> Result.andThen lexerStep


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
    as Int -> ReadState -> Res ReadState

    tryString string contentAhead updateState =
        as Text -> Bool -> (ReadState -> Res ReadState) -> Maybe (Res ReadState)

        p =
            pos state.buffer

        try startsWith string state.buffer as
            Nothing:
                Nothing

            Just newBuffer:
                Ok state
                    >> chainIf (contentAhead or p > startPos) (addIndentTokens startPos)
                    >> chainIf (p > startPos) (contentLineToTokens startPos)
                    >> Result.andThen (fn newState: updateState { newState with buffer = newBuffer })
                    >> Just

    maybeSuccessfulTry =
        as Maybe (a -> Maybe (Res ReadState))
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
    as Int -> ReadState -> Res ReadState

    try state.maybeIndentToAdd as
        Nothing:
            Ok state

        Just newIndent:
            addIndentTokensRec endPos newIndent True { state with maybeIndentToAdd = Nothing } state.indentStack


addIndentTokensRec endPos newIndent isFirstRecursion state stack =
    as Int -> Int -> Bool -> ReadState -> List Int -> Res ReadState

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
    as Int -> ReadState -> Res ReadState

    contentLine =
        Text.slice startPos (pos state.buffer) state.codeAsString

    state.accum
        # TODO (horrible) I'm adding a space in front so that indent will not
        # eat all of the spaces in front of "  -a", so that `-` can be recognised as Unop
        >> contentLineToTokensRec (" " .. contentLine) (startPos - 1)
        >> Result.mapError (fn stateToError: stateToError state)
        >> Result.map (fn tokens: { state with accum = tokens })



alias Regex = Text -> Text


contentLineToTokensRec untrimmedBlock untrimmedPos tokenAccu =
    as Text -> Int -> [ Token ] -> Result (ReadState -> Error) [ Token ]

    try Text.trimLeft untrimmedBlock as
        "":
            Ok tokenAccu

        codeBlock:
            spaces =
                Text.length untrimmedBlock - Text.length codeBlock

            start =
                untrimmedPos + spaces

            tryMatch ( regex & constructor ) =
                as Regex & ( Text -> cons ) -> Maybe (Text & constructor)
                match =
                    regex untrimmedBlock

                # TODO use try..as once it's fixed
                if match == "" then
                    Nothing
                else
                    Just << match & constructor

            Ok []
#            try List.mapFirst tryMatch recognisedTokens as
#                Nothing:
#                    Err << errorInvalidToken start codeBlock
#
#                Just ( match & constructor ):
#                    try constructor match.match as
#                        Err stateToError:
#                            Err << stateToError start
#
#                        Ok ( tokenKind & charsConsumed ):
#                            tokenStart =
#                                # TODO maybe should just assert that match.index is 0?
#                                start + match.index
#
#                            tokenEnd =
#                                tokenStart + charsConsumed - spaces
#
#                            token =
#                                { kind = tokenKind
#                                , start = tokenStart
#                                , end = tokenEnd
#                                }
#
#                            newBlock =
#                                Text.dropLeft charsConsumed untrimmedBlock
#
#                            accu =
#                                token :: tokenAccu
#
#                            contentLineToTokensRec newBlock tokenEnd accu


alias Constructor = Text -> Result (Int -> ReadState -> Error) ( Token.Kind & Int )


recognisedTokens =
    as List ( Regex & Constructor )

    recordEntryToTuple record =
        Text.startsWithRegex record.regex
        &
        fn match:
            match
                >> record.constructor
                >> Result.map fn kind: kind & record.consumed match

    parenRegex s kind =
        {
        , regex = "[ ]*\\" .. s
        , consumed = Text.length
        , constructor = fn match: Ok kind
        }

    List.map recordEntryToTuple
        [
        , { # Ints
          , regex = "[ ]*[0-9]+[.]?[0-9_]*"
          , consumed = Text.length
          , constructor = fn match: match >> Text.trimLeft >> Token.NumberLiteral >> Ok
          }
        , { # Words
          , regex = "[ ]*[@]?[a-zA-Z._][a-zA-Z./_0-9]*"
          , consumed = Text.length
          , constructor =
                fn m:
                    match =
                            Text.trimLeft m

                    (try match as
                        "..":
                            Token.Binop m Prelude.textConcat

                        "fn":
                            Token.Fn

                        "if":
                            Token.If

                        "try":
                            Token.Try

                        "as":
                            Token.As

                        "then":
                            Token.Then

                        "else":
                            Token.Else

                        "with":
                            Token.With

                        "and":
                            Token.Binop m Prelude.and_

                        "or":
                            Token.Binop m Prelude.or_

                        "not":
                            Token.Unop Prelude.not_

                        _ :
                            try Text.uncons match as
                                Nothing:
                                    Debug.todo "not happening"

                                Just ( head & tail ):
                                    if head == "@" then
                                        Token.Name { mutable = True } tail

                                    else
                                        Token.Name { mutable = False } match
                    )
                        >> Ok
          }

        # Parens
        , parenRegex "(" << Token.RoundParen Token.Open
        , parenRegex ")" << Token.RoundParen Token.Closed
        , parenRegex "[" << Token.SquareBracket Token.Open
        , parenRegex "]" << Token.SquareBracket Token.Closed
        , parenRegex "{" << Token.CurlyBrace Token.Open
        , parenRegex "}" << Token.CurlyBrace Token.Closed
        , parenRegex "," << Token.Comma
        , { # Unary addittive
            # the `>` at the end is to avoid matching `->`
          , regex = "[ ]+[+-][^ >=]"
          , consumed = fn match: Text.length match - 1
          , constructor =
                fn m:
                    try m >> Text.trimLeft >> Text.dropRight 1 as
                        "+":
                            Ok << Token.Unop Prelude.unaryPlus

                        "-":
                            Ok << Token.Unop Prelude.unaryMinus

                        op :
                            Err << errorUnknownOperator op
          }
        , { # Squiggles
          , regex = "[ ]*[=+\\-*/:><!&^|@]+"
          , consumed = Text.length
          , constructor =
                fn m:
                    match =
                        Text.trimLeft m

                    try match as
                        "->":
                            Ok << Token.Arrow { mutable = False }

                        "@>":
                            Ok << Token.Arrow { mutable = True }

                        ":":
                            Ok << Token.Colon

                        "=":
                            Ok << Token.Defop { mutable = False }

                        "@=":
                            Ok << Token.Defop { mutable = True }

                        op:
                            try Dict.get match Prelude.binops as
                                Nothing:
                                    Err << errorUnknownOperator op

                                Just binop:
                                    Ok << Token.Binop m binop
          }
        ]

errorUnknownOperator op =
    as Text -> Int -> ReadState -> Error
    Debug.todo "not implemented: errorUnknownOperator"
