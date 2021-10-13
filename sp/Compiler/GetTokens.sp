#
# Buffer
#
# I don't know yet how I want to implement text parsing, so I'll keep it abstract for now
#
alias Buffer =
    {
    , tail is Text
    , pos is Int
    }


init s =
    is Text -> Buffer
    {
    , tail = s
    , pos = 0
    }


pos b =
    is Buffer -> Int

    b.pos


consume l b =
    is Int -> Buffer -> Buffer

    {
    , tail = Text.slice 0 l b.tail
    , pos = b.pos + l
    }


startsWith sub b =
    is Text -> Buffer -> Maybe Buffer

    if Text.startsWith sub b.tail then
        Just << consume (Text.length sub) b
    else
        Nothing


regexMatch regex b =
    is Text -> Buffer -> Maybe (Text & Buffer)

    # TODO use try..as once it is fixed
    match = Text.startsWithRegex regex b.tail
    if match == "" then
        Nothing
    else
        Just << match & consume (Text.length match) b

atEnd b =
    is Buffer -> Bool

    b.tail == ""


readWhile test =
    is (char -> Bool) -> List char -> Int & List char

    rec counter list =
        is Int -> List char -> Int & List char

        try list as
            []:
                counter & list

            head :: tail:
                if test head then
                    rec (counter + 1) tail

                else
                    counter & list

    rec 0

#
# Lexer
#
alias ReadState =
    {
    , buffer is Buffer
    , codeAsString is Text
    , moduleName is Text
    #, code is List Char
    , multiCommentDepth is Int
    , indentStack is [ Int ]
    , maybeIndentToAdd is Maybe Int
    , accum is [ Token ]
    }


lexer moduleName moduleCode =
    is Text -> Text -> Res [ Token ]

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
    is ReadState -> Res [Token]

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
    is ReadState -> [ Token ]

    blockEnd =
        is Token
        {
        , kind = Token.BlockEnd
        , start = pos state.buffer
        , end = pos state.buffer
        }

    List.foldl (fn stack accum: blockEnd :: accum) state.indentStack state.accum


lexContent startPos state =
    is Int -> ReadState -> Res ReadState

    tryString string contentAhead updateState =
        is Text -> Bool -> (ReadState -> Res ReadState) -> Maybe (Res ReadState)

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


    xxx =
        is [a -> Maybe (Res ReadState)]
        [
        , fn _: tryString "\"\"\"" True (lexHardQuotedString p)
        , fn _: tryString "\"" True (lexSoftQuotedString p)
        , fn _: tryString "#" False (lexSingleLineComment p)
        , fn _: tryString "[#" False (lexMultiLineComment p)
        , fn _: tryString "\n" False (fn x: x >> lexIndent >> Ok)
        , fn _:
            if atEnd state.buffer then
                Nothing
            else
                Just << lexContent startPos { state with buffer = consume 1 .buffer }
        , fn _: tryString "" False Ok
        ]



    maybeSuccessfulTry =
        is Maybe (Res ReadState)
        List.mapFirst (fn f: f None) xxx

    try maybeSuccessfulTry as
      Just result:
          result

      Nothing:
          Debug.todo "rewrite this function because it's terrible"


chainIf predicate f result =
    is Bool -> (state -> Result err state) -> Result err state -> Result err state

    result >> Result.andThen fn state:
        if predicate then
            f state

        else
            Ok state



addIndentTokens endPos state =
    is Int -> ReadState -> Res ReadState

    try state.maybeIndentToAdd as
        Nothing:
            Ok state

        Just newIndent:
            addIndentTokensRec endPos newIndent True { state with maybeIndentToAdd = Nothing } state.indentStack


addIndentTokensRec endPos newIndent isFirstRecursion state stack =
    is Int -> Int -> Bool -> ReadState -> List Int -> Res ReadState

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
    is Int -> ReadState -> Res ReadState

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
    is Text -> Int -> [ Token ] -> Result (ReadState -> Error) [ Token ]

    try Text.trimLeft untrimmedBlock as
        "":
            Ok tokenAccu

        codeBlock:
            spaces =
                Text.length untrimmedBlock - Text.length codeBlock

            start =
                untrimmedPos + spaces

            tryMatch ( regex & constructor ) =
                is Regex & ( Text -> cons ) -> Maybe (Text & constructor)
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
    is List ( Regex & Constructor )

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
    is Text -> Int -> ReadState -> Error
    Debug.todo "not implemented: errorUnknownOperator"


#
# Block lexers
#


[# TESTS

  - state.pos is updated manually, so it should be tested!

#]
lexSingleLineComment startPos state =
        is Int -> ReadState -> Res ReadState

        length & rest =
            readWhile (fn c: c /= "\n") state.code

        endPos =
            state.pos + length

        { state with
            , pos = endPos
            , code = rest
            , accum =
                { kind = Token.Comment
                , start = startPos
                , end = endPos
                }
                    :: state.accum
        }
            >> Ok


[# TESTS

  - `"\""` passes

  - `"\n"` passes

  - `"
    "` fails

  - `"sdf\\asdf\\"` passes

  - `"sdf\\\asdf\\"` passes

  - `"sdf\\asdf\\\"` fails

  - state.pos is updated manually, so it should be tested!

#]
lexSoftQuotedString startPos state =
        is Int -> ReadState -> Res ReadState

        rec pos isEscape code =
            is Int -> Bool -> Text -> Res ReadState

            try code as
                "\\" :: rest:
                    rec (pos + 1) (not isEscape) rest

                "\"" :: rest:
                    endPos =
                        pos + 1

                    if isEscape then
                        rec endPos False rest

                    else
                        Ok
                            { state with
                                , pos = endPos
                                , code = rest
                                , accum =
                                    { kind =
                                        state.codeAsString
                                            >> String.slice (startPos + 1) (endPos - 1)
                                            # TODO transform escapes and reject non-escapable chars
                                            >> Token.TextLiteral
                                    , start = startPos
                                    , end = endPos
                                    }
                                        :: state.accum
                            }

                "\n" :: rest:
                    # https://www.reddit.com/r/ProgrammingLanguages/comments/l0ptdl/why_do_so_many_languages_not_allow_string/gjvrcg2/
                    errorNewLineInsideSoftQuote pos state

                char :: rest:
                    rec (pos + 1) False rest

                []:
                    errorUnterminatedTextLiteral pos state

        rec state.pos False state.code


[# TESTS

  - """ passes

  - "\\n" passes

  - "
    " fails

  - "sdf\\asdf\\" passes

  - "sdf\\\\asdf\\" passes

  - "sdf\\asdf\\" fails

  - state.pos is updated manually, so it should be tested!

#]
lexHardQuotedString startPos state =
        is Int -> ReadState -> Res ReadState

        rec pos isEscape doubleQuotes code =
            is Int -> Bool -> Int -> Text -> Res ReadState

            try code as
                "\\" :: rest:
                    rec (pos + 1) (not isEscape) 0 rest

                "\"" :: rest:
                    endPos =
                        pos + 1

                    if isEscape then
                        rec endPos False 0 rest

                    else if doubleQuotes < 2 then
                        rec endPos False (doubleQuotes + 1) rest

                    else
                        Ok
                            { state with
                                , pos = endPos
                                , code = rest
                                , accum =
                                    { kind =
                                        state.codeAsString
                                            >> String.slice (startPos + 3) (endPos - 3)
                                            # TODO transform escapes and reject non-escapable chars
                                            >> Token.TextLiteral
                                    , start = startPos
                                    , end = endPos
                                    }
                                        :: state.accum
                            }

                char :: rest:
                    rec (pos + 1) False 0 rest

                []:
                    errorUnterminatedTextLiteral pos state

        rec state.pos False 0 state.code


[# TESTS

  - properly nested comments should pass
  - improperly nested comments should fail
  - non-terminated comments should fail
  - state.pos is updated manually, so it should be tested!

#]
lexMultiLineComment startPos state =
        is Int -> ReadState -> Res ReadState

        rec pos depth code =
            try code as
                "[" :: "#" :: rest:
                    rec (pos + 1) (depth + 1) rest

                "#" :: "]" :: rest:
                    endPos =
                        pos + 2

                    if depth > 0 then
                        rec endPos (depth - 1) rest

                    else
                        Ok
                            { state with
                                , pos = endPos
                                , code = rest
                                , accum =
                                    { kind = Token.Comment
                                    , start = startPos
                                    , end = endPos
                                    }
                                        :: state.accum
                            }

                char :: rest:
                    rec (pos + 1) depth rest

                []:
                    errorUnterminatedMultilineComment pos state

        rec state.pos 0 state.code


[# LexIndent will successfully match anything, because a length 0 indent is valid.

NewSiblingLine, BlockStart, Block will be added only when we see that the line actually contains something.

#]
lexIndent state =
    is ReadState -> ReadState

    newIndent & newCode =
        readWhile (fn c: c == " ") state.code

    { state with
        , pos = newIndent + state.pos
        , code = newCode
        , maybeIndentToAdd = Just newIndent
    }
