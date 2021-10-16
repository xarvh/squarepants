
readWhile test =
    is (char -> Bool) -> List char -> Int & List char

    rec counter list =
        is Int -> List char -> Int & List char

        try list as
            []:
                counter & list

            head :: tail:
                if test head:
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
    , moduleName is Text
    , multiCommentDepth is Int
    , indentStack is [ Int ]
    , maybeIndentToAdd is Maybe Int
    , accum is [ Token ]
    }


readStateInit moduleName moduleCode =
    is Text -> Text -> ReadState

    { buffer = Buffer.init << "\n" .. moduleCode
    , moduleName = moduleName
    , multiCommentDepth = 0
    , indentStack = []
    , maybeIndentToAdd = Just 0
    , accum = []
    }


getPos state =
    is ReadState -> Int

    Buffer.pos state.buffer



resError pos state message =
    is Int -> ReadState -> [Text] -> Res a

    #Error.markdown (CA.P state.moduleName (getPos state) pos) message
    Error.res
        {
        , moduleName = state.moduleName
        , start = pos
        , end = getPos state
        , description = fn _: List.map Error.text message
        }



lexer moduleName moduleCode =
    is Text -> Text -> Res [ Token ]

    readStateInit moduleName moduleCode >> lexerStep



lexerStep state =
    is ReadState -> Res [Token]

    if Buffer.atEnd state.buffer:
        state
            >> closeOpenBlocks
            >> List.reverse
            >> Ok
    else
        # TODO assert that each iteration eats at least one char
        state
            >> lexContent (getPos state)
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
            if Buffer.atEnd state.buffer:
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
        if predicate:
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

    if newIndent == lastIndent:
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

    else if newIndent > lastIndent:
        if isFirstRecursion:
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
        Buffer.slice startPos (getPos state) state.buffer

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
                if match == "":
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

                        "is":
                            Token.Is

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
                                    if head == "@":
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
            getPos state + length

        { state with
            , buffer = consume (endPos - startPos) state.buffer
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

        rec isEscape bf0 =
            is Bool -> Buffer -> Res ReadState

            try Buffer.readOne bf0 as
                "\\" & bf1:
                    rec (not isEscape) bf1

                "\"" & bf1:
                    if isEscape:
                        rec False bf1

                    else:
                        endPos = Buffer.pos bf1
                        Ok
                            { state with
                                , buffer = bf1
                                , accum =
                                    { kind =
                                        state.buffer
                                            >> Buffer.slice (startPos + 1) (endPos - 1)
                                            # TODO transform escapes and reject non-escapable chars
                                            >> Token.TextLiteral
                                    , start = startPos
                                    , end = endPos
                                    }
                                        :: state.accum
                            }

                "\n" & bf1:
                    # https://www.reddit.com/r/ProgrammingLanguages/comments/l0ptdl/why_do_so_many_languages_not_allow_string/gjvrcg2/
                    resError (Buffer.pos bf1) state
                        [
                        , "Single-quoted text cannot contain newlines."
                        , "Is it possible you forgot a closing \"?"
                        , "If you want a Text with multiple lines, use the triple quotes \"\"\" instead."
                        ]

                "" & bf1:
                    resError startPos state
                        [
                        , "The file ended without a \" to close the text!"
                        ]

                char & bf1:
                    rec False bf1

        rec False state.buffer


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

        rec isEscape doubleQuotes bf0 =
            is Bool -> Int -> Buffer -> Res ReadState

            try Buffer.readOne bf0 as
                "\\" & bf1:
                    rec (not isEscape) 0 bf1

                "\"" & bf1:
                    endPos =
                        Buffer.pos bf1

                    if isEscape:
                        rec False 0 bf1

                    else if doubleQuotes < 2:
                        rec False (doubleQuotes + 1) bf1

                    else
                        Ok
                            { state with
                                , buffer = bf1
                                , accum =
                                    { kind =
                                        state.buffer
                                            >> Buffer.slice (startPos + 3) (endPos - 3)
                                            # TODO transform escapes and reject non-escapable chars
                                            >> Token.TextLiteral
                                    , start = startPos
                                    , end = endPos
                                    }
                                        :: state.accum
                            }

                "" & bf1:
                    resError (Buffer.pos bf1) state [
                        , "The file ended without a \"\"\" to close the text!"
                        ]

                char & bf1:
                    rec False 0 bf1


        rec False 0 state.buffer



[# TESTS

  - properly nested comments should pass
  - improperly nested comments should fail
  - non-terminated comments should fail
  - state.pos is updated manually, so it should be tested!

#]
lexMultiLineComment startPos state =
        is Int -> ReadState -> Res ReadState

        rec currentPos depth code =
            try code as
                "[" :: "#" :: rest:
                    rec (currentPos + 1) (depth + 1) rest

                "#" :: "]" :: rest:
                    endPos =
                        currentPos + 2

                    if depth > 0:
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
                    rec (currentPos + 1) depth rest

                []:
                    errorUnterminatedMultilineComment currentPos state

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
