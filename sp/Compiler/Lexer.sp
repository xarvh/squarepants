
readWhile test =
    is (Text -> Bool) -> Buffer -> Int & Buffer

    rec counter b =
        is Int -> Buffer -> Int & Buffer

        try Buffer.next b as
            "": counter & b
            char:
                if test char:
                  rec (counter + 1) (Buffer.skipAheadBy 1 b)
                else:
                  counter & b

    rec 0


readOne b =
    is Buffer -> Text & Buffer

    try Buffer.next b as
        "": "" & b
        char: char & Buffer.skipAheadBy 1 b



tryList ls default =
    is [ None -> Maybe b ] -> (None -> b) -> b

    try List.mapFirst (fn f: f None) ls as
        Just b: b
        Nothing: default None







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

    { buffer = Buffer.init << moduleCode
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

    # TODO is `pos` the start position?
    # TODO is `getPos state` the end position?
    Error.res (Pos.P state.moduleName pos (getPos state)) fn _: message



lexer moduleName moduleCode =
    is Text -> Text -> Res [Token]

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
        , start = Buffer.pos state.buffer
        , end = Buffer.pos state.buffer
        }

    List.foldl (fn stack accum: blockEnd :: accum) state.indentStack state.accum


lexContent startPos state =
    is Int -> ReadState -> Res ReadState

    runLexer contentAhead lexFunction buffer =
      is Bool -> (Int -> ReadState -> Res ReadState) -> Buffer -> Res ReadState
      p = Buffer.pos buffer
      Ok state
          >> chainIf (contentAhead or p > startPos) (addIndentTokens startPos)
          >> chainIf (p > startPos) (contentLineToTokens startPos)
          >> Result.andThen (fn newState: lexFunction startPos { newState with buffer = buffer })

    ts string contentAhead lexFunction _ =
        is Text -> Bool -> (Int -> ReadState -> Res ReadState) -> None -> Maybe (Res ReadState)

        state.buffer
          >> Buffer.startsWith string
          >> Maybe.map fn b:
                  runLexer contentAhead lexFunction b

    tryList
        [
        , ts "\"\"\"" True lexHardQuotedString
        , ts "\"" True lexSoftQuotedString
        , ts "#" False lexSingleLineComment
        , ts "[#" False lexMultiLineComment
        , ts "\n" False lexIndent
        ]
        fn _:
            x = readOne state.buffer
            try x as
              "" & b: runLexer False (fn _: Ok) b
              char & b: lexContent startPos { state with buffer = b }


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
            addIndentTokensRec endPos newIndent True state.indentStack { state with maybeIndentToAdd = Nothing }


addIndentTokensRec endPos newIndent isFirstRecursion stack state =
    is Int -> Int -> Bool -> List Int -> ReadState -> Res ReadState

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
        { state with
        , accum = makeToken Token.NewSiblingLine :: state.accum
        , indentStack = stack
        }
          >> Ok

    else if newIndent > lastIndent:
        if isFirstRecursion:
            [#
               ```
               lastIndent
                 newIndent
               ```
            #]
            { state with
            , accum = makeToken Token.BlockStart :: state.accum
            , indentStack = newIndent :: state.indentStack
            }
              >> Ok

        else
            [# This is an error:
               ```
               previousRecursionIndent
                   lastIndent
                 newIndent
               ```
            #]
            resError endPos state
                [
                , "last indent was at row " .. Text.fromInt lastIndent
                , "but this new indent is at row " .. Text.fromInt newIndent
                ]

    else
        addIndentTokensRec endPos newIndent False poppedStack { state with accum = makeToken Token.BlockEnd :: state.accum }



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
                is Regex & Constructor -> Maybe (Text & Constructor)
                match =
                    regex untrimmedBlock

                # TODO use try..as once it's fixed
                if match == "":
                    Nothing
                else
                    Just << match & constructor

            try List.mapFirst tryMatch recognisedTokens as
                Nothing:
                    Err fn state:
                        Error.Simple
                            {
                            , moduleName = state.moduleName
                            , start = start
                            , end = start + 10
                            , description = fn _:
                                [ "Unrecognized token: `" .. codeBlock .. "`"
                                ]
                            }

                Just ( match & constructor ):
                    try constructor match as
                        Err stateToError:
                            Err << stateToError start

                        Ok ( tokenKind & charsConsumed ):
                            tokenStart =
                                start

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
                            first = Text.slice 0 1 match
                            if first == "@":
                                Token.Name { mutable = True } (Text.dropLeft 1 match)
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

errorUnknownOperator op start state =
    is Text -> Int -> ReadState -> Error

    Error.Simple
        {
        , moduleName = state.moduleName
        , start = start
        , end = start + Text.length op
        , description = fn _:
            [ "Unknown operator: `" .. op .. "`"
            ]
        }


#
# Block lexers
#


[# TESTS

  - state.pos is updated manually, so it should be tested!

#]
lexSingleLineComment startPos state =
        is Int -> ReadState -> Res ReadState

        length & buffer =
            readWhile (fn c: c /= "\n") state.buffer

        { state with
            , buffer = buffer
            , accum =
                { kind = Token.Comment
                , start = startPos
                , end = Buffer.pos buffer
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

            try readOne bf0 as
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

            try readOne bf0 as
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
lexMultiLineComment _ state =
    is startPos -> ReadState -> Res ReadState

    rec depth b0 =
        is Int -> Buffer -> Res ReadState

        sw text f _ =
            Buffer.startsWith text b0 >> Maybe.map f

        tryList
          [ sw "[#" << rec (depth + 1)
          , sw "#]" << fn b1:
              if depth > 0:
                  rec (depth - 1) b1
              else:
                  Ok
                      { state with
                          , buffer = b1
                          , accum =
                              { kind = Token.Comment
                              , start = Buffer.pos state.buffer
                              , end = Buffer.pos b1
                              }
                                  :: state.accum
                      }
          ]
          fn _: try readOne b0 as
              "" & b1:
                    resError (Buffer.pos b1) state [
                        , "The file ended without a `#]` to close the comment!"
                        ]
              char & b1:
                rec depth b1

    rec 0 state.buffer


[# LexIndent will successfully match anything, because a length 0 indent is valid.

NewSiblingLine, BlockStart, Block will be added only when we see that the line actually contains something.

#]
lexIndent _ state =
    is startPos -> ReadState -> Res ReadState

    newIndent & buffer =
        readWhile (fn c: c == " ") state.buffer

    { state with
        , buffer = buffer
        , maybeIndentToAdd = Just newIndent
    }
        >> Ok
