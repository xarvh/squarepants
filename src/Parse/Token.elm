module Parse.Token exposing (..)

import Parse.Chunks exposing (Chunk)
import Parse.Error exposing (Error)
import Parse.Indent exposing (Indented(..), IndentedChunk)
import Regex exposing (Regex)


type alias IndentedToken =
    Indented Token


type alias Token =
    { kind : TokenKind
    , start : Int
    , end : Int
    }


type TokenKind
    = StringLiteral String
    | NumberLiteral String
    | Symbol String
    | If
    | Is
    | Then
    | Else
    | Return
    | Binop String
    | Unop String
    | RoundParen OpenOrClosed
    | SquareBracket OpenOrClosed
    | CurlyBrace OpenOrClosed
    | Comma


type OpenOrClosed
    = Open
    | Closed


recognisedTokens : List ( Regex, String -> TokenKind )
recognisedTokens =
    [ ( "^[0-9]+[.]?[0-9]*"
      , NumberLiteral
      )
    , ( "^[a-zA-Z._][a-zA-Z._0-9]*"
      , \s ->
            case s of
                "if" ->
                    If

                "is" ->
                    Is

                "then" ->
                    Then

                "else" ->
                    Else

                "and" ->
                    Binop "and"

                "or" ->
                    Binop "or"

                "return" ->
                    Return

                "risk" ->
                    Unop "risk"

                _ ->
                    Symbol s
      )
    , ( "^[,()\\[\\]{}]"
      , \s ->
            case s of
                "(" ->
                    RoundParen Open

                ")" ->
                    RoundParen Closed

                "[" ->
                    SquareBracket Open

                "]" ->
                    SquareBracket Closed

                "{" ->
                    CurlyBrace Open

                "}" ->
                    CurlyBrace Closed

                "," ->
                    Comma

                _ ->
                    Debug.todo "regex fail -_-"
      )
    , ( "^[=+\\-*/:><!^|#]+"
      , \s ->
            case s of
                "!" ->
                    Unop "!"

                _ ->
                    Binop s
      )
    ]
        |> List.map (Tuple.mapFirst reOrDie)


reOrDie reString =
    case Regex.fromString reString of
        Nothing ->
            Debug.todo "WTF"

        Just re ->
            re


chunksToTokens : String -> List IndentedChunk -> Result Error (List IndentedToken)
chunksToTokens code chunks =
    chunksToTokensRec code chunks []
        |> Result.map List.reverse


chunksToTokensRec : String -> List IndentedChunk -> List IndentedToken -> Result Error (List IndentedToken)
chunksToTokensRec code chunks tokenAccu =
    case chunks of
        [] ->
            Ok tokenAccu

        chunk :: chunksTail ->
            tokenAccu
                |> appendChunk code chunk
                |> Result.andThen (chunksToTokensRec code chunksTail)


appendChunk : String -> IndentedChunk -> List IndentedToken -> Result Error (List IndentedToken)
appendChunk code ic tokenAccu =
    case ic of
        Content chunk ->
            case chunk.t of
                Parse.Chunks.ContentLine ->
                    tokenAccu
                        |> contentLineToTokensRec code chunk.start chunk.end
                        |> Result.mapError (\errorKind -> Error errorKind chunk.start)

                Parse.Chunks.SoftQuotedString ->
                    chunkToStringLiteral code chunk :: tokenAccu |> Ok

                Parse.Chunks.HardQuotedString ->
                    chunkToStringLiteral code chunk :: tokenAccu |> Ok

                -- TODO do we want comments to make it to the AST?
                _ ->
                    Ok tokenAccu

        Structure structure ->
            Structure structure :: tokenAccu |> Ok


chunkToStringLiteral : String -> Chunk -> IndentedToken
chunkToStringLiteral code chunk =
    { kind =
        code
            -- TODO remove quotes?
            -- TODO remove escapes
            |> String.slice chunk.start chunk.end
            |> StringLiteral
    , start = chunk.start
    , end = chunk.end
    }
        |> Content


contentLineToTokensRec : String -> Int -> Int -> List IndentedToken -> Result Parse.Error.Kind (List IndentedToken)
contentLineToTokensRec code untrimmedStart chunkEnd tokenAccu =
    let
        chunkStart =
            skipSpacesFrom code untrimmedStart

        codeChunk =
            String.slice chunkStart chunkEnd code

        tryMatch ( regex, constructor ) =
            case Regex.find regex codeChunk of
                match :: tail ->
                    Just ( match, constructor )

                [] ->
                    Nothing
    in
    case mapFind tryMatch recognisedTokens of
        Nothing ->
            { token = codeChunk }
                |> Parse.Error.InvalidToken
                |> Err

        Just ( match, constructor ) ->
            let
                tokenStart =
                    chunkStart + match.index

                tokenEnd =
                    tokenStart + String.length match.match

                newStart =
                    skipSpacesFrom code tokenEnd

                token =
                    { kind = constructor match.match
                    , start = tokenStart
                    , end = tokenEnd
                    }

                accu =
                    Content token :: tokenAccu
            in
            if newStart >= chunkEnd then
                Ok accu

            else
                contentLineToTokensRec code newStart chunkEnd accu


skipSpacesFrom : String -> Int -> Int
skipSpacesFrom code start =
    let
        untrimmed =
            String.slice start -1 code

        trimmed =
            String.trimLeft untrimmed
    in
    start + String.length untrimmed - String.length trimmed


{-| This is mostly so that I don't have to run a successful `f` twice
-}
mapFind : (a -> Maybe b) -> List a -> Maybe b
mapFind f ls =
    case ls of
        [] ->
            Nothing

        head :: tail ->
            case f head of
                Just b ->
                    Just b

                Nothing ->
                    mapFind f tail
