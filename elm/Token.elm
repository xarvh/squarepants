module Token exposing (..)

import Chunks exposing (Chunk)
import Error exposing (Error)
import ParseIndent exposing (Indented(..), IndentedChunk)
import Regex exposing (Regex)


type alias IndentedToken =
    -- TODO preserve position!
    Indented Token


type Token
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


recognisedToken : List ( Regex, String -> Token )
recognisedToken =
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
        NormalChunk chunk ->
            case chunk.t of
                Chunks.ContentLine ->
                    tokenAccu
                        |> contentLineToTokensRec (String.slice chunk.start chunk.end code)
                        |> Result.mapError (\errorKind -> Error errorKind chunk.start)

                Chunks.SoftQuotedString ->
                    (code
                        -- TODO remove quotes?
                        -- TODO remove escapes
                        |> String.slice chunk.start chunk.end
                        |> StringLiteral
                        |> NormalChunk
                    )
                        :: tokenAccu
                        |> Ok

                Chunks.HardQuotedString ->
                    (code
                        -- TODO remove quotes?
                        -- TODO remove escapes
                        |> String.slice chunk.start chunk.end
                        |> StringLiteral
                        |> NormalChunk
                    )
                        :: tokenAccu
                        |> Ok

                -- TODO do we want comments to make it to the AST?
                _ ->
                    Ok tokenAccu

        NewLine ->
            NewLine :: tokenAccu |> Ok

        BlockStart ->
            BlockStart :: tokenAccu |> Ok

        BlockEnd ->
            BlockEnd :: tokenAccu |> Ok


contentLineToTokensRec : String -> List IndentedToken -> Result Error.Kind (List IndentedToken)
contentLineToTokensRec untrimmedCodeChunk tokenAccu =
    let
        codeChunk =
            String.trimLeft untrimmedCodeChunk

        tryMatch ( regex, constructor ) =
            case Regex.find regex codeChunk of
                match :: tail ->
                    Just ( match, constructor )

                [] ->
                    Nothing
    in
    case mapFind tryMatch recognisedToken of
        Nothing ->
            { token = codeChunk }
                |> Error.InvalidToken
                |> Err

        Just ( match, constructor ) ->
            let
                remainder =
                    codeChunk
                        |> String.dropLeft (match.index + String.length match.match)
                        |> String.trimLeft

                token =
                    constructor match.match

                accu =
                    NormalChunk token :: tokenAccu
            in
            if remainder == "" then
                Ok accu

            else
                contentLineToTokensRec remainder accu


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
