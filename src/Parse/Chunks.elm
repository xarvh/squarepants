module Parse.Chunks exposing (..)

import Array exposing (Array)
import Parse.Error exposing (Error)


type ChunkType
    = ContentLine
    | Indent
    | SingleLineComment
    | MultiLineComment Int
    | SoftQuotedString
    | HardQuotedString


type alias Chunk =
    { t : ChunkType
    , start : Int
    , end : Int
    }



----
--- Chunks to Lines
--


toSemanticLines : List Chunk -> List (List Chunk)
toSemanticLines chunks =
    let
        init =
            { lines = []
            , lastLine = []
            }

        addChunkToAccum chunk accum =
            case chunk.t of
                Indent ->
                    { lines = List.reverse accum.lastLine :: accum.lines
                    , lastLine = [ chunk ]
                    }

                _ ->
                    { accum
                        | lastLine = chunk :: accum.lastLine
                    }
    in
    chunks
        |> List.foldl addChunkToAccum init
        |> .lines
        |> List.reverse
        |> List.drop 1



----
--- String (Array Char) to Chunks
--


type alias State =
    { position : Int
    , chunks : List Chunk
    , chunkType : ChunkType
    , chunkStart : Int

    -- This doesn't change
    , code : Array Char
    }


fromString : Array Char -> Result Error (List Chunk)
fromString code =
    { position = 0
    , chunks = []
    , chunkType = Indent
    , chunkStart = 0
    , code = code
    }
        |> stringToChunksRec
        |> Result.map (.chunks >> List.reverse)


stringToChunksRec : State -> Result Error State
stringToChunksRec state =
    if state.position >= Array.length state.code then
        state
            -- ContentLine is not really used
            |> addCurrentChunk 0 ContentLine
            |> Ok

    else
        let
            indent_Start =
                compare_discardAndChangeChunkType "\n" Indent

            indent_End =
                notSpace_dontConsume

            lineComment_Start =
                compare_changeChunkType_thenConsume "--" SingleLineComment

            lineComment_End =
                compare_discardAndChangeChunkType "\n" Indent

            hardQuotedString_Start =
                compare_changeChunkType_thenConsume "\"\"\"" HardQuotedString

            hardQuotedString_End =
                compare_consume_thenChangeChunkType "\"\"\"" ContentLine

            softQuotedString_Start =
                compare_changeChunkType_thenConsume "\"" SoftQuotedString

            softQuotedString_End =
                compare_consume_thenChangeChunkType "\"" ContentLine

            multiComment_Start chunkType =
                compare_changeChunkType_thenConsume "{-"
                    (case chunkType of
                        MultiLineComment depth ->
                            MultiLineComment (depth + 1)

                        _ ->
                            MultiLineComment 0
                    )

            multiComment_End depth =
                compare_consume_thenChangeChunkType "-}"
                    (if depth == 0 then
                        ContentLine

                     else
                        MultiLineComment <| depth - 1
                    )

            tests =
                case state.chunkType of
                    Indent ->
                        [ indent_End ]

                    ContentLine ->
                        [ errorOn "\t" Parse.Error.Tab
                        , multiComment_Start state.chunkType
                        , indent_Start
                        , lineComment_Start
                        , hardQuotedString_Start
                        , softQuotedString_Start
                        ]

                    SingleLineComment ->
                        [ lineComment_End ]

                    MultiLineComment depth ->
                        [ multiComment_Start state.chunkType
                        , multiComment_End depth
                        ]

                    SoftQuotedString ->
                        [ errorOn "\n" Parse.Error.NewlineInsideSoftQuote
                        , errorOn "\"\"\"" Parse.Error.HardQuoteClosesSoftQuote
                        , softQuotedString_End
                        ]

                    HardQuotedString ->
                        [ hardQuotedString_End ]
        in
        matchFirst state tests


addCurrentChunk : Int -> ChunkType -> State -> State
addCurrentChunk skipOffset newChunkType state =
    { state
        | chunkStart = state.position + skipOffset
        , chunkType = newChunkType
        , chunks =
            -- indents are useful even when they're empty
            if state.position == state.chunkStart && state.chunkType /= Indent then
                state.chunks

            else
                { t = state.chunkType
                , start = state.chunkStart
                , end = state.position
                }
                    :: state.chunks
    }


matchFirst : State -> List (State -> Maybe (Result Error State)) -> Result Error State
matchFirst state testFunctions =
    case testFunctions of
        -- No special markers found, so it's the same chunk
        [] ->
            state
                |> consumeChars 1
                |> stringToChunksRec

        head :: tail ->
            case head state of
                Nothing ->
                    matchFirst state tail

                Just result ->
                    result
                        |> Result.andThen stringToChunksRec


consumeChars : Int -> State -> State
consumeChars length state =
    { state | position = state.position + length }


{-| Compare a string with a position in State

TODO This is so tragically slow in Elm.
Should at least convert to Array once rather than once per Char. XD

-}
compare : String -> State -> Bool
compare targetAsString state =
    let
        target =
            targetAsString
                |> String.toList
                |> Array.fromList

        compareRec offset =
            case Array.get offset target of
                Nothing ->
                    -- The two strings match!
                    True

                Just targetChar ->
                    case Array.get (state.position + offset) state.code of
                        Nothing ->
                            False

                        Just codeChar ->
                            if targetChar == codeChar then
                                compareRec (offset + 1)

                            else
                                False
    in
    compareRec 0


errorOn : String -> Parse.Error.Kind -> State -> Maybe (Result Error State)
errorOn target errorKind state =
    if compare target state then
        Just <| Err { kind = errorKind, position = state.position }

    else
        Nothing


notSpace_dontConsume : State -> Maybe (Result Error State)
notSpace_dontConsume state =
    case Array.get state.position state.code of
        Nothing ->
            Debug.todo "does this even happen?"

        Just char ->
            if char == ' ' then
                Nothing

            else
                state
                    |> addCurrentChunk 0 ContentLine
                    -- do not consume!!
                    |> Ok
                    |> Just


compare_discardAndChangeChunkType : String -> ChunkType -> State -> Maybe (Result Error State)
compare_discardAndChangeChunkType target chunkType state =
    if compare target state then
        state
            |> addCurrentChunk (String.length target) chunkType
            |> consumeChars (String.length target)
            |> Ok
            |> Just

    else
        Nothing


compare_consume_thenChangeChunkType : String -> ChunkType -> State -> Maybe (Result Error State)
compare_consume_thenChangeChunkType target chunkType state =
    if compare target state then
        state
            |> consumeChars (String.length target)
            |> addCurrentChunk 0 chunkType
            |> Ok
            |> Just

    else
        Nothing


compare_changeChunkType_thenConsume : String -> ChunkType -> State -> Maybe (Result Error State)
compare_changeChunkType_thenConsume target chunkType state =
    if compare target state then
        state
            |> addCurrentChunk 0 chunkType
            |> consumeChars (String.length target)
            |> Ok
            |> Just

    else
        Nothing
