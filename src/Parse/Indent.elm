module Parse.Indent exposing (..)

import Array exposing (Array)
import Parse.Chunks exposing (Chunk, ChunkType(..))
import Parse.Error exposing (Error)


type Indented chunk
    = NormalChunk chunk
    | NewLine
    | BlockStart
    | BlockEnd


type alias IndentedChunk =
    Indented Chunk


type alias State =
    { chunksToRead : List Chunk
    , chunksWithIndent : List IndentedChunk
    , indentStack : List Int
    }


indentChunks : List Chunk -> Result Error (List IndentedChunk)
indentChunks chunks =
    { chunksToRead = chunks
    , chunksWithIndent = []
    , indentStack = []
    }
        |> process
        |> Result.map (.chunksWithIndent >> List.reverse)


process : State -> Result Error State
process state =
    case state.chunksToRead of
        [] ->
            Ok state

        chunk :: tail ->
            case chunk.t of
                Indent ->
                    if firstChunkIsIndent tail then
                        -- discard empty lines
                        process { state | chunksToRead = tail }

                    else
                        let
                            currentIndent =
                                state.indentStack
                                    |> List.head
                                    |> Maybe.withDefault 0

                            indentLength =
                                chunk.end - chunk.start

                            chunksWithIndent =
                                NewLine :: state.chunksWithIndent
                        in
                        if indentLength > currentIndent then
                            process
                                { chunksToRead = tail

                                --, chunksWithIndent = NormalChunk chunk :: BlockStart :: chunksWithIndent
                                , chunksWithIndent = BlockStart :: chunksWithIndent
                                , indentStack = indentLength :: state.indentStack
                                }

                        else if indentLength == currentIndent then
                            process
                                { chunksToRead = tail

                                --, chunksWithIndent = NormalChunk chunk :: chunksWithIndent
                                , chunksWithIndent = chunksWithIndent
                                , indentStack = state.indentStack
                                }

                        else
                            case findInStack indentLength state.indentStack chunksWithIndent of
                                Nothing ->
                                    Err
                                        { kind = Parse.Error.BadIndent { length = indentLength, stack = state.indentStack }
                                        , position = chunk.end
                                        }

                                Just ( reducedStack, chunksAndBlockEnds ) ->
                                    process
                                        { chunksToRead = tail

                                        --, chunksWithIndent = NormalChunk chunk :: chunksAndBlockEnds
                                        , chunksWithIndent = chunksAndBlockEnds
                                        , indentStack = reducedStack
                                        }

                _ ->
                    process
                        { state
                            | chunksToRead = tail
                            , chunksWithIndent = NormalChunk chunk :: state.chunksWithIndent
                        }


findInStack : Int -> List Int -> List IndentedChunk -> Maybe ( List Int, List IndentedChunk )
findInStack indentLength stack ccs =
    case stack of
        currentIndent :: rest ->
            let
                parentIndent =
                    List.head rest |> Maybe.withDefault 0
            in
            -- we know already that `indentLength < currentIndent`
            if indentLength > parentIndent then
                {- This is an error:
                   ```
                   parentIndent
                       currentIndent
                     indentLength
                   ```
                -}
                Nothing

            else if indentLength == parentIndent then
                {-
                   ```
                   parentIndent
                     currentIndent
                   indentLength
                   ```
                -}
                Just ( rest, BlockEnd :: ccs )

            else
                findInStack indentLength rest (BlockEnd :: ccs)

        _ ->
            Just ( [], BlockEnd :: ccs )


firstChunkIsIndent : List Chunk -> Bool
firstChunkIsIndent cs =
    case cs of
        [] ->
            False

        head :: tail ->
            head.t == Indent
