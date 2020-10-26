module ParseIndent exposing (..)

import Array exposing (Array)
import Chunks exposing (Chunk, ChunkType(..))


type IndentedChunk
    = NormalChunk Chunk
    | NewLine
    | BlockStart
    | BlockEnd


type ErrorType
    = ErrorIndent { length : Int, stack : List Int }


type alias Error =
    { t : ErrorType
    , position : Int
    }


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
        |> Result.map .chunksWithIndent


process : State -> Result Error State
process state =
    case state.chunksToRead of
        [] ->
            Ok state

        chunk :: tail ->
            case chunk.t of
                Indent ->
--                     if firstChunkIsIndent tail then
--                         -- discard empty lines
--                         process { state | chunksToRead = tail }
-- 
--                     else
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
                                , chunksWithIndent = NormalChunk chunk :: BlockStart :: chunksWithIndent
                                , indentStack = indentLength :: state.indentStack
                                }

                        else if indentLength == currentIndent then
                            process
                                { chunksToRead = tail
                                , chunksWithIndent = NormalChunk chunk :: chunksWithIndent
                                , indentStack = state.indentStack
                                }

                        else
                            case findInStack indentLength state.indentStack chunksWithIndent of
                                Nothing ->
                                    Err
                                        { t = ErrorIndent { length = indentLength, stack = state.indentStack }
                                        , position = chunk.end
                                        }

                                Just ( reducedStack, chunksAndBlockEnds ) ->
                                    process
                                        { chunksToRead = tail
                                        , chunksWithIndent = NormalChunk chunk :: chunksAndBlockEnds
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
        currentIndent :: parentIndent :: rest ->
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
                Just ( parentIndent :: rest, BlockEnd :: ccs )

            else
                findInStack indentLength (parentIndent :: rest) (BlockEnd :: ccs)

        _ ->
            Nothing


firstChunkIsIndent : List Chunk -> Bool
firstChunkIsIndent cs =
    case cs of
        [] ->
            False

        head :: tail ->
            head.t == Indent
