module Error exposing (..)


type alias Error =
    { kind : Kind
    , position : Int
    }


type Kind
    = BadIndent { length : Int, stack : List Int }
    | Tab
    | NewlineInsideSoftQuote
    | HardQuoteClosesSoftQuote


toString : String -> Error -> String
toString code error =
    let
        ( line, col ) =
            positionToLineAndColumn code error.position

        l =
            String.length code

        a =
            error.position - 10 |> clamp 0 l

        b =
            error.position + 10 |> clamp 0 l

        slice =
            String.slice a b code
    in
    String.fromInt line ++ "," ++ String.fromInt col ++ ": ```\n" ++ slice ++ "\n```\n" ++ Debug.toString error.kind


positionToLineAndColumn : String -> Int -> ( Int, Int )
positionToLineAndColumn s index =
    let
        before =
            String.slice 0 index s

        newLineIndices =
            String.indices "\n" before

        lineNumber =
            List.length newLineIndices + 1

        lastNewLineIndex =
            newLineIndices
                |> List.reverse
                |> List.head
                |> Maybe.withDefault 0

        colNumber =
            index - lastNewLineIndex
    in
    ( lineNumber, colNumber )
