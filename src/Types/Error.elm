module Types.Error exposing (..)


type Error
    = Simple ErrorArgs
    | Nested (List Error)


type alias ErrorArgs =
    { kind : Kind, pos : Int }


type alias Res a =
    Result Error a


errorTodo : String -> Res a
errorTodo s =
    { kind = Whatever s
    , pos = -1
    }
        |> Simple
        |> Err


error : Int -> Kind -> Res a
error pos kind =
    { kind = kind
    , pos = pos
    }
        |> Simple
        |> Err



----
--- Kinds
--


type Kind
    = BadIndent { length : Int, stack : List Int }
    | InvalidToken String
    | UnknownOperator String
    | UnterminatedMultiLineComment
    | Tab
    | NewLineInsideSoftQuote
    | HardQuoteClosesSoftQuote
    | UnterminatedStringLiteral
      -- TODO remove this one
    | Whatever String


kindToString : Kind -> String
kindToString kind =
    case kind of
        NewLineInsideSoftQuote ->
            "single-quoted strings can't go on multiple lines, use \\n or a triple-quoted string instead"

        Whatever s ->
            s

        _ ->
            Debug.toString kind



----
--- To human
--


toStrings : String -> Error -> List String
toStrings code e =
    flatten e []
        |> List.map (argsToString code)


flatten : Error -> List ErrorArgs -> List ErrorArgs
flatten e accum =
    case e of
        Simple ar ->
            ar :: accum

        Nested ls ->
            List.foldl flatten accum ls


argsToString : String -> ErrorArgs -> String
argsToString code e =
    let
        ( line, col ) =
            positionToLineAndColumn code e.pos

        l =
            String.length code

        {- TODO: search 3 newline prior and 3 newlines after, then output a record:
           { before : String
           , target : String
           , after : String
           , line : Int
           , col : Int
           , message : String
           }

        -}
        a =
            e.pos - 10 |> clamp 0 l

        b =
            e.pos + 10 |> clamp 0 l

        slice =
            String.slice a b code
    in
    String.fromInt line ++ "," ++ String.fromInt col ++ ": ```\n" ++ slice ++ "\n```\n" ++ kindToString e.kind


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
