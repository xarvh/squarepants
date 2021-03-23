module Types.Error exposing (..)

-- TODO move this in Compiler/


type alias Res a =
    Result Error a


type Error
    = Simple ErrorArgs
    | Nested (List Error)


type alias ErrorArgs =
    { file : String
    , content : Content
    }


type alias Content =
    List ( ContentType, List ( Priority, String ) )


type Priority
    = Default
      -- These are nice-to-have, but not important
    | LowlightHint
    | HighlightHint
      -- This highlight should appear no matter the limits of the display
    | HighlightImportant


type ContentType
    = Text
    | InlineCode
    | CodeBlock
    | CodeBlockWithLineNumber Int



----
--- Helpers
--


errorTodo : String -> Res a
errorTodo s =
    makeRes "TODO" [ text s ]


makeError : String -> Content -> Error
makeError file content =
    { file = file
    , content = content
    }
        |> Simple


makeRes : String -> Content -> Res a
makeRes file content =
    Err <| makeError file content


text : String -> ( ContentType, List ( Priority, String ) )
text s =
    ( Text, [ ( Default, s ) ] )


codeBlock : String -> ( ContentType, List ( Priority, String ) )
codeBlock s =
    ( CodeBlock, [ ( Default, s ) ] )


showLines : String -> Int -> Int -> ( ContentType, List ( Priority, String ) )
showLines code lineSpan pos =
    let
        ( line, col ) =
            positionToLineAndColumn code pos

        start =
            line - lineSpan

        end =
            line + lineSpan

        lines =
            code
                |> String.split "\n"
                |> List.drop start
                |> List.take (lineSpan * 2)
                |> String.join "\n"
    in
    ( CodeBlockWithLineNumber start, [ ( Default, lines ) ] )


flatten : Error -> List ErrorArgs -> List ErrorArgs
flatten e accum =
    case e of
        Simple ar ->
            ar :: accum

        Nested ls ->
            List.foldl flatten accum ls


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



----
--- View
--


toString : ErrorArgs -> String
toString eArgs =
    let
        hr =
            "== ERROR!!! =================================="

        file =
            if eArgs.file /= "" then
                [ "# " ++ eArgs.file
                , ""
                ]

            else
                []

        content =
            eArgs.content
                |> List.concatMap (contentBlockToString >> String.split "\n")
                |> List.map ((++) "  ")
    in
    hr
        :: file
        ++ content
        |> String.join "\n"


contentBlockToString : ( ContentType, List ( Priority, String ) ) -> String
contentBlockToString ( cty, spans ) =
    let
        wrap s =
            case cty of
                Text ->
                    s

                InlineCode ->
                    "`" ++ s ++ "`"

                CodeBlock ->
                    "\n```\n" ++ s ++ "\n```\n"

                CodeBlockWithLineNumber start ->
                    s
                        |> String.split "\n"
                        |> List.indexedMap (\i line -> (String.padLeft 5 ' ' <| String.fromInt (i + start)) ++ " | " ++ line)
                        |> String.join "\n"
    in
    spans
        |> List.map contentSpanToString
        |> String.join ""
        |> wrap


contentSpanToString : ( Priority, String ) -> String
contentSpanToString ( pri, s ) =
    case pri of
        Default ->
            s

        LowlightHint ->
            s

        HighlightHint ->
            s

        HighlightImportant ->
            s
