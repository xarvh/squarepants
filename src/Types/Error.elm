module Types.Error exposing (..)

-- TODO move this in Compiler/


type alias Res a =
    Result Error a


type Error
    = Simple ErrorArgs
    | Nested (List Error)


type alias ErrorArgs =
    { file : String
    , content : List ContentDiv
    }


type ContentDiv
    = Text String
    | CodeBlock String
    | CodeBlockWithLineNumber Int (List String)



----
--- Helpers
--


errorTodo : String -> Res a
errorTodo s =
    makeRes "TODO" [ text s ]


makeError : String -> List ContentDiv -> Error
makeError file content =
    { file = file
    , content = content
    }
        |> Simple


makeRes : String -> List ContentDiv -> Res a
makeRes file content =
    Err <| makeError file content


text : String -> ContentDiv
text =
    Text


codeBlock : String -> ContentDiv
codeBlock =
    CodeBlock


inlineCode : String -> String
inlineCode s =
    "`" ++ s ++ "`"


showLines : String -> Int -> Int -> ContentDiv
showLines code lineSpan pos =
    let
        ( line, _ ) =
            positionToLineAndColumn code pos

        lines =
            String.split "\n" code

        start =
            line - lineSpan - 1 |> clamp 0 (List.length lines - 1)

        size =
            line - start + lineSpan |> max 1
    in
    lines
        |> List.drop start
        |> List.take size
        |> CodeBlockWithLineNumber (start + 1)


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
                |> List.concatMap (contentDivToString >> String.split "\n")
                |> List.map ((++) "  ")
    in
    hr :: file ++ content |> String.join "\n"


contentDivToString : ContentDiv -> String
contentDivToString div =
    case div of
        Text s ->
            s

        CodeBlock s ->
            "\n```\n" ++ s ++ "\n```\n"

        CodeBlockWithLineNumber start ls ->
            let
                pad =
                    (start + List.length ls)
                        |> String.fromInt
                        |> String.length

                fmtLine index line =
                    ((index + start)
                        |> String.fromInt
                        |> String.padLeft pad ' '
                    )
                        ++ " | "
                        ++ line
            in
            ls
                |> List.indexedMap fmtLine
                |> String.join "\n"
                |> (\s -> s ++ "\n")
