module Types.Error exposing (..)

-- TODO move this in Compiler/

import Dict exposing (Dict)
import MetaFile exposing (MetaFile)
import Types.CanonicalAst as CA
import Types.FormattableAst as FA


type alias Res a =
    Result Error a


type Error
    = Simple ErrorArgs
    | Nested (List Error)


type alias ErrorEnv =
    { metaFile : MetaFile
    , moduleByName : Dict String { fsPath : String, content : String }
    }


type alias ErrorArgs =
    { moduleName : String
    , start : Int
    , end : Int
    , description : ErrorEnv -> List ContentDiv
    }


type ContentDiv
    = Text String
    | CodeBlock String
    | CodeBlockWithLineNumber Int (List String)



----
--- Builders
--


errorTodo : String -> Res a
errorTodo s =
    res
        { moduleName = "errorTodo"
        , start = -1
        , end = -1
        , description = \_ -> [ text s ]
        }


err : ErrorArgs -> Error
err =
    Simple


{-| TODO move to FA as FA.res -}
faSimple : String -> FA.Pos -> String -> Res a
faSimple moduleName ( start, end ) message =
    res
        { moduleName = moduleName
        , start = start
        , end = end
        , description = \_ -> [ text message ]
        }


{-| TODO move to CA as CA.res -}
caSimple : CA.Pos -> String -> Res a
caSimple pos message =
    res
        { moduleName = pos.n
        , start = pos.s
        , end = pos.e
        , description = \_ -> [ text message ]
        }


res : ErrorArgs -> Res a
res =
    Simple >> Err


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
        { line } =
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


showCodeBlock : String -> { line : Int, col : Int } -> { line : Int, col : Int } -> ContentDiv
showCodeBlock code start end =
    if start == end then
        CodeBlockWithLineNumber 0 []

    else
        let
            {- TODO
               if start and end are on the same line, highlight the word
               if start and end are on different line, hightlight the block
            -}
            extraLines =
                2

            lines =
                String.split "\n" code

            maxLines =
                List.length lines

            startLine =
                start.line - extraLines - 1 |> clamp 0 (maxLines - 1)

            endLine =
                end.line + extraLines |> clamp 0 (maxLines - 1)

            size =
                endLine - startLine
        in
        lines
            |> List.drop startLine
            |> List.take size
            |> CodeBlockWithLineNumber (startLine + 1)



----
--- View
--


flatten : Error -> List ErrorArgs -> List ErrorArgs
flatten e accum =
    case e of
        Simple ar ->
            ar :: accum

        Nested ls ->
            List.foldl flatten accum ls


positionToLineAndColumn : String -> Int -> { line : Int, col : Int }
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
    { line = lineNumber, col = colNumber }


toString : ErrorEnv -> ErrorArgs -> String
toString eEnv eArgs =
    let
        hr =
            "-- ERROR!!! ------------------------------"

        mod =
            eEnv.moduleByName
                |> Dict.get eArgs.moduleName
                |> Maybe.withDefault
                    { fsPath = eArgs.moduleName ++ " <unknown file>"
                    , content = ""
                    }

        start =
            positionToLineAndColumn mod.content eArgs.start

        end =
            positionToLineAndColumn mod.content eArgs.end

        location =
            mod.fsPath ++ " " ++ String.fromInt start.line ++ ":" ++ String.fromInt start.col

        description =
            eEnv
                |> eArgs.description
                |> (::) (showCodeBlock mod.content start end)
                |> List.concatMap (contentDivToString >> String.split "\n")
                |> List.map ((++) "  ")
    in
    hr :: location :: "" :: description |> String.join "\n"


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
