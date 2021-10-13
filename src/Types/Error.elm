module Types.Error exposing (..)

-- TODO move this in Compiler/

import Dict exposing (Dict)
import MetaFile exposing (MetaFile)
import Set exposing (Set)
import Types.CanonicalAst as CA exposing (Pos)
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
    { pos : CA.Pos
    , description : ErrorEnv -> List String
    }


type Highlight
    = HighlightWord { line : Int, colStart : Int, colEnd : Int }
    | HighlightBlock { lineStart : Int, lineEnd : Int }



----
--- Builders
--


errorTodo : String -> Res a
errorTodo s =
    res
        { pos = CA.E
        , description = \_ -> [ s ]
        }


err : ErrorArgs -> Error
err =
    Simple


markdown : Pos -> (ErrorEnv -> List String) -> Error
markdown pos envToMarkdownRows =
    Simple
        { pos = pos
        , description = envToMarkdownRows
        }


{-| TODO move to FA as FA.res
-}
faSimple : String -> FA.Pos -> String -> Res a
faSimple moduleName ( start, end ) message =
    res
        { pos = CA.P moduleName start end
        , description = \_ -> [ message ]
        }


{-| TODO move to CA as CA.res
-}
caSimple : CA.Pos -> String -> Res a
caSimple pos message =
    res
        { pos = pos
        , description = \_ -> [ message ]
        }


res : ErrorArgs -> Res a
res =
    Simple >> Err


inlineCode : String -> String
inlineCode s =
    "`" ++ s ++ "`"


showCodeBlock : String -> { line : Int, col : Int } -> { line : Int, col : Int } -> String
showCodeBlock code start end =
    if end.line < 0 then
        ""

    else
        let
            highlight =
                if start.line /= end.line then
                    HighlightBlock { lineStart = start.line, lineEnd = end.line }

                else
                    HighlightWord { line = start.line, colStart = start.col, colEnd = end.col }

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
            |> fmtBlock (startLine + 1) [ highlight ]



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
        { location, block } =
            posToHuman eEnv eArgs.pos

        description =
            eEnv
                |> eArgs.description
                |> (::) block
                |> List.concatMap (String.split "\n")
                |> List.map ((++) "  ")
    in
    String.padRight 50 '-' (location ++ " ") :: "" :: description |> String.join "\n"


posToHuman : ErrorEnv -> Pos -> { location : String, block : String }
posToHuman eEnv pos =
    let
        noBlock loc =
            { location = loc
            , block = ""
            }
    in
    case pos of
        CA.P moduleName startAsInt endAsInt ->
            case Dict.get moduleName eEnv.moduleByName of
                Just mod ->
                    let
                        start =
                            positionToLineAndColumn mod.content startAsInt

                        end =
                            positionToLineAndColumn mod.content endAsInt
                    in
                    { location = mod.fsPath ++ " " ++ String.fromInt start.line ++ ":" ++ String.fromInt start.col
                    , block = showCodeBlock mod.content start end
                    }

                Nothing ->
                    noBlock <| "<The module name is `" ++ moduleName ++ "` but I can't find it. This is a compiler bug.>"

        CA.N ->
            noBlock "<native code>"

        CA.S ->
            noBlock "<the location information has been stripped>"

        CA.T ->
            noBlock "<defined in test modules>"

        CA.I n ->
            noBlock <| "<inferred " ++ String.fromInt n ++ ">"

        CA.E ->
            noBlock "<errorTodo, get rid of me!>"

        CA.F ->
            noBlock "<FormattableToCanonicalAst todo, get rid of me!>"

        CA.G ->
            noBlock "<global value defined in the meta.json>"

        CA.U ->
            noBlock "<union type, get rid of me!>"


highlightSplit : Highlight -> ( Dict Int ( Int, Int ), Set Int ) -> ( Dict Int ( Int, Int ), Set Int )
highlightSplit h ( words, lines ) =
    case h of
        HighlightWord { line, colStart, colEnd } ->
            ( Dict.insert line ( colStart, colEnd ) words
            , lines
            )

        HighlightBlock { lineStart, lineEnd } ->
            ( words
            , List.range lineStart lineEnd
                |> List.foldl Set.insert lines
            )


fmtBlock : Int -> List Highlight -> List String -> String
fmtBlock start highlights ls =
    let
        ( highlightedWords, highlightedLines ) =
            List.foldl highlightSplit ( Dict.empty, Set.empty ) highlights

        pad =
            (start + List.length ls)
                |> String.fromInt
                |> String.length

        wordHighlight lineNumber =
            case Dict.get lineNumber highlightedWords of
                Nothing ->
                    ""

                Just ( s, e ) ->
                    "\n"
                        ++ String.repeat pad " "
                        ++ "   "
                        ++ String.repeat (s - 1) " "
                        ++ String.repeat (max 1 <| e - s) "^"

        lineDem lineIndex =
            if Set.member lineIndex highlightedLines then
                " > "

            else
                " | "

        fmtLine i line =
            let
                index =
                    i + start
            in
            (index
                |> String.fromInt
                |> String.padLeft pad ' '
            )
                ++ lineDem index
                ++ line
                ++ wordHighlight index
    in
    ls
        |> List.indexedMap fmtLine
        |> String.join "\n"
        |> (\s -> s ++ "\n")
