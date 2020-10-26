module Main exposing (..)

import Array exposing (Array)
import Browser
import Chunks exposing (Chunk)
import Error exposing (Error)
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import ParseIndent exposing (IndentedChunk(..))


init =
    """

readStuff : () #> Result Error String
readStuff () =
  if readFile "stuff.json" is Ok string then string else ""


modifyThisVariable : #Int -> Void
modifyThisVariable v =
  v += 1


someValue : Int
someValue =
  a = 3
  b = 2
  return a + b


someOtherFunction : Int String #> Void
someOtherFunction n s =

  someSideEffect ()

  x = readStuff ()

  return
"""


type alias Model =
    String


update msg model =
    model


view model =
    Html.div
        []
        [ viewIndentedChunks model
        , viewChunks model
        ]



----
--- Chunks
--


viewChunk : String -> Chunk -> Html msg
viewChunk code chunk =
    let
        content =
            String.slice chunk.start chunk.end code

        isIndent =
            chunk.t == Chunks.Indent
    in
    Html.code
        [ style "border" "1px solid lightgray"
        , class <| Debug.toString chunk.t
        , if isIndent then
            style "color" "lightgray"

          else
            class ""
        , Html.Attributes.id <| String.fromInt chunk.start ++ "-" ++ String.fromInt chunk.end
        ]
        [ Html.text
            (if isIndent then
                (chunk.end - chunk.start)
                    |> List.range 0
                    |> List.map String.fromInt
                    |> String.join ""
                    |> (\s -> s ++ " ")

             else
                String.slice chunk.start chunk.end code
            )
        , if chunk.t == Chunks.ContentLine && String.endsWith "\n" content then
            Html.span
                [ style "background-color" "orange" ]
                [ Html.text "\\n" ]

          else
            Html.text ""
        ]


viewChunks : Model -> Html msg
viewChunks model =
    let
        resultSemLines =
            model
                |> String.toList
                |> Array.fromList
                |> Chunks.fromString
                |> Result.map Chunks.toSemanticLines
    in
    case resultSemLines of
        Err err ->
            err
                |> Debug.toString
                |> Html.text

        Ok semLines ->
            semLines
                |> List.map (List.map (viewChunk model) >> Html.li [])
                |> Html.ul []



----
--- Indented Chunks
--


viewIndentedChunks : Model -> Html msg
viewIndentedChunks model =
    let
        resultIndentedChunks =
            model
                |> String.toList
                |> Array.fromList
                |> Chunks.fromString
                |> Result.andThen ParseIndent.indentChunks
    in
    case resultIndentedChunks of
        Err err ->
            Html.pre []
                [ Html.code []
                    [ err
                        |> Error.toString model
                        |> Html.text
                    ]
                ]

        Ok indentedChunks ->
            indentedChunks
                |> List.foldl (addIndentedChunk model) ( 0, [], [] )
                |> (\( depth, chunks, html ) -> html)
                |> List.reverse
                |> Html.ul []




viewDepth : Int -> Html msg
viewDepth depth =
    Html.code
        [ style "color" "#eee"
        , class "depth"
        ]
        [ depth
            |> List.range 0
            |> List.map String.fromInt
            |> String.join ""
            |> Html.text
        ]


addIndentedChunk : String -> IndentedChunk -> ( Int, List Chunk, List (Html msg) ) -> ( Int, List Chunk, List (Html msg) )
addIndentedChunk code ic ( depth, chunks, html ) =
    case ic of
        NormalChunk chunk ->
            ( depth, chunk :: chunks, html )

        NewLine ->
            ( depth
            , []
            , (chunks
                |> List.reverse
                |> List.map (viewChunk code)
                |> (::) (viewDepth depth)
                |> Html.li []
              )
                :: html
            )

        BlockStart ->
            ( depth + 1, chunks, html )

        BlockEnd ->
            ( depth - 1, chunks, html )



----
---
--


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
