module Main exposing (..)

import Array exposing (Array)
import Browser
import Chunks exposing (Chunk)
import Html exposing (..)
import Html.Attributes exposing (class, style)


init =
    """
rectFragmentShader : Attributes -> Uniforms -> Varying -> Maybe Color
rectFragmentShader attributes uniforms varying =

    -- TODO: transform into pixelSize, make it a uniform

    pixelsPerTile =
        30.0

    e =
        0.5 / pixelsPerTile

    {-
     -     0               1                            1                     0
     -     |------|--------|----------------------------|----------|----------|
     -  -edge-e  -edge  -edge+e                      edge-e      edge      edge+e
     -}
    mirrorStep : Float -> Float -> Float
    mirrorStep edge p =
        (smoothstep (-edge - e) (-edge + e), p) - (smoothstep (edge - e) (edge + e) p)

    strokeSize =
        uniforms.dimensions / 2.0 + uniforms.strokeWidth

    fillSize =
        uniforms.dimensions / 2.0 - uniforms.strokeWidth

    alpha =
        (mirrorStep strokeSize.x localPosition.x) * (mirrorStep strokeSize.y localPosition.y)

    strokeVsFill =
        (mirrorStep fillSize.x localPosition.x) * (mirrorStep fillSize.y localPosition.y)

    color =
        mix stroke fill strokeVsFill

    return Just <| opacity * alpha * (vec4 color 1.0)
    """


type alias Model =
    String


update msg model =
    model


view model =
    viewChunks model



{-
   viewLines : Model -> Html msg
   viewLines model =
       let
           code =
               model
                   |> String.toList
                   |> Array.fromList

       in
                 code
                   |> Chunks.fromString
                   |> Result.map (Chunks.toLines code)
                   |> List.map (\c -> Html.li [] [ Html.code [] [ Html.text <| String.slice c.start c.end model ] ])
-}


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


charIndexToLineCol : String -> Int -> ( Int, Int )
charIndexToLineCol s index =
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


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
