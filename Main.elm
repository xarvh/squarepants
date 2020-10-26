module Main exposing (..)

import Array exposing (Array)
import Browser
import Chunks exposing (Chunk)
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import ParseIndent exposing (IndentedChunk(..))


init =
    """


someSideEffect : () #> Void
someSideEffect () =
  print "a String"


readStuff : () #> Result Error String
readStuff () =
  try! readFile "stuff.json"


modifyThisVariable : Int! -> Void
modifyThisVariable v =
  v += 1


someOtherFunction : Int String #> Void
someOtherFunction n s =

  someSideEffect ()

  x = readStuff ()

  return
"""



{-


The block:
```
x =
  writeFile "blah.json"
  return 0

x

x
```

Executes the write once, when x is declared.
Subsequent references to x will not execute anything, but maybe someone may expect that it would.
So the compiler should catch these useless references and tell the user that they don't do anything






Too many ambiguous symbols?

mutable type : `#Int`
effect functions : `#>`
glsl-friendly function : `_>`

debug only : `try!!!`, `log!!!`
not : `!` `!=`


~>

->

#>




A `->` function is always compatible with a `#>`, but not vice vice versa.




  -> you CANNOT mutate variables outside of function scope (ie, closures or globals), you need to pass them as args.

-}











rectFragmentShader : Attributes Uniforms Varying -> Maybe Color
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
    viewIndentedChunks model



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



TODO NOW: unify in a single Error
type Error
    = ChunkError Chunks.Error
    | IndentError ParseIndent.Error


viewIndentedChunks : Model -> Html msg
viewIndentedChunks model =
    let
        resultIndentedChunks =
            model
                |> String.toList
                |> Array.fromList
                |> Chunks.fromString
                |> Result.mapError ChunkError
                |> Result.andThen (ParseIndent.indentChunks >> Result.mapError IndentError)
    in
    case resultIndentedChunks of
        Err err ->
            err
                |> Debug.toString
                |> Html.text

        Ok indentedChunks ->
            indentedChunks
                |> List.foldl (viewIndentedChunk model) ( 0, [], [] )
                |> (\( depth, chunks, html ) -> html)
                |> Html.ul []


viewIndentedChunk : String -> IndentedChunk -> ( Int, List Chunk, List (Html msg) ) -> ( Int, List Chunk, List (Html msg) )
viewIndentedChunk code ic ( depth, chunks, html ) =
    case ic of
        NormalChunk chunk ->
            ( depth, chunk :: chunks, html )

        NewLine ->
            ( depth
            , []
            , (chunks
                |> List.reverse
                |> List.map (viewChunk code)
                |> (::)
                    (Html.code
                        [ style "color" "#eee" ]
                        [ depth
                            |> List.range 0
                            |> List.map String.fromInt
                            |> String.join ""
                            |> Html.text
                        ]
                    )
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
----
---
--


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
