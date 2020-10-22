module Main exposing (..)

import Browser
import Html exposing (..)
import StringToTokens


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


update msg model =
    model


view model =
    case StringToTokens.stringToChunks model of
        Err err ->
            err
                |> Debug.toString
                |> Html.text

        Ok chunks ->
            chunks
                |> List.map
                    (\c ->
                        Html.li []
                            --[ Html.div [] [ Html.text <| Debug.toString <| c.t ]
                            [ Html.div [] [ Html.text <| String.slice c.start c.end model ]
                            ]
                    )
                |> Html.ul []


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
