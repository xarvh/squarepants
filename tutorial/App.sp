
initialContent as Text =
    """
pixelColor =
    x: y:

    {
    , r = if x < 0.5 then 0.1 else 0.6
    , g = 0.5
    , b = y
    }
"""


union Msg =
  , OnInput Text
  , OnClick
  , OnMouseMove Number Number
  , OnMouseLeave


alias Model = {
    , code as Text
    , maybePosition as Maybe { x as Number, y as Number }
    , maybeError as Maybe Text
    }


init as None: Model = _: {
    , code = initialContent
    , maybePosition = Nothing
    , maybeError = compileAndUpdateCanvas initialContent
    }



updateCanvas as Text: Text: Result Text None =
    domId: compiledJs:

    # TODO remove this hack once we have a proper FFI
    VirtualDom.unsafeExecuteJavaScript "updateCanvas" { domId, compiledJs }



compileAndUpdateCanvas as Text: Maybe Text =
    code:

    try CompileText.main code >> Result.onOk (updateCanvas "output") as
        Ok _:
            Nothing

        Err error:
            Just error


update as Msg: Model: Model =
    msg: model:

    try msg as
        OnInput code:
            # TODO updateCanvas should be handled as a side effect once we have uniqueness types
            { model with
            , code = code
            , maybeError = compileAndUpdateCanvas code
            }

        OnMouseMove x y:
            { model with maybePosition = Just { x, y } }

        OnMouseLeave:
            { model with maybePosition = Nothing }



onMouseMove as VirtualDom.EventHandler Msg =
    event:

    VirtualDom.eventToFloat [ "target", "clientWidth" ] event >> onOk clientWidth:
    VirtualDom.eventToFloat [ "target", "clientHeight" ] event >> onOk clientHeight:
    VirtualDom.eventToFloat [ "offsetX" ] event >> onOk offsetX:
    VirtualDom.eventToFloat [ "offsetY" ] event >> onOk offsetY:

    # -1 because we want coordinates to be from 0 to 1 inclusive
    w = clientWidth - 1
    h = clientHeight - 1
    x = offsetX / w
    y = 1 - offsetY / h

    Ok << OnMouseMove x y


floatToDec as Number: Text =
    n:

    round (n * 100) / 100
    >> Text.fromNumber


view as Model: Html Msg =
    model:

    class =
        Html.class

    div [ class "page" ]
        [
        , div [ class "column" ]
            [
            , div
                  []
                  [
                  , Html.text "This page is "
                  , Html.a [ Html.href "https://github.com/xarvh/squarepants/blob/main/tutorial/App.sp" ] [ Html.text "written" ]
                  , Html.text " in "
                  , Html.a [ Html.href "https://github.com/xarvh/squarepants#readme" ] [ Html.text "Squarepants" ]
                  ]
            , div
                  []
                  [ Html.text "The compiler is also written in Squarepants, so it can be used by the page to compile the program below." ]
            , try model.maybePosition as
                Nothing:
                    div []
                        [
                        , div [] [ Html.text "This program is executed for every pixel of the image at the right." ]
                        , div [] [ Html.text "It takes the x and y coordinates of the pixel, and transforms it into a color." ]
                        ]

                Just { x, y }:
                    div []
                        [
                        , div [] [ Html.text << "x = " .. floatToDec x ]
                        , div [] [ Html.text << "y = " .. floatToDec y ]
                        ]

            , div [ class "content" ]
                [
                , Html.textarea
                    [
                    , Html.class "input"
                    , Html.onInput OnInput
                    ]
                    model.code
                , div
                    [
                    , Html.onClick OnClick
                    , class "output-wrapper"
                    ]
                    [
                    , Html.canvas
                        [
                        , Html.id "output"
                        , Html.width "300"
                        , Html.height "300"
                        , Html.on "mousemove" onMouseMove
                        , Html.on "mouseleave" (e: Ok OnMouseLeave)
                        ]
                    ]
                ]
            , Html.pre [ Html.class "error" ]
                [
                , try model.maybeError as
                    Just e:
                        Html.code [] [ Html.text  e ]

                    Nothing:
                        Html.none
                ]
            ]
        ]


main as VirtualDom.App Msg Model =
    { init, update, view }
