
initialContent as Text =
    """
pixelColor =
    fn x, y:

    {
    , r = if x < 50% then 10% else 60%
    , g = 50%
    , b = y
    }
"""


union Msg =
  , OnInput Text
  , OnClick
  , OnMouseMove Number Number
  , OnMouseLeave


alias Model =
    {
    , code as Text
    , maybePosition as Maybe { x as Number, y as Number }
    , maybeError as Maybe Text
    , compiledCode as CompileText.CompiledCode
    }


init as fn None: Model =
    fn _:
    {
    , code = initialContent
    , maybePosition = Nothing
    , maybeError = Nothing
    , compiledCode = CompileText.CompiledText ""
    }



#updateCanvas as fn Text, Text: Result Text None =
#    fn domId, compiledJs:
#
#    # TODO remove this hack once we have a proper FFI
#    VirtualDom.unsafeExecuteJavaScript "updateCanvas" { domId, compiledJs }



#compileAndUpdateCanvas as fn Text: Maybe Text =
#    fn code:
#
#    #try CompileText.main code >> onOk (updateCanvas "output" __) as
#    try  as
#        , Ok compiledCode:
#            Nothing
#
#        , Err error:
#            Just error


update as fn Msg, Model: Model =
    fn msg, model:

    try msg as
        , OnInput code:
            try CompileText.main code as
                , Err error:
                    { model with
                    , code = code
                    , maybeError = Just error
                    }

                , Ok compiledCode:
                    { model with
                    , code = code
                    , maybeError = Nothing
                    , compiledCode
                    }

        , OnMouseMove x y:
            { model with maybePosition = Just { x, y } }

        , OnMouseLeave:
            { model with maybePosition = Nothing }



onMouseMove as VirtualDom.EventHandler Msg =
    fn event:

    VirtualDom.eventToFloat [ "target", "clientWidth" ] event >> onOk fn clientWidth:
    VirtualDom.eventToFloat [ "target", "clientHeight" ] event >> onOk fn clientHeight:
    VirtualDom.eventToFloat [ "offsetX" ] event >> onOk fn offsetX:
    VirtualDom.eventToFloat [ "offsetY" ] event >> onOk fn offsetY:

    # -1 because we want coordinates to be from 0 to 1 inclusive
    w = clientWidth - 1
    h = clientHeight - 1
    x = offsetX / w
    y = 1 - offsetY / h

    Ok << OnMouseMove x y


viewCompiledOutput as fn Model: Html Msg =
    fn model:

    try model.compiledCode as

        , CompileText.CompiledNumber n:
            Html.div
                []
                [ Html.text (Text.fromNumber n) ]

        , CompileText.CompiledText t:
            Html.div
                []
                [ Html.text t ]

#        , CompileText.Shader s:
#                    , Html.canvas
#                        [
#                        , Html.id "output"
#                        , Html.width "300"
#                        , Html.height "300"
#                        , Html.on "mousemove" onMouseMove
#                        , Html.on "mouseleave" (fn e: Ok OnMouseLeave)
#                        ]



floatToPercent as fn Number: Text =
    fn n:
    Text.fromNumber (round (n * 100)) .. "%"


view as fn Model: Html Msg =
    fn model:

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
                , Nothing:
                    div []
                        [
                        , div [] [ Html.text "This program is executed for every pixel of the image at the right." ]
                        , div [] [ Html.text "It takes the x and y coordinates of the pixel, and transforms it into a color." ]
                        ]

                , Just { x, y }:
                    div []
                        [
                        , div [] [ Html.text << "x = " .. floatToPercent x ]
                        , div [] [ Html.text << "y = " .. floatToPercent y ]
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
                    , viewCompiledOutput model
                    ]
                ]
            , Html.pre [ Html.class "error" ]
                [
                , try model.maybeError as
                    , Just e:
                        Html.code [] [ Html.text  e ]

                    , Nothing:
                        Html.none
                ]
            ]
        ]


main as VirtualDom.App Msg Model =
    { init, update, view }

