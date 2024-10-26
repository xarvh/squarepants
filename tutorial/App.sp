numberQuickExample as Text =
    """
    main =
        1 + 3 * 2
    """


textQuickExample as Text =
    """
    main =
        "blah" .. "meh"
    """


htmlQuickExample as Text =
    """
    program =
       Html.div
          [ Html.class "col" ]
          [
          , Html.button
               [ Html.onClick "Event:Apple" ]
               [ Html.text "Apple" ]

          , Html.button
               [ Html.onClick "Pear click!" ]
               [ Html.text "Pear" ]
          ]

    """


canvasQuickExample as Text =
    """
    alias N = Number

    main as fn N, N: { r as N, g as N, b as N } =
        fn x, y:

        {
        , r = if x < 50% then 10% else 60%
        , g = 50%
        , b = y
        }
    """


var Msg =
    , 'OnInput Text
    , 'OnMouseMove Number Number
    , 'OnMouseLeave
    , 'OnEmbeddedInput Text
    , 'OnScroll Number Number


Model =
    {
    , code as Text
    , compiledCode as CompileText.CompiledCode
    , embeddedInputs as [ Text ]
    # TODO don't put Html in the model
    , maybeError as Maybe (Html Msg)
    , maybePosition as Maybe { x as Number, y as Number }
    }


init as fn @Array VirtualDom.Effect: Model =
    fn @effects:
    code =
        htmlQuickExample

    maybeError & compiledCode =
        try CompileText.main code as
            'ok c: 'nothing & c
            'err e: 'just e & CompileText.'CompiledText ""

    {
    , code
    , compiledCode
    , embeddedInputs = []
    , maybeError
    , maybePosition = 'nothing
    }
    >> maybeUpdateCanvas @effects __


update as fn @Array VirtualDom.Effect, Msg, Model: Model =
    fn @effects, msg, model:
    try msg as

        'OnEmbeddedInput text:
            { model with embeddedInputs = List.take 10 [ text, .embeddedInputs... ] }

        'OnScroll top left:
            syncScroll @effects top left

            model

        'OnInput code:
            try CompileText.main code as

                'err error:
                    { model with
                    , code
                    , maybeError = 'just error
                    }

                'ok compiledCode:
                    { model with
                    , code
                    , compiledCode
                    , maybeError = 'nothing
                    }
                    >> maybeUpdateCanvas @effects __

        'OnMouseMove x y:
            { model with maybePosition = 'just { x, y } }

        'OnMouseLeave:
            { model with maybePosition = 'nothing }


syncScroll as fn @Array VirtualDom.Effect, Number, Number: None =
    fn @effects, top, left:
    Array.push @effects (VirtualDom.setViewportOf "highlight" top left)


maybeUpdateCanvas as fn @Array VirtualDom.Effect, Model: Model =
    fn @effects, model:
    meh =
        try model.compiledCode as
            CompileText.'CompiledShader shaderFn: Array.push @effects (VirtualDom.drawCanvas "output" shaderFn)
            _: 'none

    model


onMouseMove as VirtualDom.EventHandler Msg =
    fn event:
    VirtualDom.eventToFloat [ "target", "clientWidth" ] event
    >> onOk fn clientWidth:
    VirtualDom.eventToFloat [ "target", "clientHeight" ] event
    >> onOk fn clientHeight:
    VirtualDom.eventToFloat [ "offsetX" ] event
    >> onOk fn offsetX:
    VirtualDom.eventToFloat [ "offsetY" ] event
    >> onOk fn offsetY:
    # -1 because we want coordinates to be from 0 to 1 inclusive
    w =
        clientWidth - 1

    h =
        clientHeight - 1

    x =
        offsetX / w

    y =
        1 - offsetY / h

    'ok << 'OnMouseMove x y


#
# View
#

floatToPercent as fn Number: Text =
    fn n:
    Text.fromNumber (round (n * 100)) .. "%"


tokenToClass as fn Token.Kind: Text =
    try __ as

        # Structure
        Token.'comment _:
            "comment"

        # Structure
        Token.'newSiblingLine:
            ""

        Token.'blockStart:
            ""

        Token.'blockEnd:
            ""

        Token.'badIndent:
            ""

        # Terms
        Token.'textLiteral _ _:
            "literal"

        Token.'numberLiteral _ _:
            "literal"

        Token.'lowercase _: "lower"
        Token.'constructor _: "uppercase"
        Token.'uppercase _: "upper"
        Token.'recordShorthand _: "lower"

        Token.'argumentPlaceholder:
            "keyword"

        Token.'uniquenessPolymorphismBinop:
            "keyword"

        # Keywords
        Token.'fn:
            "keyword"

        Token.'if _:
            "keyword"

        Token.'then:
            "keyword"

        Token.'else _:
            "keyword"

        Token.'try:
            "keyword"

        Token.'as:
            "keyword"

        Token.'with:
            "keyword"

        # Separators
        Token.'comma:
            "paren"

        Token.'colon:
            "paren"

        Token.'threeDots:
            "paren"

        # Ops
        Token.'defop:
            "op"

        Token.'unop _:
            "op"

        Token.'binop _ _:
            "op"

        # Parens
        Token.'roundParen _:
            "paren"

        Token.'squareBracket _ _:
            "paren"

        Token.'curlyBrace _ _:
            "paren"

        Token.'this_is_sp_native:
            "keyword"


viewColorToken as fn Int & [ Html msg ], Text, Token: Int & [ Html msg ] =
    fn start & accum, code, Token.'token tStart tEnd kind:
    slice =
        Text.slice start tEnd code

    acc =
        if Text.startsWith " " slice then
            [
            , Html.span [ Html.class (tokenToClass kind) ] [ Html.text << Text.dropLeft 1 slice ]
            , Html.span [] [ Html.text " " ]
            , accum...
            ]
        else
            [
            , Html.span [ Html.class (tokenToClass kind) ] [ Html.text slice ]
            , accum...
            ]

    tEnd & acc


onScrollEvent as VirtualDom.EventHandler Msg =
    fn event:
    VirtualDom.eventToFloat [ "target", "scrollTop" ] event
    >> onOk fn top:
    VirtualDom.eventToFloat [ "target", "scrollLeft" ] event
    >> onOk fn left:
    'OnScroll top left >> 'ok


viewEditor as fn Model: Html Msg =
    fn model:
    code =
        model.code

    highlightedContent as [ Html Msg ] =
        try Compiler/Lexer.lexer 'true { content = code, fsPath = "" } as

            'err _:
                [ Html.text code ]

            'ok tokens:
                tokens
                >> List.concat
                >> List.for (0 & []) __ (viewColorToken __ code __)
                >> Tuple.second
                >> List.reverse

    # https://css-tricks.com/creating-an-editable-textarea-that-supports-syntax-highlighted-code/
    Html.div
        [
        , Html.style "position" "relative"
        , Html.style "margin-bottom" "2em"
        , Html.class "flex1 h100"
        ]
        [
        , Html.textarea
            [
            , Html.class "editing border"
            , Html.onInput 'OnInput
            , Html.on "scroll" onScrollEvent
            # TODO onkeydown tab
            , Html.spellcheck 'false
            , Html.value code
            ]
            code
        , Html.pre
            [
            , Html.id "highlight"
            , Html.class "highlighting border"
            , Html.ariaHidden 'true
            ]
            [
            , Html.code [ Html.class "highlighting-content" ] highlightedContent
            ]
        , Html.div
            [
            , Html.style "position" "absolute"
            , Html.style "top" "calc(50% - 12px)"
            , Html.style "left" "calc(100% - 15px)"
            , Html.style "width" "0"
            , Html.style "height" "0"
            , Html.style "border" "25px solid transparent"
            , Html.style "border-left" "25px solid lightgrey"
            , Html.class "align-center justify-center"
            ]
            (if model.maybeError == 'nothing then
                 []
             else
                 [
                 , Html.a
                     [
                     , Html.class "pulse"
                     , Html.style "color" "red"
                     , Html.style "font-size" "130px"
                     , Html.style "position" "relative"
                     , Html.style "left" "-15px"
                     , Html.style "top" "-5px"
                     , Html.style "z-index" "2"
                     , Html.style "user-select" "none"
                     , Html.style "text-decoration" "none"
                     , Html.href "#error"
                     ]
                     [ Html.text "×" ]
                 ]
            )
        ]


viewCompiledHtml as fn Model, Html Text: [ Html Msg ] =
    fn model, html:
    [
    , Html.div [ Html.class "h100 border" ] [ Html.map 'OnEmbeddedInput html ]
    , Html.div
        [ Html.class "h100 ml col" ]
        [
        , Html.div [ Html.class "mb" ] [ Html.text "Last messages:" ]
        , if model.embeddedInputs == [] then
            Html.div
                [
                , Html.style "margin-bottom" "4px"
                , Html.style "color" "gray"
                ]
                [ Html.text "(none yet)" ]
        else
            Html.code [ Html.class "ml" ] (List.map model.embeddedInputs (fn i: Html.div [] [ Html.text i ]))
        ]
    ]


viewCompiledOutput as fn Model: Html Msg =
    fn model:
    Html.div
        [ Html.class "flex1 justify-center align-center h100 mt" ]
        (try model.compiledCode as

             CompileText.'CompiledHtml html:
                 viewCompiledHtml model html

             CompileText.'CompiledNumber n:
                 [
                 , Html.div [ Html.style "font-size" "400%" ] [ Html.text (Text.fromNumber n) ]
                 ]

             CompileText.'CompiledText t:
                 [
                 , Html.div [ Html.style "font-size" "200%" ] [ Html.text t ]
                 ]

             CompileText.'CompiledShader shaderFn:
                 [
                 , Html.div
                     [ Html.class "row" ]
                     [
                     , Html.canvas
                         [
                         , Html.class "flex1"
                         , Html.id "output"
                         , Html.height "300"
                         , Html.on "mousemove" onMouseMove
                         , Html.on "mouseleave" (fn e: 'ok 'OnMouseLeave)
                         ]
                     , try model.maybePosition as

                         'nothing:
                             # TODO replace with this with a layout that doesn't jump around... -_-
                             div [ Html.class "ml", Html.style "visibility" "hidden" ] [ Html.text "Program output:" ]

                         'just { x, y }:
                             out =
                                 shaderFn x y

                             Html.div
                                 [ Html.class "ml" ]
                                 [
                                 , div [] [ Html.text "Program input:" ]
                                 , div [] [ Html.text << "x = " .. floatToPercent x ]
                                 , div [] [ Html.text << "y = " .. floatToPercent y ]
                                 , div [ Html.class "mt" ] [ Html.text "Program output:" ]
                                 , div [] [ Html.text << "r = " .. floatToPercent out.r ]
                                 , div [] [ Html.text << "g = " .. floatToPercent out.g ]
                                 , div [] [ Html.text << "b = " .. floatToPercent out.b ]
                                 ]
                     ]
                 ]
        )


view as fn Model: Html Msg =
    fn model:
    div
        [ Html.class "col page-width" ]
        [
        #
        # Page "header"
        #
        , Html.div
            [ Html.class "mb" ]
            [
            # header
            , Html.div
                [ Html.class "row align-center" ]
                [
                , Html.img
                    [
                    , Html.src "https://xarvh.github.io/squarepants/logo/logo.svg"
                    , Html.alt "A stylized black Greek letter lambda, with square red pants, jumping happily"
                    , Html.style "width" "50px"
                    ]
                , Html.h1
                    []
                    [
                    , Html.text "Squarepants "
                    , Html.span [ Html.class "lineThrough" ] [ Html.text "tutorial" ]
                    , Html.text " demo"
                    ]
                ]
            ]
        #
        # Page "payload"
        #
        , Html.div
            [ Html.class "mb" ]
            [
            , Html.a [ Html.href "https://github.com/xarvh/squarepants#readme" ] [ Html.text "Squarepants" ]
            , Html.text " is a small, scalable functional language for interactive apps, made for everyone."
            ]
        , Html.div
            [ Html.class "mb" ]
            [
            , Html.text "This page is entirely "
            , Html.a [ Html.href "https://github.com/xarvh/squarepants/blob/main/tutorial/App.sp" ] [ Html.text "written" ]
            , Html.text " in Squarepants without any ad-hoc JavaScript."
            ]
        , Html.div
            [ Html.class "mb" ]
            [
            , Html.text "(Also, both Squarepants and this page are under heavy development and are far from completed. Expect bugs.)"
            ]
        , Html.div
            [ Html.class "mt mb" ]
            [
            , Html.text "Try some quick examples: "
            , Html.button [ Html.class "ml", Html.onClick << 'OnInput numberQuickExample ] [ Html.text "Numbers" ]
            , Html.button [ Html.class "ml", Html.onClick << 'OnInput textQuickExample ] [ Html.text "Text" ]
            , Html.button [ Html.class "ml", Html.onClick << 'OnInput htmlQuickExample ] [ Html.text "Html" ]
            , Html.button [ Html.class "ml", Html.onClick << 'OnInput canvasQuickExample ] [ Html.text "Canvas" ]
            ]
        # Main
        , Html.div
            [
            , Html.class "align-center"
            , Html.style "min-width" "900px"
            , Html.style "height" "50vh"
            ]
            [
            , viewEditor model
            , viewCompiledOutput model
            ]
        # Error
        , try model.maybeError as

            'nothing:
                Html.none

            'just error:
                Html.div
                    [ Html.id "error" ]
                    [
                    , Html.div [ Html.class "mt red pulse" ] [ Html.text "Error!" ]
                    , Html.pre [ Html.style "margin-top" "0" ] [ error ]
                    ]
        ]


[#
        , div [ Html.class "column" ]
            [
            , try model.maybePosition as
                , 'nothing:
                    div []
                        [
                        , div [] [ Html.text "This program is executed for every pixel of the image at the right." ]
                        , div [] [ Html.text "It takes the x and y coordinates of the pixel, and transforms it into a color." ]
                        ]

                , 'just { x, y }:
                    div []
                        [
                        , div [] [ Html.text << "x = " .. floatToPercent x ]
                        , div [] [ Html.text << "y = " .. floatToPercent y ]
                        ]

            , div [ Html.class "content" ]
                [
                , div
                    [
                    , Html.class "output-wrapper"
                    ]
                    [
                    ]
                ]
            , Html.pre [ Html.class "error" ]
                [
                , try model.maybeError as
                    , 'just e:
                        Html.code [] [ Html.text  e ]

                    , 'nothing:
                        Html.none
                ]
            ]
#]

main as VirtualDom.App Msg Model =
    { init, update, view }
