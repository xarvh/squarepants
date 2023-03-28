

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
main =
   Html.button
      [ Html.onClick "BLAH" ]
      [ Html.text "Click me!!" ]
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


union Msg =
  , OnInput Text
  , OnMouseMove Number Number
  , OnMouseLeave
  , OnEmbeddedInput Text
  , OnScroll Number Number


alias Model =
    {
    , code as Text
    , maybePosition as Maybe { x as Number, y as Number }
    , maybeError as Maybe Text
    , compiledCode as CompileText.CompiledCode
    , embeddedInputs as [Text]
    }


init as fn @Array VirtualDom.Effect: Model =
    fn @effects:

    code = htmlQuickExample

    maybeError & compiledCode =
        try CompileText.main code as
            , Ok c: Nothing & c
            , Err e: Just e & CompileText.CompiledText ""

    {
    , code
    , maybePosition = Nothing
    , maybeError
    , compiledCode
    , embeddedInputs = []
    }
    >> maybeUpdateCanvas @effects __


update as fn @Array VirtualDom.Effect, Msg, Model: Model =
    fn @effects, msg, model:

    try msg as
        , OnEmbeddedInput text:
            { model with embeddedInputs = [ text, ... .embeddedInputs ] }

        , OnScroll top left:
            syncScroll @effects top left
            model

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
                    >> maybeUpdateCanvas @effects __

        , OnMouseMove x y:
            { model with maybePosition = Just { x, y } }

        , OnMouseLeave:
            { model with maybePosition = Nothing }


syncScroll as fn @Array VirtualDom.Effect, Number, Number: None =
    fn @effects, top, left:
    Array.push @effects (VirtualDom.setViewportOf "highlight" top left)


maybeUpdateCanvas as fn @Array VirtualDom.Effect, Model: Model =
    fn @effects, model:

    meh =
        try model.compiledCode as
            , CompileText.CompiledShader shaderFn:
                Array.push @effects (VirtualDom.drawCanvas "output" shaderFn)

            , _:
                None

    model


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


#
# View
#


floatToPercent as fn Number: Text =
    fn n:
    Text.fromNumber (round (n * 100)) .. "%"


wordToClass as fn Token.Word: Text =
    fn word:

    try word.name as
        , "alias": "keyword"
        , "union": "keyword"
        , _: if word.isUpper then "upper" else "lower"


tokenToClass as fn Token.Kind: Text =
    fn kind:

    try kind as
        # Structure
        , Token.NewSiblingLine: ""
        , Token.BlockStart: ""
        , Token.BlockEnd: ""
        , Token.BadIndent: ""
        # Terms
        , Token.TextLiteral _: "literal"
        , Token.NumberLiteral _ _: "literal"
        , Token.Word w: wordToClass w
        , Token.ArgumentPlaceholder: "keyword"
        , Token.UniquenessPolymorphismBinop: "keyword"
        # Keywords
        , Token.Fn: "keyword"
        , Token.If: "keyword"
        , Token.Then: "keyword"
        , Token.Else: "keyword"
        , Token.Try: "keyword"
        , Token.As: "keyword"
        , Token.With: "keyword"
        # Separators
        , Token.Comma: "paren"
        , Token.Colon: "paren"
        , Token.ThreeDots: "paren"
        # Ops
        , Token.Defop: "op"
        , Token.Unop _: "op"
        , Token.Binop _: "op"
        # Parens
        , Token.RoundParen _: "paren"
        , Token.SquareBracket _: "paren"
        , Token.CurlyBrace _: "paren"


viewColorToken as fn Text, Token, (Int & [Html msg]): Int & [Html msg] =
    fn code, (Token comment tStart tEnd kind), ( start & accum ):

    slice =
        Text.slice start tEnd code

    acc =
        if Text.startsWith " " slice then
            [
            , Html.span [ Html.class (tokenToClass kind) ] [ Html.text << Text.dropLeft 1 slice ]
            , Html.span [] [ Html.text " " ]
            , ...accum
            ]

        else
            [
            , Html.span [ Html.class (tokenToClass kind) ] [ Html.text slice ]
            , ...accum
            ]

    tEnd & acc


onScrollEvent as VirtualDom.EventHandler Msg =
    fn event:

    VirtualDom.eventToFloat [ "target", "scrollTop" ] event
    >> onOk fn top:
    VirtualDom.eventToFloat [ "target", "scrollLeft" ] event
    >> onOk fn left:

    OnScroll top left
    >> Ok


viewEditor as fn Model: Html Msg =
    fn model:

    code =
        model.code

    highlightedContent as [Html Msg] =
        try Compiler/Lexer.lexer { fsPath = "", content = code } as
            , Err _:
                [ Html.text code ]

            , Ok tokens:
                tokens
                >> List.concat
                >> List.for ( 0 & [] ) __ (viewColorToken code __ __)
                >> Tuple.second
                >> List.reverse

    # https://css-tricks.com/creating-an-editable-textarea-that-supports-syntax-highlighted-code/
    Html.div
        [
#        , Html.style "height" "300px"
        , Html.style "position" "relative"
        , Html.class "flex1 h100"
        ]
        [
        , Html.textarea
            [
            , Html.class "editing border"
            , Html.onInput OnInput
            , Html.on "scroll" onScrollEvent
            # TODO onkeydown tab
            , Html.spellcheck False
            ]
            code
        , Html.pre
            [
            , Html.id "highlight"
            , Html.class "highlighting border"
            , Html.ariaHidden True
            ]
            [
            , Html.code
              [ Html.class "highlighting-content" ]
              highlightedContent
            ]
       ]



viewCompiledHtml as fn Model, Html Text: [Html Msg] =
    fn model, html:

    [
    , Html.div
        [ Html.class "h100 border" ]
        [ Html.map OnEmbeddedInput html ]
    , Html.div
        [ Html.class "ml" ]
        (List.map (fn i: Html.div [] [ Html.text i]) model.embeddedInputs)
    ]



viewCompiledOutput as fn Model: Html Msg =
    fn model:

    Html.div
      [ Html.class "flex1 justify-center align-center h100" ]
      (try model.compiledCode as
        , CompileText.CompiledHtml html:
            viewCompiledHtml model html

        , CompileText.CompiledNumber n:
                [ Html.text (Text.fromNumber n) ]

        , CompileText.CompiledText t:
                [ Html.text t ]

        , CompileText.CompiledShader shaderFn:
            [Html.canvas
                [
                , Html.class "flex1"
                , Html.id "output"
                , Html.height "300"
                , Html.on "mousemove" onMouseMove
                , Html.on "mouseleave" (fn e: Ok OnMouseLeave)
                ]
            ]
      )


view as fn Model: Html Msg =
    fn model:

    div [ Html.class "col page-width" ]
        [
        #
        # Page "header"
        #
        , Html.div [ Html.class "mb" ]
            [
            # header
            , Html.div [ Html.class "row align-center" ]
                [
                , Html.img
                    [
                    , Html.src "https://xarvh.github.io/squarepants/logo/logo.svg"
                    , Html.alt "A stylized black Greek letter lambda, with square red pants, jumping happily"
                    , Html.style "width" "50px"
                    ]
                , Html.h1 []
                    [ Html.text "Squarepants "
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



#        , Html.div
#            []
#            [
            , Html.div
                [ Html.class "mt mb" ]
                [
                , Html.text "Try some quick examples: "
                , Html.button [ Html.class "ml", Html.onClick << OnInput numberQuickExample ] [ Html.text "Numbers" ]
                , Html.button [ Html.class "ml", Html.onClick << OnInput textQuickExample ] [ Html.text "Text" ]
                , Html.button [ Html.class "ml", Html.onClick << OnInput htmlQuickExample ] [ Html.text "Html" ]
                , Html.button [ Html.class "ml", Html.onClick << OnInput canvasQuickExample ] [ Html.text "Canvas" ]
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
#            ]


[#
        , div [ Html.class "column" ]
            [
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
                    , Just e:
                        Html.code [] [ Html.text  e ]

                    , Nothing:
                        Html.none
                ]
            ]
#]
        ]


main as VirtualDom.App Msg Model =
    { init, update, view }

