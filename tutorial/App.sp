
initialContent as Text =
    """
alias N = Number

x = List.blah (fn x: x + 2) []

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


init as fn @Array VirtualDom.Effect: Model =
    fn @effects:

    maybeError & compiledCode =
        try CompileText.main initialContent as
            , Ok c: Nothing & c
            , Err e: Just e & CompileText.CompiledText ""

    {
    , code = initialContent
    , maybePosition = Nothing
    , maybeError
    , compiledCode
    }
    >> maybeUpdateCanvas @effects __


update as fn @Array VirtualDom.Effect, Msg, Model: Model =
    fn @effects, msg, model:

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
                    >> maybeUpdateCanvas @effects __

        , OnMouseMove x y:
            { model with maybePosition = Just { x, y } }

        , OnMouseLeave:
            { model with maybePosition = Nothing }


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


viewEditor as fn Model: Html Msg =
    fn model:

    code =
        model.code

    width =
        code
        >> Text.split "\n" __
        >> List.map Text.length __
        >> List.maximum
        >> Maybe.withDefault 10 __
        >> max 40 __

    widthAttr =
        width * 9
        >> Text.fromNumber
        >> (fn s: s .. "px")
        >> Html.style "width" __

    height =
        code
         >> Text.split "\n" __
         >> List.length
         >> max 15 __

    heightAttr =
        (height + 1) * 18
        >> Text.fromNumber
        >> (fn s: s .. "px")
        >> Html.style "height" __

    editorOverlay as [Html Msg] =
        try Compiler/Lexer.lexer { fsPath = "", content = code } as
            , Err _:
                [ Html.text code ]

            , Ok tokens:
                tokens
                >> List.concat
                >> List.for ( 0 & [] ) __ (viewColorToken code __ __)
                >> Tuple.second
                >> List.reverse

    Html.div
       [
       , Html.class "editor-content"
       , widthAttr
       , heightAttr
       ]
       [ Html.textarea
           [ Html.class "editor-textarea"
           , Html.onInput OnInput
           , Html.spellcheck False
           ]
           code
       , Html.div
           [ Html.class "editor-overlay" ]
           editorOverlay
       ]


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

        , CompileText.CompiledShader shaderFn:
            Html.canvas
                [
                , Html.id "output"
                , Html.width "300"
                , Html.height "300"
                , Html.on "mousemove" onMouseMove
                , Html.on "mouseleave" (fn e: Ok OnMouseLeave)
                ]


view as fn Model: Html Msg =
    fn model:

    div [ Html.class "page" ]
        [
        , div [ Html.class "column" ]
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

            , div [ Html.class "content" ]
                [
                , viewEditor model
                , div
                    [
                    , Html.onClick OnClick
                    , Html.class "output-wrapper"
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

