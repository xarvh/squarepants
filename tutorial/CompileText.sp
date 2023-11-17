#
# Colors
#
yellow as fn Text: Html msg =
    fn t:
    Html.span [ Html.class "yellow" ] [ Html.text t ]


red as fn Text: Html msg =
    fn t:
    Html.span [ Html.class "red" ] [ Html.text t ]


blue as fn Text: Html msg =
    fn t:
    Html.span [ Html.class "blue" ] [ Html.text t ]


formattedToConsoleColoredText as fn Error.FormattedText: Html msg =
    fn formattedText:
    try formattedText as
        Error.'FormattedText_Default t: Html.text t
        Error.'FormattedText_Emphasys t: yellow t
        Error.'FormattedText_Warning t: red t
        Error.'FormattedText_Decoration t: blue t


resToConsoleText as fn Res a: Result (Html msg) a =
    fn res:
    try res as

        'ok a:
            'ok a

        'err e:
            e
            >> Error.toFormattedText
            >> List.map formattedToConsoleColoredText __
            >> Html.div [] __
            >> 'err


onResSuccess as fn fn a: Result (Html msg) b: fn Res a: Result (Html msg) b =
    fn f:
    fn res:
    res
    >> resToConsoleText
    >> onOk f


#
# Meta
#
meta as Imports =
    try ImportsFile.fromText "modules.sp" Platforms/Browser.platform.defaultModules as

        'ok m:
            ImportsFile.toImports m

        'err err:
            errAsText =
                err
                >> Error.toFormattedText
                >> List.map toHuman __
                >> Text.join "" __

            Debug.log errAsText "--"

            Debug.todo "This is a compiler bug, not your fault."


#
#
#
selfToExposed as fn Self.Self: USR & Self.Self =
    fn self:
    try self.expression as
        EA.'variable ('RefGlobal usr): usr & self
        _: todo << "can't create an USR for " .. toHuman self.expression


exposedValues as [ USR & Self.Self ] =
    l1 =
        [
        , Self.introspect Html.div
        , Self.introspect Html.button
        , Self.introspect Html.onClick
        , Self.introspect Html.text
        , Self.introspect Html.style
        , Self.introspect Html.class
        ]
        >> List.map selfToExposed __

#    l2 =
#        [ 'USR ('UMR Meta.'Core "List") "blah" & Self.introspect (fn x: 0.1) ]

    [ l1 ] >> List.concat


viewErrorWrongType as fn TA.RawType: Html msg =
    fn type:
    Html.div
        []
        [
        , Html.text "I don't know how to use this type:"
        , type
        >> Human/Type.doRawType {} __
        >> Human/Type.display "" __
        >> Html.text
        ]


#
# Compile
#
var CompiledCode =
    , 'CompiledNumber Number
    , 'CompiledText Text
    , 'CompiledShader (fn Number, Number: { b as Number, g as Number, r as Number })
    , 'CompiledHtml (Html Text)


main as fn Text: Result (Html msg) CompiledCode =
    fn code:
    inputFileName =
        "user_input"

    entryModule =
        'UMR (Meta.'importsPath Meta.'user "") "" inputFileName

    {
    , entryModule
    , exposedValues
    , meta
    , modules = [ entryModule & code ]
    , umrToFsPath = fn _: inputFileName
    }
    >> Compiler/Compiler.compileModules
    >> onResSuccess fn out:
    loadResult as Result TA.RawType CompiledCode =
        Self.load out 'CompiledNumber
        >> Result.onErr fn _:
        Self.load out 'CompiledText
        >> Result.onErr fn _:
        Self.load out 'CompiledShader
        >> Result.onErr fn _:
        Self.load out 'CompiledHtml

    loadResult
    >> Result.onErr fn actualCompiledType:
    actualCompiledType
    >> viewErrorWrongType
    >> 'err
