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
        Error.'formattedText_Default t: Html.text t
        Error.'formattedText_Emphasys t: yellow t
        Error.'formattedText_Warning t: red t
        Error.'formattedText_Decoration t: blue t


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
defaultImports as Imports =

    name =
        "<internal imports.sp>"

    importsPath =
        Meta.'importsPath Meta.'user name

    try ImportsFile.toImports { importsPath, joinPath = Text.join "/" __ } Platforms/Browser.platform.defaultImportsFile as
        'ok imports:
            imports

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


exposedNames as [Self.Self] = []



exports as Meta.Exports =
      todo "generate from exposedNames"


#exposedValues as [ USR & Self.Self ] =
#    l1 =
#        [
#        , Self.introspect Html.div
#        , Self.introspect Html.button
#        , Self.introspect Html.onClick
#        , Self.introspect Html.text
#        , Self.introspect Html.style
#        , Self.introspect Html.class
#        ]
#        >> List.map selfToExposed __
#
#    l2 =
#        [ 'USR ('UMR Meta.'Core "List") "blah" & Self.introspect (fn x: 0.1) ]
#
#    [ l1 ] >> List.concat










textToCaModule as fn Imports, UMR, Text, Text: Res CA.Module =
    fn imports, umr, fsPath, content:

    errorModule as Error.Module =
        {
        , content
        , fsPath
        }

    resolvePars as Meta.ResolvePars Error =
        {
        , currentImports = imports
        , currentModule = umr
        , loadExports = fn importsPath: 'ok exports #'err << Error.'raw [ "Cannot access libraries: ", Debug.toHuman importsPath ]
        , makeError = Error.'raw __
        }

    ro as Compiler/MakeCanonical.ReadOnly =
        {
        , errorModule
        , resolvePars = fn pos: resolvePars
        , umr
        }

    Compiler/MakeCanonical.textToCanonicalModule 'false ro



loadCaModule as fn Dict UMR CA.Module: fn UMR: Res CA.Module =
    fn modulesByUmr:
    fn umr:

    try Dict.get umr modulesByUmr as
        'just m:
            'ok m
        'nothing:
            [ "Cannot find module: " .. Debug.toHuman umr ]
            >> Error.'raw
            >> 'err



viewErrorWrongType as fn TA.RawType: Html msg =
    fn type:
    Html.div
        []
        [
        , Html.text "I don't know how to use this type:"
        , type
        >> Human/Type.doRawType defaultImports __
        >> Human/Format.formatExpression { isRoot = 'true, originalContent = "" } __
        >> Fmt.render
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

    entryUmr =
        'UMR (Meta.'importsPath Meta.'user "") "" inputFileName

    entryUsr =
        'USR entryUmr "program"

    textToCaModule defaultImports entryUmr inputFileName code
    >> onResSuccess fn caModule:

    modulesByUmr =
        Self.toCaModules exposedNames
        >> Dict.insert entryUmr caModule __

    {
    , loadCaModule = loadCaModule modulesByUmr
    , projectImports = defaultImports
    , requiredUsrs = [ entryUsr ]
    }
    >> Compiler/LazyBuild.build
    >> onResSuccess fn { constructors, rootValues }:

    lp as Self.LoadPars =
        {
        , constructors
        , defs = rootValues
        , entryUsr
        , type = todo "loadPars.type"
        }

    loadResult as Result TA.RawType CompiledCode =
        Self.load lp 'CompiledNumber
        >> Result.onErr fn _:
        Self.load lp 'CompiledText
        >> Result.onErr fn _:
        Self.load lp 'CompiledShader
        >> Result.onErr fn _:
        Self.load lp 'CompiledHtml

    loadResult
    >> Result.onErr fn actualCompiledType:
    actualCompiledType
    >> viewErrorWrongType
    >> 'err
