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
            >> List.map __ formattedToConsoleColoredText
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
                >> List.map __ toHuman
                >> Text.join "" __

            Debug.log errAsText "--"

            Debug.todo "This is a compiler bug, not your fault."


#
#
#
selfToExposed as fn Self.Self: USR & Self.Self =
    fn self:
    try self.expression as
        EA.'globalVariable usr: usr & self
        _: todo << "can't create an USR for " .. toHuman self.expression


exposedNames as [ Self.Self ] =
    [
    , sp_introspect_value Html.div
    , sp_introspect_value Html.button
    , sp_introspect_value Html.onClick
    , sp_introspect_value Html.text
    , sp_introspect_value Html.style
    , sp_introspect_value Html.class
    , sp_introspect_type VirtualDom.VirtualNode
    , sp_introspect_type VirtualDom.Attr
    , sp_introspect_type_open Html
    , sp_introspect_type_open Html.Attr
    ]


exports as Meta.Exports =
    List.for Dict.empty exposedNames fn self, exp:

        'USR ('UMR importsPath sourceDir modulePath) name = self.usr

        module =
            Dict.get modulePath exp
            >> Maybe.withDefault Dict.empty __

        # TODO isOpen = ?
        Dict.insert modulePath (Dict.insert name { isOpen = 'false, usr = self.usr } module) exp


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
        , loadExports = fn importsPath: 'ok exports
        #'err << Error.'raw [ "Cannot access libraries: ", Debug.toHuman importsPath ]
        , makeError =
            Error.'raw __
        }

    ro as Compiler/MakeCanonical.ReadOnly =
        {
        , errorModule
        , resolvePars = fn pos: resolvePars
        , umr
        }

    Compiler/MakeCanonical.textToCanonicalModule 'false ro


loadCaModule as fn Dict UMR CA.Module: fn USR: Res CA.Module =
    fn modulesByUmr:
    fn usr:

    'USR umr name = usr

    try Dict.get umr modulesByUmr as

        'just m:
            'ok m

        'nothing:
            [
            , "Cannot find module: " .. Debug.toHuman umr
            , "Which is needed because it defines " .. name
            ]
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
    modulePath =
        "UserInput"

    entryUmr =
        'UMR (Meta.'importsPath Meta.'user "") "" modulePath

    entryUsr =
        'USR entryUmr "program"

    textToCaModule defaultImports entryUmr modulePath code
    >> onResSuccess fn caModule:
    modulesByUmr =
        Self.toCaModules exposedNames >> Dict.insert entryUmr caModule __

    {
    , loadCaModule = loadCaModule modulesByUmr
    , projectImports = defaultImports
    , requiredUsrs = [ entryUsr ]
    }
    >> Compiler/LazyBuild.build
    >> onResSuccess fn { constructors, rootValues }:

    entryTUsr =
      EA.translateUsr entryUsr

    try List.find rootValues (fn v: v.usr == entryTUsr) as
        'nothing:
            log "" (List.map rootValues (fn s: s.usr))
            'err (Html.text "internal bug: cannot find entryUsr!?")
        'just t: 'ok t
    >> onOk fn entryValue:

    lp as Self.LoadPars =
        {
        , constructors
        , defs = rootValues
        , entryUsr = entryTUsr
        , type = entryValue.type
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
    >> Result.onErr (__ >> viewErrorWrongType >> 'err)
