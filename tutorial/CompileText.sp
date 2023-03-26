#
# Colors
#
yellow as fn Text: Text =
    fn t:
    "<span class=yellow>" .. t .. "</span>"

red as fn Text: Text =
    fn t:
    "<span class=red>" .. t .. "</span>"

blue as fn Text: Text =
    fn t:
    "<span class=blue>" .. t .. "</span>"


formattedToConsoleColoredText as fn Error.FormattedText: Text =
    fn formattedText:
    try formattedText as
        , Error.FormattedText_Default t: t
        , Error.FormattedText_Emphasys t: yellow t
        , Error.FormattedText_Warning t: red t
        , Error.FormattedText_Decoration t: blue t


resToConsoleText as fn Res a: Result Text a =
    fn res:
    try res as
        , Ok a: Ok a
        , Err e:
            e
            >> Error.toFormattedText
            >> List.map formattedToConsoleColoredText __
            >> Text.join "" __
            >> Err


onResSuccess as fn (fn a: Result Text b): fn Res a: Result Text b =
    fn f:
    fn res:
    res
    >> resToConsoleText
    >> onOk f


#
# Meta
#
meta as Meta =
  try ModulesFile.textToModulesFile "modules.sp" Platforms/Browser.platform.defaultModules as
      , Ok m:
          ModulesFile.toMeta m

      , Err err:
          errAsText =
              err
              >> Error.toFormattedText
              >> List.map formattedToConsoleColoredText __
              >> Text.join "" __

          Debug.log errAsText "--"

          Debug.todo "This is a compiler bug, not your fault."

#
#
#
selfToExposed as fn Self.Self: USR & Self.Self =
    fn self:

    try self.expression as
        , EA.Variable (RefGlobal usr):
            usr & self

        , _:
            todo << "can't create an USR for " .. toHuman self.expression


exposedValues as [ USR & Self.Self ] =

    l1 =
        [
        , Self.introspect Html.button
        , Self.introspect Html.onClick
        , Self.introspect Html.text
        ]
        >> List.map selfToExposed __

    l2 =
        [USR (UMR Meta.Core "List") "blah" & Self.introspect (fn x: 0.1)]

    [ l1, l2 ]
    >> List.concat





#
# Compile
#
union CompiledCode =
    , CompiledNumber Number
    , CompiledText Text
    , CompiledShader fn Number, Number: { r as Number, g as Number, b as Number }
    , CompiledHtml (Html Text)


main as fn Text: Result Text CompiledCode =
    fn code:

    inputFileName =
        "user_input"

    entryModule =
        UMR (Meta.SourceDirId inputFileName) inputFileName

    {
    , meta
    , umrToFsPath = fn _: inputFileName
    , exposedValues
    , entryModule
    , modules = [entryModule & code]
    }
    >> Compiler/Compiler.compileModules
    >> onResSuccess fn out:

    loadResult as Result TA.RawType CompiledCode =
        Self.load out CompiledNumber
        >> Result.onErr fn _:
        Self.load out CompiledText
        >> Result.onErr fn _:
        Self.load out CompiledShader
        >> Result.onErr fn _:
        Self.load out CompiledHtml

    loadResult
    >> Result.onErr fn actualCompiledType:
    actualCompiledType
    >> Human/Type.doRawType {} __
    >> Human/Type.display "" __
    >> Err

