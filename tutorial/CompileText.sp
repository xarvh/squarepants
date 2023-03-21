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
  try ModulesFile.textToModulesFile "modules.sp" Platforms/RawJavaScript.platform.defaultModules as
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
# Compile
#
union CompiledCode =
    , CompiledNumber Number
    , CompiledText Text


main as fn Text: Result Text CompiledCode =
    fn code:

    inputFileName =
        "user_input"

    entryModule =
        UMR (Meta.SourceDir inputFileName) inputFileName

    {
    , meta
    , umrToFsPath = fn _: inputFileName
    , nativeValues = Prelude.coreNativeValues
    , entryModule
    , modules = [entryModule & code]
    }
    >> Compiler/Compiler.compileModules
    >> onResSuccess fn out:

    Load.dynamicLoad out CompiledNumber
    >> Result.onErr fn _:
    Load.dynamicLoad out CompiledText

