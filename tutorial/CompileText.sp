
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
# Module loading
#

loadModule as fn Meta, UMR, Text: Res CA.Module =
    fn meta, umr, content:

    params as Compiler/MakeCanonical.ReadOnly =
        {
        , meta
        , errorModule = { fsPath = "", content }
        , umr
        }

    Compiler/MakeCanonical.textToCanonicalModule False params



#
# Compile
#

main as fn Text: Result Text Text =

    platform =
        Platforms/RawJavaScript.platform

    modulesFileName =
        "modules.sp"

    inputFileName =
        "user_input"

    meta =
      try ModulesFile.textToModulesFile modulesFileName platform.defaultModules as
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

    umr =
        UMR (Meta.SourceDir inputFileName) inputFileName


    entryUsr =
      USR umr "pixelColor"


    fn code:

    loadModule meta umr code
    >> resToConsoleText
    >> onOk fn caModule:

    allCaModules =
        [ caModule, ...Prelude.coreModules ]

    allCaModules
    >> Compiler/TypeCheck.initStateAndGlobalEnv
    >> onResSuccess fn (luv & typeCheckGlobalEnv):

    !lastUnificationVarId =
        cloneImm luv

    allCaModules
    >> List.mapRes (Compiler/TypeCheck.doModule @lastUnificationVarId typeCheckGlobalEnv __) __
    >> onResSuccess fn taModules:

    # ---> check that `pixelColor` has the correct type

    taModules
    >> List.mapRes Compiler/UniquenessCheck.doModule __
    >> onResSuccess fn modulesWithDestruction:

    modulesWithDestruction
    >> Compiler/MakeEmittable.translateAll
    >> Result.mapError (fn e: todo "MakeEmittable.translateAll returned Err") __
    >> onResSuccess fn (meState & emittableStatements):

    !emittableState =
        cloneImm meState

    platform.compile
        {
        , constructors = Dict.toList (Dict.map (fn k, v: v.type) typeCheckGlobalEnv.constructors)
        }
        entryUsr
        @emittableState
        emittableStatements
    >> Ok

