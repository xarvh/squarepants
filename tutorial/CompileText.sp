
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


resToConsoleText as fn Error.Env, Res a: Result Text a =
    fn errorEnv, res:
    try res as
        , Ok a: Ok a
        , Err e:
            e
            >> Error.toFormattedText errorEnv __
            >> List.map formattedToConsoleColoredText __
            >> Text.join "" __
            >> Err



onResSuccess as fn Error.Env, (fn a: Result Text b): fn Res a: Result Text b =
    fn errorEnv, f:
    fn res:
    res
    >> resToConsoleText errorEnv __
    >> onOk f





#
# Module loading
#

loadModule as fn Meta, UMR, Text: Res CA.Module =
    fn meta, umr, moduleAsText:

    # TODO get rid of eenv so this is not needed
    UMR source moduleName =
        umr

    params as Compiler/MakeCanonical.Params =
        {
        , meta
        , stripLocations = False
        , source
        , name = moduleName
        }

    eenv as Error.Env = {
        , moduleByName = Dict.ofOne moduleName {
            , fsPath = "<user input>"
            , content = moduleAsText
            }
        }

    Compiler/MakeCanonical.textToCanonicalModule params moduleAsText



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
              eenv as Error.Env =
                  {
                  , moduleByName = Dict.ofOne modulesFileName
                      {
                      , fsPath = modulesFileName
                      , content = platform.defaultModules
                      }
                  }

              errAsText =
                  err
                  >> Error.toFormattedText eenv __
                  >> List.map formattedToConsoleColoredText __
                  >> Text.join "" __

              Debug.log errAsText "--"

              Debug.todo "This is a compiler bug, not your fault."

    umr =
        UMR (Meta.SourceDir inputFileName) inputFileName


    entryUsr =
      USR umr "pixelColor"


    fn code:

    eenv as Error.Env =
        { moduleByName =
            Dict.ofOne inputFileName { fsPath = "", content = code }
        }

    loadModule meta umr code
    >> resToConsoleText eenv __
    >> onOk fn caModule:

    allCaModules =
        [ caModule, ...Prelude.coreModules ]

    allCaModules
    >> Compiler/TypeCheck.initStateAndGlobalEnv
    >> onResSuccess eenv fn (luv & typeCheckGlobalEnv):

    !lastUnificationVarId =
        cloneImm luv

    allCaModules
    >> List.mapRes (Compiler/TypeCheck.doModule @lastUnificationVarId typeCheckGlobalEnv __) __
    >> onResSuccess eenv fn taModules:

    # ---> check that `pixelColor` has the correct type

    taModules
    >> List.mapRes Compiler/UniquenessCheck.doModule __
    >> onResSuccess eenv fn modulesWithDestruction:

    modulesWithDestruction
    >> Compiler/MakeEmittable.translateAll
    >> Result.mapError (fn e: todo "MakeEmittable.translateAll returned Err") __
    >> onResSuccess eenv fn (meState & emittableStatements):

    !emittableState =
        cloneImm meState

    platform.compile
        {
        , errorEnv = eenv
        , constructors = Dict.toList (Dict.map (fn k, v: v.type) typeCheckGlobalEnv.constructors)
        }
        entryUsr
        @emittableState
        emittableStatements
    >> Ok

