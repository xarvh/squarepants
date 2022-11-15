
yellow as Text: Text =
    t:
    "<span class=yellow>" .. t .. "</span>"

red as Text: Text =
    t:
    "<span class=red>" .. t .. "</span>"

blue as Text: Text =
    t:
    "<span class=blue>" .. t .. "</span>"


formattedToConsoleColoredText as Error.FormattedText: Text =
    formattedText:
    try formattedText as
        Error.FormattedText_Default t: t
        Error.FormattedText_Emphasys t: yellow t
        Error.FormattedText_Warning t: red t
        Error.FormattedText_Decoration t: blue t


resToConsoleText as Error.Env: Res a: Result Text a =
    errorEnv: res:
    try res as
        Ok a: Ok a
        Err e:
            e
            >> Error.toFormattedText errorEnv
            >> List.map formattedToConsoleColoredText
            >> Text.join ""
            >> Err



onResSuccess as Error.Env: (a: Result Text b): Res a: Result Text b =
    errorEnv: f: res:
    res
    >> resToConsoleText errorEnv
    >> onOk f





#
# Module loading
#

loadModule as Meta: UMR: Text: Res CA.Module =
    meta: umr: moduleAsText:

    # TODO get rid of eenv so this is not needed
    UMR source moduleName =
        umr

    params as Compiler/MakeCanonical.Params = {
        , meta
        , stripLocations = False
        , source
        , name = moduleName
        }

    eenv as Error.Env = {
        , moduleByName = Dict.singleton moduleName {
            , fsPath = "<user input>"
            , content = moduleAsText
            }
        }

    Compiler/MakeCanonical.textToCanonicalModule params moduleAsText








#
# Compile
#

typeCheckModule as Meta: CA.Globals: CA.Module: Res Compiler/TypeCheck.Env =
    meta: globals: module:

    env as Compiler/TypeCheck.Env = {
        , currentModule = module.umr
        , meta
        , instanceVariables = Dict.mapKeys CA.RefRoot globals.instanceVariables
        , constructors = globals.constructors
        , types = globals.types
        , nonFreeTyvars = Dict.empty
        , nonAnnotatedRecursives = Dict.empty
        }

    Compiler/TypeCheck.fromModule env module







main as Text: Result Text Text =

    platform =
        Platforms/RawJavaScript.platform

    modulesFileName =
        "modules.sp"

    inputFileName =
        "user_input"

    meta =
      try ModulesFile.textToModulesFile modulesFileName platform.defaultModules as
          Ok m:
              ModulesFile.toMeta m

          Err err:
              eenv as Error.Env =
                  {
                  , moduleByName = Dict.singleton modulesFileName {
                      , fsPath = modulesFileName
                      , content = platform.defaultModules
                      }
                  }

              errAsText =
                  err
                    >> Error.toFormattedText eenv
                    >> List.map formattedToConsoleColoredText
                    >> Text.join ""

              Debug.log errAsText "--"

              Debug.todo "This is a compiler bug, not your fault."

    umr =
        UMR (Meta.SourceDir inputFileName) inputFileName


    entryUsr =
      USR umr "pixelColor"


    code:


    eenv as Error.Env =
        { moduleByName =
            Dict.singleton inputFileName { fsPath = "", content = code }
        }


    loadModule meta umr code
    >> resToConsoleText eenv
    >> onOk module:


    modules =
        Dict.singleton umr module


    globals =
        try Compiler/Pipeline.globalExpandedTypes modules as
            Err e:
                Debug.todo << Debug.toHuman e

            Ok g: g

    typeCheckModules =
        (Dict.values modules)
        >> List.mapRes (m: typeCheckModule meta globals m)


    # ---> check that `pixelColor` has the correct type

    Compiler/MakeEmittable.translateAll (Dict.values modules)
    >> Result.mapError (e: todo "MakeEmittable.translateAll returned Err")
    >> onResSuccess eenv emittableStatements:

    platform.compile {
        , errorEnv = eenv
        , constructors = Dict.toList globals.constructors
        }
        entryUsr
        emittableStatements
        >> Ok
