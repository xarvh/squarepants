[#

module =
    path = ImportsFile
    exposes =
        ImportsFile open

module =
   path = Core
   exposes =
      None open
      Bool open
      Text
      List open
      Number

#]
ExportsFile =
    Dict Name (Dict Name Bool)


#
# ImportsFile to Imports
#

toExports as fn Imports, ExportsFile: Result [ Text ] Exports =
    fn imports, exportsFile:
    todo ""

    # TODO this should be an Array Error, but we don't have Pos annotation on ModulesFile/SPON!
    !errors as Array Text =
        Array.fromList []

    todo "...."

    errs =
        Array.toList @errors >> List.map (fn msg: Error.'raw [ msg ]) __

    if errs == [] then
        'ok meta
    else
        'err (Error.'nested errs)


#
# Reader
#

exposesReader as SPON.Reader { name as Name, open as Bool } =
    todo "...."


moduleReader as SPON.Reader { exposes as Dict Name Bool, path as Text } =
    SPON.field "path" SPON.text
    >> SPON.onAcc fn path:
    SPON.field "exposes" (SPON.many exposesReader)
    >> SPON.onAcc fn exposes:
    SPON.return { exposes = List.for Dict.empty exposes (fn e, d: Dict.insert e.name e.open d), path }


modulesFileReader as SPON.Reader ExportsFile =
    moduleReader
    >> SPON.many
    >> SPON.onAcc fn modules:
    List.for Dict.empty modules fn module, d:
        Dict.insert module.name module.exposes d
    >> SPON.return


fromText as fn Text, Text: Result [ Text ] ExportsFile =
    SPON.read exportsFileReader __ __
