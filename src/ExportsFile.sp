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


result_withAddError as fn fn [ error ]: groupedErrors, fn fn error: None: payload: Result groupedErrors payload =
    fn groupErrors, collectErrors:
    !errors =
        Array.fromList []

    addError =
        Array.push @errors __

    payload =
        collectErrors addError

    errorList =
        Array.toList @errors

    if errorList == [] then
        'ok payload
    else
        'err (groupErrors errorList)


#
# ImportsFile to Imports
#

toExports as fn Imports, ExportsFile: Res Exports =
    fn imports, exportsFile:
    result_withAddError Error.'nested fn addError_:
        addError =
            __ >> Error.'raw >> addError_

        Dict.for Dict.empty exportsFile fn modulePath, exposedNames, d:
            try Dict.get modulePath imports.modulePathToLocation as

                'nothing:
                    addError
                        [
                        , "TODO exports refers to a module with path " .. modulePath .. " but that is not in imports."
                        , imports.modulePathToLocation
                            >> Dict.keys
                            >> Text.join ", " __
                        ]

                    d

                'just (Meta.'locationLibrary importsPath modulePath_):
                    addError
                        [
                        , "TODO you can't export modules from a library"
                        ]

                    d

                'just (Meta.'locationSourceDir umr):
                    Dict.for d exposedNames fn name, isOpen, dd:
                        usr =
                            'USR umr name

                        addNameToModule as fn Maybe (Dict Name Meta.ExportOptions): Maybe (Dict Name Meta.ExportOptions) =
                            __
                            >> Maybe.withDefault __ Dict.empty
                            >> Dict.insert name { isOpen, usr } __
                            >> 'just

                        Dict.update modulePath addNameToModule dd


#
# Reader
#

exposesReader as SPON.Reader { name as Name, open as Bool } =
    SPON.anyName
    >> SPON.onAcc fn name:
    # TODO
    { name, open = 'true } >> SPON.return


moduleReader as SPON.Reader { exposes as Dict Name Bool, path as Text } =
    SPON.field "path" SPON.upperName
    >> SPON.onAcc fn path:
    SPON.maybe (SPON.field "exposes" (SPON.many exposesReader))
    >> SPON.onAcc fn maybeExposes:
    SPON.return { exposes = List.for Dict.empty (Maybe.withDefault maybeExposes []) (fn d, e: Dict.insert e.name e.open d), path }


exportsFileReader as SPON.Reader ExportsFile =
    SPON.field "module" moduleReader
    >> SPON.many
    >> SPON.onAcc fn modules:
    List.for Dict.empty modules fn d, module:
        Dict.insert module.path module.exposes d
    >> SPON.return


fromText as fn Text, Text: Res ExportsFile =
    SPON.read exportsFileReader __ __
