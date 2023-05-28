

platform as Types/Platform.Platform =
    {
    , name = "webgpu"
    , defaultModules = webgpuModules
    , quickstart = "TODO"
    , defaultOutputPath = "webgpu.js"
    , makeExecutable
    }


webgpuModules as Text =
    """

library =
    source = "core:posix"

    module =
        path = IO
        globalTypes = IO

    module =
        path = Path
    """


makeExecutable as fn Self.LoadPars: Text =
    fn out:

    ""
#        Targets/Javascript/EmittableToJs.translateUsr out.entryUsr

#        jaStatements =
#            Targets/Javascript/EmittableToJs.translateAll
#                {
#                , constructors = out.constructors
#                , eaDefs = out.defs
#                , platformOverrides = overrides
#                }

#        jaStatements
#        >> List.map (Targets/Javascript/JsToText.emitStatement 0 __) __
#        >> Text.join "\n\n" __

#    header .. Targets/Javascript/Runtime.nativeDefinitions .. runtime .. compiledStatements .. callMain


overrides as [USR & Text] =
    []

#    ioModule =
#        USR (UMR Meta.Posix "IO") __
#
#    pathModule =
#        USR (UMR Meta.Posix "Path") __
#
#    [
#    , ioModule "parallel" & "io_parallel"
#    , ioModule "readDir" & "io_readDir"
#    , ioModule "readFile" & "io_readFile"
#    , ioModule "writeFile" & "io_writeFile"
#    , ioModule "writeStdout" & "io_writeStdout"
#    , ioModule "writeStderr" & "io_writeStderr"
#    , pathModule "dirname" & "path_dirname"
#    , pathModule "resolve" & "path_resolve"
#    ]

