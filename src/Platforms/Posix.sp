platform as fn Meta.ImportsPath: Platform =
    fn platformImportsPath:
    {
    , defaultImportsFile
    , defaultOutputName = "nodeExecutable.js"
    , makeExecutable = makeExecutable platformImportsPath
    , name = "posix"
    , quickstart = "TODO"
    }


defaultImportsFile as ImportsFile =
    DefaultImports.platformDefaultImportsFile
        [
        , "IO" & [ "IO" ]
        , "Path" & []
        ]

State =
    Targets/Javascript/EmittableToJs.State

makeExecutable as fn Meta.ImportsPath: fn Self.LoadPars: Text =
    fn platformImportsPath:
    fn out:

    !state as State =
        cloneImm Targets/Javascript/EmittableToJs.initState


    entryName =
        Targets/Javascript/EmittableToJs.translateUsr @state out.entryUsr

    callMain =
        """


        const args = arrayToListLow(process.argv.slice(1));
        process.exitCode =
        """
        .. entryName
        .. """
        (null, process.env, args)[0];
        """

    compiledStatements as Text =
        log "Creating JS AST..." ""

        jaStatements =
            Targets/Javascript/EmittableToJs.translateAll @state
                {
                , constructors = out.constructors
                , eaDefs = out.defs
                , platformOverrides = overrides platformImportsPath
                }

        log "Emitting JS..." ""

        jaStatements
        >> List.map (Targets/Javascript/JsToText.emitStatement 0 __) __
        >> Text.join "\n\n" __

    header .. Targets/Javascript/Runtime.nativeDefinitions @state .. runtime @state .. compiledStatements .. callMain


header as Text =
    # HACK the stack size is needed because we don't yet have tail-call optimization. T_T
    """
    #!/usr/bin/env -S node --stack-size=65500 --max-old-space-size=4096

    //Error.stackTraceLimit = 100;

    const { performance } = require('perf_hooks');


    """


overrides as fn Meta.ImportsPath: [ USR & Text ] =
    fn platformImportsPath:
    ioModule =
        'USR ('UMR platformImportsPath "" "IO") __

    pathModule =
        'USR ('UMR platformImportsPath "" "Path") __

    [
    , ioModule "parallel" & "io_parallel"
    , ioModule "readDir" & "io_readDir"
    , ioModule "readFile" & "io_readFile"
    , ioModule "writeFile" & "io_writeFile"
    , ioModule "readStdin" & "io_readStdin"
    , ioModule "writeStdout" & "io_writeStdout"
    , ioModule "writeStderr" & "io_writeStderr"
    , pathModule "dirname" & "path_dirname"
    , pathModule "resolve" & "path_resolve"
    , pathModule "join" & "path_join"
    ]


runtime as fn @State: Text =
    fn @state:

    makeOk as Text =
        'USR ('UMR CoreDefs.importsPath "src" "Result") "'ok" >> Targets/Javascript/EmittableToJs.translateUsr @state __

    makeErr as Text =
        'USR ('UMR CoreDefs.importsPath "src" "Result") "'ok" >> Targets/Javascript/EmittableToJs.translateUsr @state __


    """

    //
    // Platform: IO
    //
    const fs = require('fs');
    const path = require('path');

    const io_readDir = (io, dirPath) => {
        // as @IO, Text: Re [Bool & Text]

        var entries;
        try {
            entries = fs.readdirSync(dirPath, { withFileTypes: true });
        } catch (e) {
            return [
    """
    .. makeErr
    .. """
    (e.message), null];
            }

            return [
    """
    .. makeOk
    .. """
    (arrayToListLow(entries.map((dirent) => ({
                first: dirent.isDirectory(),
                second: dirent.name,
            })))), null];
        };


        const io_readFile = (io, path) => {
            // as @IO, Text: Re Text

            var content;
            try {
                content = fs.readFileSync(path, 'utf8');
            } catch (e) {
                return [
    """
    .. makeErr
    .. """
    (e.message), null];
            }

            return [
    """
    .. makeOk
    .. """
    (content), null];
        };


        const io_writeFile = (io, path, content) => {
            // as @IO, Text, Text: Re Int

            try {
                fs.writeFileSync(path, content);
            } catch (e) {
                return [
    """
    .. makeErr
    .. """
    (e.message), null];
            }

            return [
    """
    .. makeOk
    .. """
    (0), null];
        };


        const io_readStdin = (io) => {
            // as @IO: Re Text

            try {
                return [
    """
    .. makeOk
    .. """
    (fs.readFileSync(0, 'utf8')), null];
            } catch (e) {
                return [
    """
    .. makeErr
    .. """
    (e.message), null];
            }
        };


        const io_writeStdout = (io, content) => {
            // as @IO, Text: Re None

            try {
                fs.writeFileSync(1, content);
            } catch (e) {
                return [
    """
    .. makeErr
    .. """
    (e.message), null];
            }

            return [
    """
    .. makeOk
    .. """
    (null), null];
        };


        const io_writeStderr = (io, content) => {
            // as @IO, Text: Re Int

            try {
                fs.writeFileSync(2, content);
            } catch (e) {
                return [
    """
    .. makeErr
    .. """
    (e.message), null];
            }

            return [
    """
    .. makeOk
    .. """
    (null), null];
        };


        const path_resolve = (p) => path.resolve(...arrayFromListLow(p));

        const path_join = (p) => path.join(...arrayFromListLow(p));

        const path_dirname = path.dirname;


    """
