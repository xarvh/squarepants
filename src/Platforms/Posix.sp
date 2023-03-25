

platform as Types/Platform.Platform =
    {
    , name = "posix"
    , defaultModules = DefaultModules.asText .. posixModules
    , quickstart = "TODO"
    , defaultOutputPath = "nodeExecutable.js"
    , makeExecutable
    }


posixModules as Text =
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

    # TODO check that type is `IO Int`

    callMain =
        """
        const args = arrayToListLow(process.argv.slice(1));
        const out = """ .. out.entryName .. """({}, args)[1]('never');
        if (out[0] === 'Ok') {
            process.exitCode = out[1];
        } else {
            console.error(out[1]);
            process.exitCode = 1;
        }
        """

    compiledStatements =
        log "Creating JS AST..." ""
        jaStatements =
            Targets/Javascript/EmittableToJs.translateAll
                {
                , constructors = out.constructors
                , eaDefs = out.defs
                , platformOverrides = overrides
                }

        log "Emitting JS..." ""
        jaStatements
        >> List.map (Targets/Javascript/JsToText.emitStatement 0 __) __
        >> Text.join "\n\n" __

    header .. Targets/Javascript/Runtime.nativeDefinitions .. runtime .. compiledStatements .. callMain


header as Text =
    # HACK the stack size is needed because we don't yet have tail-call optimization. T_T
    """#!/usr/bin/env -S node --stack-size=65500 --max-old-space-size=4096

//Error.stackTraceLimit = 100;

const { performance } = require('perf_hooks');

"""


overrides as [USR & Text] =

    ioModule =
        USR (UMR Meta.Posix "IO") __

    pathModule =
        USR (UMR Meta.Posix "Path") __

    [
    , ioModule "parallel" & "io_parallel"
    , ioModule "readDir" & "io_readDir"
    , ioModule "readFile" & "io_readFile"
    , ioModule "writeFile" & "io_writeFile"
    , ioModule "writeStdout" & "io_writeStdout"
    , ioModule "writeStderr" & "io_writeStderr"
    , pathModule "dirname" & "path_dirname"
    , pathModule "resolve" & "path_resolve"
    ]


runtime as Text =
    """

//
// Platform: IO
//
const fs = require('fs');
const path = require('path');

const io_wrap = (f) => [ "IO.IO", f ];

const io_parallel = (iosAsList) => io_wrap((never) => {
    // as [IO a]: IO [a]

    const ios = arrayFromListLow(iosAsList);

    // TODO actually run them in parallel!

    let arr = [];
    for (let io of ios) {
        const r = io[1](never);
        if (r[0] === "Ok")
            arr.push(r[1]);
        else
            return $core$Result$Err(r[1]);
    }

    return $core$Result$Ok(arrayToListLow(arr));
});


const io_readDir = (dirPath) => io_wrap((never) => {
    // as Text: IO [Bool & Text]

    var entries;
    try {
        entries = fs.readdirSync(dirPath, { withFileTypes: true });
    } catch (e) {
        return $core$Result$Err(e.message);
    }

    return $core$Result$Ok(arrayToListLow(entries.map((dirent) => ({
        first: dirent.isDirectory(),
        second: dirent.name,
    }))));
});


const io_readFile = (path) => io_wrap((never) => {
    // as Text: IO Text

    var content;
    try {
        content = fs.readFileSync(path, 'utf8');
    } catch (e) {
        return $core$Result$Err(e.message);
    }

    return $core$Result$Ok(content);
});


const io_writeFile = (path, content) => io_wrap((never) => {
    // as Text: Text: IO Int

    try {
        fs.writeFileSync(path, content);
    } catch (e) {
        return $core$Result$Err(e.message);
    }

    return $core$Result$Ok(0);
});


const io_writeStdout = (content) => io_wrap((never) => {
    // as Text: IO Int

    console.info(content);
    return $core$Result$Ok(0);
});


const io_writeStderr = (content) => io_wrap((never) => {
    // as Text: IO Int

    console.error(content);
    return $core$Result$Ok(-1);
});


const path_resolve = (p) => path.resolve(...arrayFromListLow(p));


const path_dirname = path.dirname;

"""

