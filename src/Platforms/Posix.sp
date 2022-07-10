

platform as Types/Platform.Platform = {
    , name = "posix"
    , compile
    , defaultModules = DefaultModules.asText .. posixModules
    , quickstart = "TODO"
    , defaultOutputPath = "nodeExecutable.js"
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


compile as Types/Platform.GetRidOfMe: Meta.UniqueSymbolReference: [EA.GlobalDefinition]: Text =
    getRidOfMe: targetUsr: emittableStatements:

    { errorEnv = eenv, constructors } =
        getRidOfMe

    log "Creating JS AST..." ""
    jaStatements =
        Targets/Javascript/EmittableToJs.translateAll eenv constructors emittableStatements

    log "Emitting JS..." ""

    callMain =
        """

        const out = """ .. Compiler/MakeEmittable.translateUsr targetUsr .. """({})(array_toList(process.argv.slice(1)))[1]('never');
        if (out[1]) console.error(out[1]);
        """

    statements =
        jaStatements
            >> List.map (Targets/Javascript/JsToText.emitStatement 0)
            >> Text.join "\n\n"

    header .. Targets/Javascript/Runtime.nativeDefinitions .. posixRuntime .. statements .. callMain


header as Text =
    # HACK the stack size is needed because we don't yet have tail-call optimization. T_T
    """#!/usr/bin/env -S node --stack-size=65500

//Error.stackTraceLimit = 100;

const { performance } = require('perf_hooks');

"""


posixRuntime as Text =
    """

//
// Platform: IO
//
const fs = require('fs');
const path = require('path');

const io_wrap = (f) => [ "IO.IO", f ];

const io_parallel = (iosAsList) => io_wrap((never) => {
    // as [IO a]: IO [a]

    const ios = array_fromList(iosAsList);

    // TODO actually run them in parallel!

    let arr = [];
    for (let io of ios) {
        const r = io[1](never);
        if (r[0] === "Ok")
            arr.push(r[1]);
        else
            return $core$Result$Err(r[1]);
    }

    return $core$Result$Ok(array_toList(arr));
});


const io_readDir = (dirPath) => io_wrap((never) => {
    // as Text: IO [Bool & Text]

    var entries;
    try {
        entries = fs.readdirSync(dirPath, { withFileTypes: true });
    } catch (e) {
        return $core$Result$Err(e.message);
    }

    return $core$Result$Ok(array_toList(entries.map((dirent) => ({
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


const io_writeFile = (path) => (content) => io_wrap((never) => {
    // as Text: Text: IO None

    try {
        fs.writeFileSync(path, content);
    } catch (e) {
        return $core$Result$Err(e.message);
    }

    return $core$Result$Ok(null);
});


const io_writeStdout = (content) => io_wrap((never) => {
    // as Text: IO None

    console.info(content);
    return $core$Result$Ok(null);
});


const path_resolve = (p) => path.resolve(...array_fromList(p));


const path_dirname = path.dirname;

"""
