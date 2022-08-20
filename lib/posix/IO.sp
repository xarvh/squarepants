

alias Env = {
    , vars as Dict Text Text
    }


alias Program =
    Env: [Text]: IO Int


union Never =
    Never Never


union IO a =
    IO (Never: Result Text a)


succeed as a: IO a =
    a:
    IO (never: Ok a)


fail as Text: IO a =
    message:
    IO (never: Err message)


_run as Never: IO a: Result Text a =
    never: r:
    IO neverToResult = r
    neverToResult never


onSuccess as (a: IO b): IO a: IO b =
    f: m:
    IO never:
        try _run never m as
            Ok a:
                _run never (f a)

            Err e:
                Err e


onResult as (Result Text a: IO b): IO a: IO b =
    f: m:
    IO never:
        m
            >> _run never
            >> f
            >> _run never


#
# Posix
#
parallel as [IO a]: IO [a] =
    _:
    todo "io.parallel"


readDir as Text: IO [Bool & Text] =
    _:
    todo "io.readDir"


readFile as Text: IO Text =
    _:
    todo "io.readFile"


writeFile as Text: Text: IO Int =
    _:
    todo "io.writeFile"


writeStdout as Text: IO Int =
    _:
    todo "io.writeStdout"


writeStderr as Text: IO Int =
    _:
    todo "io.writeStderr"

