

alias Env =
    {
    , vars as Dict Text Text
    }


alias Program =
    fn Env, [Text]: IO Int


union Never =
    Never Never


union IO a =
    IO (fn Never: Result Text a)


succeed as fn a: IO a =
    fn a:
    IO (fn never: Ok a)


fail as fn Text: IO a =
    fn message:
    IO (fn never: Err message)


_run as fn Never, IO a: Result Text a =
    fn never, r:
    IO neverToResult = r
    neverToResult never


onSuccess as fn (fn a: IO b): fn IO a: IO b =
    fn f: fn m:
    IO fn never:
        try _run never m as
            , Ok a:
                _run never (f a)

            , Err e:
                Err e


onResult as fn (fn Result Text a: IO b): fn IO a: IO b =
    fn f: fn m:
    IO fn never:
        m
        >> _run never __
        >> f
        >> _run never __


#
# Posix
#
parallel as fn [IO a]: IO [a] =
    fn _:
    todo "io.parallel"


readDir as fn Text: IO [Bool & Text] =
    fn _:
    todo "io.readDir"


readFile as fn Text: IO Text =
    fn _:
    todo "io.readFile"


readStdin as IO Text =
    succeed "compiler error, IO.readStdin not overridden"


writeFile as fn Text, Text: IO Int =
    fn _, _:
    todo "io.writeFile"


writeStdout as fn Text: IO Int =
    fn _:
    todo "io.writeStdout"


writeStderr as fn Text: IO Int =
    fn _:
    todo "io.writeStderr"

