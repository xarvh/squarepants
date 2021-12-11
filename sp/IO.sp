

alias Env = {
    , vars as Dict Text Text
    }


alias Program =
    Env: [Text]: IO None


union Never =
    Never Never


union IO a =
    IO (Never: Result Text a)


succeed a =
    as a: IO a

    IO (fn never: Ok a)


fail message =
    as Text: IO a

    IO (fn never: Err message)


_run never (IO neverToResult) =
    as Never: IO a: Result Text a
    neverToResult never


onSuccess f m =
    as (a: IO b): IO a: IO b

    IO fn never:
        try _run never m as
            Ok a:
                _run never (f a)

            Err e:
                Err e


onResult f m =
    as (Result Text a: IO b): IO a: IO b

    IO fn never:
        m
            >> _run never
            >> f
            >> _run never


#
# Posix
#
parallel _ =
    as [IO a]: IO [a]
    todo "native0"


readDir _ =
    as Text: IO [Bool & Text]
    todo "native1"


readFile _ =
    as Text: IO Text
    todo "native2"


writeFile _ =
    as Text: Text: IO None
    todo "native3"


writeStdout _ =
    as Text: IO None
    todo "native4"

