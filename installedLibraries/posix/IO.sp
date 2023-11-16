Program =
    fn @IO, @Hash Text Text, [ Text ]: Int


# TODO This should be Res
Re b =
    Result Text b


var IO =
    , 'never IO


#
# Posix
#
reToStderr as fn @IO, Re a: Int =
    fn @io, re:
    try re as

        'ok _:
            0

        'err error:
            writeStderr @io (error .. "\n")

            1


readDir as fn @IO, Text: Re [ Bool & Text ] =
    fn @_, _:
    todo "io.readDir"


readFile as fn @IO, Text: Re Text =
    fn @_, _:
    todo "io.readFile"


readStdin as fn @IO: Re Text =
    todo "compiler error, IO.readStdin not overridden"


writeFile as fn @IO, Text, Text: Re None =
    fn @_, _, _:
    todo "io.writeFile"


writeStdout as fn @IO, Text: Re None =
    fn @_, _:
    todo "io.writeStdout"


writeStderr as fn @IO, Text: Re None =
    fn @_, _:
    todo "io.writeStderr"
