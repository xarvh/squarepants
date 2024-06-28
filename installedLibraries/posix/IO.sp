Program =
    fn @IO, @Hash Text Text, [ Text ]: Int


# TODO This should be Res
Re b =
    Result Text b


var IO_ =
    , 'never IO_


IO =
    { never as IO_ }


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
    this_is_sp_native


readFile as fn @IO, Text: Re Text =
    this_is_sp_native


readStdin as fn @IO: Re Text =
    this_is_sp_native


writeFile as fn @IO, Text, Text: Re None =
    this_is_sp_native


writeStdout as fn @IO, Text: Re None =
    this_is_sp_native


writeStderr as fn @IO, Text: Re None =
    this_is_sp_native
