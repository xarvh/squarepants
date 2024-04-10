#
# Native functions
#

forEach as fn Text, fn Text: None: None =
    this_is_sp_native


length as fn Text: Int =
    this_is_sp_native


slice as fn Int, Int, Text: Text =
    this_is_sp_native


fromNumber as fn Number: Text =
    this_is_sp_native


toNumber as fn Text: Maybe Number =
    this_is_sp_native


startsWith as fn Text, Text: Bool =
    this_is_sp_native


trimLeft as fn Text: Text =
    this_is_sp_native


trimRight as fn Text: Text =
    re =
        replaceRegex "[ ]*$"

    re "" __


toLower as fn Text: Text =
    this_is_sp_native


toUpper as fn Text: Text =
    this_is_sp_native


dropLeft as fn Int, Text: Text =
    this_is_sp_native


dropRight as fn Int, Text: Text =
    fn n, s:
    if n > 0 then
        slice 0 -n s
    else
        s


padLeft as fn Int, Text, Text: Text =
    fn minLength, pad, s:
    textLength =
        Text.length s

    if textLength < minLength then
        times =
            (minLength - textLength) / Text.length pad

        repeat times pad .. s
    else
        s


padRight as fn Int, Text, Text: Text =
    fn minLength, pad, s:
    textLength =
        Text.length s

    if textLength < minLength then
        times =
            (minLength - textLength) / Text.length pad

        s .. repeat times pad
    else
        s


repeat as fn Int, Text: Text =
    fn n, s:
    join "" (List.repeat n s)


replace as fn Text, Text, Text: Text =
    fn toRemove, toPut, s:
    # TODO use a native
    s
    >> Text.split toRemove __
    >> Text.join toPut __


# HACK
# Produces "" if it can't match anything, or if the regex is invalid.
# Good enough for what I need, but shouldn't probably be part of any API that wants to be solid.
# TODO: also, why "startsWithRegex" rather than just "findRegex"?
startsWithRegex as fn Text: fn Text: Text =
    this_is_sp_native


replaceRegex as fn Text: fn Text, Text: Text =
    this_is_sp_native


split as fn Text, Text: [ Text ] =
    this_is_sp_native


contains as fn Text, Text: Bool =
    fn sub, str:
    # TODO use a native
    try split sub str as
        [ _ ]: 'false
        _: 'true


#
# Non native functions
#

join as fn Text, [ Text ]: Text =
    fn sep, listOfText:
    try listOfText as

        Core.'nil:
            ""

        Core.'cons head tail:
            rec as fn [ Text ], Text: Text =
                fn ls, acc:
                try ls as
                    Core.'nil: acc
                    Core.'cons h t: rec t (acc .. sep .. h)

            rec tail head
