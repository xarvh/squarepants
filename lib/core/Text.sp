#
# Native functions
#

forEach as fn Text, fn Text: None: None =
    fn text, f:
    todo "native"


length as fn Text: Int =
    fn s:
    todo "native"


slice as fn Int, Int, Text: Text =
    fn start, end, s:
    todo "native"


fromNumber as fn Number: Text =
    fn n:
    todo "native"


toNumber as fn Text: Maybe Number =
    fn s:
    todo "native"


startsWith as fn Text, Text: Bool =
    fn sub, s:
    todo "native"


trimLeft as fn Text: Text =
    fn s:
    todo "native"


trimRight as fn Text: Text =
    re =
        replaceRegex "[ ]*$"

    re "" __


toLower as fn Text: Text =
    fn t:
    todo "native"


toUpper as fn Text: Text =
    fn t:
    todo "native"


dropLeft as fn Int, Text: Text =
    fn n, s:
    todo "native"


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
            (textLength - minLength) / Text.length pad

        repeat times pad .. s
    else
        s


padRight as fn Int, Text, Text: Text =
    fn minLength, pad, s:
    textLength =
        Text.length s

    if textLength < minLength then
        times =
            (textLength - minLength) / Text.length pad

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
    fn regex:
    fn s:
    todo "native"


replaceRegex as fn Text: fn Text, Text: Text =
    fn regex:
    fn replaceWith, s:
    todo "native"


split as fn Text, Text: [ Text ] =
    fn separator, target:
    todo "native"


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
