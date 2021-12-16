#
# Native functions
#


forEach as Text: (Text: None): None =
    text: f:
    todo "native"


length as Text: Int =
    s:
    todo "native"


slice as Int: Int: Text: Text =
    start: end: s:
    todo "native"


fromNumber as Number: Text =
    n:
    todo "native"


toNumber as Text: Maybe Number =
    s:
    todo "native"


startsWith as Text: Text: Bool =
    sub: s:
    todo "native"


trimLeft as Text: Text =
    s:
    todo "native"


dropLeft as Int: Text: Text =
    n: s:
    todo "native"


dropRight as Int: Text: Text =
    n: s:

    if n > 0 then
        # TODO use -n once the parser is fixed
        slice 0 (0 - n) s
    else:
        s


padLeft as Int: Text: Text: Text =
    minLength: pad: s:

    textLength = Text.length s

    if textLength < minLength then
      times = (textLength - minLength) / Text.length pad
      repeat times pad .. s
    else:
      s


padRight as Int: Text: Text: Text =
    minLength: pad: s:

    textLength = Text.length s

    if textLength < minLength then
      times = (textLength - minLength) / Text.length pad
      s .. repeat times pad
    else:
      s


repeat as Int: Text: Text =
    n: s:

    join "" << List.repeat n s


replace as Text: Text: Text: Text =
    toRemove: toPut: s:

    # TODO use a native
    s >> Text.split toRemove >> Text.join toPut


# HACK
# Produces "" if it can't match anything, or if the regex is invalid.
# Good enough for what I need, but shouldn't probably be part of any API that wants to be solid.
startsWithRegex as Text: Text: Text =
    regex: s:
    todo "native"


replaceRegex as Text: Text: Text: Text =
    regex: replaceWith: s:
    todo "native"


split as Text: Text: [ Text ] =
    separator: target:
    todo "native"


contains as Text: Text: Bool =
    sub: str:

    # TODO use a native
    try split sub str as
      [ _ ]: False
      _: True

#
# Non native functions
#


join as Text: List Text: Text =
    sep: listOfText:

    try listOfText as
        SPCore.Nil:
          ""

        SPCore.Cons head tail:
          rec as [ Text ]: Text: Text =
            ls: acc:

            try ls as
              SPCore.Nil:
                acc
              SPCore.Cons h t:
                rec t << acc .. sep .. h

          rec tail head

