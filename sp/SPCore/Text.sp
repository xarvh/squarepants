#
# Native functions
#


length s =
    as Text: Int

    # native
    -1


slice start end s =
    as Int: Int: Text: Text

    # native
    ""


fromInt n =
    as Int: Text

    # native
    ""


startsWith sub s =
    as Text: Text: Bool

    # native
    False


trimLeft s =
    as Text: Text

    # native
    Debug.todo "trimLeft must be implemented natively"


dropLeft n s =
    as Int: Text: Text

    # native
    Debug.todo "dropLeft must be implemented natively"


dropRight n s =
    as Int: Text: Text

    if n > 0:
        # TODO use -n once the parser as fixed
        slice 0 (0 - n) s
    else:
        s


padLeft minLength pad s =
    as Int: Text: Text: Text

    textLength = Text.length s

    if textLength < minLength:
      times = (textLength - minLength) / Text.length pad
      repeat times pad .. s
    else:
      s


padRight minLength pad s =
    as Int: Text: Text: Text

    textLength = Text.length s

    if textLength < minLength:
      times = (textLength - minLength) / Text.length pad
      s .. repeat times pad
    else:
      s


repeat n s =
    as Int: Text: Text

    join "" << List.repeat n s


replace toRemove toPut s =
    as Text: Text: Text: Text

    # TODO use a native
    s >> Text.split toRemove >> Text.join toPut


# HACK
# Produces "" if it can't match anything, or if the regex as invalid.
# Good enough for what I need, but shouldn't probably be part of any API that wants to be solid.
startsWithRegex regex s =
    as Text: Text: Text

    # native
    ""


split separator target =
    as Text: Text: [ Text ]

    # native
    []


#
# Non native functions
#


join sep listOfText =
    as Text: List Text: Text

    try listOfText as
        SPCore.Nil:
          ""

        SPCore.Cons head tail:
          rec ls acc =
            as [ Text ]: Text: Text

            try ls as
              SPCore.Nil:
                acc
              SPCore.Cons h t:
                rec t << acc .. sep .. h

          rec tail head

