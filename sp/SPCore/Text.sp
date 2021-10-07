#
# Native functions
#


length s =
    as Text -> Number

    # native
    -1


slice start end s =
    as Number -> Number -> Text -> Text

    # native
    ""


fromInt n =
    as Number -> Text

    # native
    ""


startsWith sub s =
    as Text -> Text -> Bool

    # native
    False


trimLeft s =
    as Text -> Text

    # native
    Debug.todo "implement native!"




# HACK
# Produces "" if it can't match anything, or if the regex is invalid.
# Good enough for what I need, but shouldn't probably be part of any API that wants to be solid.
startsWithRegex regex s =
    as Text -> Text -> Text

    # native
    ""


split separator target =
    as Text -> Text -> [ Text ]

    # native
    []


#
# Non native functions
#


join sep listOfText =
    as Text -> List Text -> Text

    try listOfText as
        SPCore.Nil:
          ""

        SPCore.Cons head tail:
          rec ls acc =
            as [ Text ] -> Text -> Text

            try ls as
              SPCore.Nil:
                acc
              SPCore.Cons h t:
                rec t << acc .. sep .. h

          rec tail head

