#
# Native functions
#


length s =
    is Text -> Int

    # native
    -1


slice start end s =
    is Int -> Int -> Text -> Text

    # native
    ""


fromInt n =
    is Int -> Text

    # native
    ""


startsWith sub s =
    is Text -> Text -> Bool

    # native
    False


trimLeft s =
    is Text -> Text

    # native
    Debug.todo "trimLeft must be implemented natively"


dropRight n s =
    is Int -> Text -> Text

    # native
    Debug.todo "dropRight must be implemented natively"


uncons s =
    is Text -> Maybe (Text & Text)

    # native
    Debug.todo "uncons must be implemented natively"



# HACK
# Produces "" if it can't match anything, or if the regex is invalid.
# Good enough for what I need, but shouldn't probably be part of any API that wants to be solid.
startsWithRegex regex s =
    is Text -> Text -> Text

    # native
    ""


split separator target =
    is Text -> Text -> [ Text ]

    # native
    []


#
# Non native functions
#


join sep listOfText =
    is Text -> List Text -> Text

    try listOfText as
        SPCore.Nil:
          ""

        SPCore.Cons head tail:
          rec ls acc =
            is [ Text ] -> Text -> Text

            try ls as
              SPCore.Nil:
                acc
              SPCore.Cons h t:
                rec t << acc .. sep .. h

          rec tail head

