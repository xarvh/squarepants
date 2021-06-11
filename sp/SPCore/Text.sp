

fromInt n =
    as Number -> Text

    "<native>"


split separator target =
    as Text -> Text -> [ Text ]

    [ "<native>" ]


join sep listOfText =
    as Text -> List Text -> Text

    try listOfText as
        SPCore.Nil:
          ""

        SPCore.Cons head tail:
          rec ls acc =
            try ls as
              SPCore.Nil:
                acc
              SPCore.Cons h t:
                rec t << acc .. sep .. h

          rec tail head
