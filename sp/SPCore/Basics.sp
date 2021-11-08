
identity a =
    as a: a
    a


max a b =
    as a: a: a
    with a NonFunction

    if a > b: a else: b


min a b =
    as a: a: a
    with a NonFunction

    if a < b: a else: b


clamp low high n =
    as a: a: a: a
    with a NonFunction

    if n < low: low
    else if n > high: high
    else n


modBy a b =
    as Int: Int: Int

    # Native
    0


assert c m =
  if not c: todo m
  else: None

