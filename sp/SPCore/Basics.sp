
identity a =
    is a -> a
    a


max a b =
    is a -> a -> a
    with a NonFunction

    if a > b: a else: b


min a b =
    is a -> a -> a
    with a NonFunction

    if a < b: a else: b


clamp low high n =
    is a -> a -> a -> a
    with a NonFunction

    if n < low: low
    else if n > high: high
    else n


assert c m =
  if not c: todo m
  else: None

