
identity a =
    as a: a
    a


max a b =
    as Number: Number: Number

    if a > b: a else: b


min a b =
    as Number: Number: Number

    if a < b: a else: b


clamp low high n =
    as Number: Number: Number: Number

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

