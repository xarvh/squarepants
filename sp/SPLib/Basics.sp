
identity a =
    is a -> a
    a


max a b =
    is a -> a -> a
    with a NonFunciton

    if a > b: a else: b


min a b =
    is a -> a -> a
    with a NonFunciton

    if a < b: a else: b


assert c m =
  if not c: todo m
  else: None
