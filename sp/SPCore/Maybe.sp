
union Maybe a =
    , Nothing
    , Just a


andThen f ma =
    is (a -> Maybe b) -> Maybe a -> Maybe b

    try ma as
        Nothing: Nothing
        Just a: f a


map f m =
  is (a -> b) -> Maybe a -> Maybe b

  try m as
    Nothing: Nothing
    Just v: Just (f v)

