
union Maybe a =
    , Nothing
    , Just a


andThen f ma =
    as (a -> Maybe b) -> Maybe a -> Maybe b

    try ma as
        Nothing: Nothing
        Just a: f a


map f m =
  as (a -> b) -> Maybe a -> Maybe b

  try m as
    Nothing: Nothing
    Just v: Just (f v)

