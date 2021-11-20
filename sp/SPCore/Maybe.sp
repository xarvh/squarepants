
union Maybe a =
    , Nothing
    , Just a


andThen f ma =
    as (a: Maybe b): Maybe a: Maybe b

    try ma as
        Nothing: Nothing
        Just a: f a


map f m =
  as (a: b): Maybe a: Maybe b

  try m as
    Nothing: Nothing
    Just v: Just (f v)


map2 f ma mb =
    as (a: b: c): Maybe a: Maybe b: Maybe c
    ma >> andThen fn a:
    mb >> andThen fn b:
    Just (f a b)


map3 f ma mb mc =
    as (a: b: c: d): Maybe a: Maybe b: Maybe c: Maybe d
    ma >> andThen fn a:
    mb >> andThen fn b:
    mc >> andThen fn c:
    Just (f a b c)


mapRes f m =
    as (a: Result e b): Maybe a: Result e (Maybe b)

    try m as
        Nothing:
            Ok Nothing

        Just a:
            f a >> Result.map Just



withDefault default maybe =
  as a: Maybe a: a

  try maybe as
    Just v: v
    Nothing: default


