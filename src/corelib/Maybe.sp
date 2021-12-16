
union Maybe a =
    , Nothing
    , Just a


andThen as (a: Maybe b): Maybe a: Maybe b =
    f: ma:
    try ma as
        Nothing: Nothing
        Just a: f a


map as (a: b): Maybe a: Maybe b =
  f: m:
  try m as
    Nothing: Nothing
    Just v: Just (f v)


map2 as (a: b: c): Maybe a: Maybe b: Maybe c =
    f: ma: mb:
    ma >> andThen a:
    mb >> andThen b:
    Just (f a b)


map3 as (a: b: c: d): Maybe a: Maybe b: Maybe c: Maybe d =
    f: ma: mb: mc:
    ma >> andThen a:
    mb >> andThen b:
    mc >> andThen c:
    Just (f a b c)


mapRes as (a: Result e b): Maybe a: Result e (Maybe b) =
    f: m:
    try m as
        Nothing:
            Ok Nothing

        Just a:
            f a >> Result.map Just


withDefault as a: Maybe a: a =
  default: maybe:
  try maybe as
    Just v: v
    Nothing: default

