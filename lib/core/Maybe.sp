
union Maybe a =
    , Nothing
    , Just a


andThen as fn (fn a: Maybe b): fn Maybe a: Maybe b =
    fn f: fn ma:
    try ma as
        , Nothing: Nothing
        , Just a: f a


map as fn (fn a: b), Maybe a: Maybe b =
    fn f, m:
    try m as
        , Nothing: Nothing
        , Just v: Just (f v)


#map2 as fn (fn a, b: c), Maybe a, Maybe b: Maybe c =
#    fn f, ma, mb:
#    ma >> andThen fn a:
#    mb >> andThen fn b:
#    Just (f a b)


#map3 as fn (fn a, b, c: d), Maybe a, Maybe b, Maybe c: Maybe d =
#    fn f, ma, mb, mc:
#    ma >> andThen fn a:
#    mb >> andThen fn b:
#    mc >> andThen fn c:
#    Just (f a b c)


mapRes as fn (fn a: Result e b), Maybe a: Result e (Maybe b) =
    fn f, m:
    try m as
        , Nothing:
            Ok Nothing

        , Just a:
            Result.map Just (f a)


withDefault as fn a, Maybe a: a =
    fn default, maybe:
    try maybe as
        , Just v: v
        , Nothing: default

