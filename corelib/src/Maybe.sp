var Maybe a =
    , 'nothing
    , 'just a


onJust as fn fn a: Maybe b: fn Maybe a: Maybe b =
    fn f:
    fn ma:
    try ma as
        'nothing: 'nothing
        'just a: f a


map as fn fn a: b, Maybe a: Maybe b =
    fn f, m:
    try m as
        'nothing: 'nothing
        'just v: 'just (f v)


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

toResult as fn e, Maybe a: Result e a =
    fn e, maybeA:
    try maybeA as
        'just a: 'ok a
        'nothing: 'err e


mapRes as fn fn a: Result e b, Maybe a: Result e (Maybe b) =
    fn f, m:
    try m as
        'nothing: 'ok 'nothing
        'just a: Result.map 'just (f a)


withDefault as fn a, Maybe a: a =
    fn default, maybe:
    try maybe as
        'just v: v
        'nothing: default
