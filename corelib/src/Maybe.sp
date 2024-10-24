var Maybe a =
    , 'nothing
    , 'just a


onJust as fn (fn a: Maybe b): fn Maybe a: Maybe b =
    fn f:
    fn ma:
    try ma as
        'nothing: 'nothing
        'just a: f a


map as fn Maybe a, (fn a: b): Maybe b =
    fn m, f:
    try m as
        'nothing: 'nothing
        'just v: 'just (f v)


toResult as fn Maybe a, e: Result e a =
    fn maybeA, e:
    try maybeA as
        'just a: 'ok a
        'nothing: 'err e


withDefault as fn Maybe a, a: a =
    fn maybe, default:
    try maybe as
        'just v: v
        'nothing: default
