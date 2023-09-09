var Result error a =
    , 'err error
    , 'ok a


map as fn fn a: b, Result e a: Result e b =
    fn f, result:
    try result as
        'err e: 'err e
        'ok a: 'ok (f a)


onOk as fn fn a: Result e b: fn Result e a: Result e b =
    fn f:
    fn result:
    try result as
        'err e: 'err e
        'ok a: f a


onErr as fn fn a: Result b o: fn Result a o: Result b o =
    fn f:
    fn result:
    try result as
        'err e: f e
        'ok a: 'ok a


mapError as fn fn e1: e2, Result e1 a: Result e2 a =
    fn f, result:
    try result as
        'ok a: 'ok a
        'err e1: 'err (f e1)


fromMaybe as fn err, Maybe a: Result err a =
    fn err, maybe:
    try maybe as
        'nothing: 'err err
        'just a: 'ok a


withDefault as fn a, Result e a: a =
    fn default, result:
    try result as
        'ok a: a
        'err _: default
