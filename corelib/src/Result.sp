var Result error a =
    , 'err error
    , 'ok a


map as fn Result e a, (fn a: b): Result e b =
    fn result, f:
    try result as
        'err e: 'err e
        'ok a: 'ok (f a)


onOk as fn (fn a: Result e b): fn Result e a: Result e b =
    fn f:
    fn result:
    try result as
        'err e: 'err e
        'ok a: f a


onErr as fn (fn a: Result b o): fn Result a o: Result b o =
    fn f:
    fn result:
    try result as
        'err e: f e
        'ok a: 'ok a


resolveErrorsWith as fn (fn err: a), (fn None: Result err a): a =
    fn errorToPayload, getResult:
    try getResult 'none as
        'ok payload: payload
        'err error: errorToPayload error


mapError as fn Result e1 a, (fn e1: e2): Result e2 a =
    fn result, f:
    try result as
        'ok a: 'ok a
        'err e1: 'err (f e1)


fromMaybe as fn Maybe a, err: Result err a =
    fn maybe, err:
    try maybe as
        'nothing: 'err err
        'just a: 'ok a


withDefault as fn Result e a, a: a =
    fn default, result:
    try result as
        'ok a: a
        'err _: default
