
union Result error a =
    , Err error
    , Ok a


map as fn (fn a: b), Result e a: Result e b =
    fn f, result:

    try result as
        , Err e: Err e
        , Ok a: Ok (f a)


onOk as fn (fn a: Result e b): fn Result e a: Result e b =
    fn f: fn result:

    try result as
        , Err e: Err e
        , Ok a: f a


onErr as fn (fn e: Result e a): fn Result e a: Result e a =
    fn f: fn result:

    try result as
        , Err e: f e
        , Ok a: Ok a


mapError as fn (fn e1: e2), Result e1 a: Result e2 a =
    fn f, result:

    try result as
        , Ok a: Ok a
        , Err e1: Err (f e1)


fromMaybe as fn err, Maybe a: Result err a =
    fn err, maybe:

    try maybe as
        , Nothing: Err err
        , Just a: Ok a


withDefault as fn a, Result e a: a =
    fn default, result:

    try result as
        , Ok a: a
        , Err _: default

