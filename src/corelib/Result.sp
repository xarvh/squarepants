
union Result error a =
    , Err error
    , Ok a


map as (a: b): Result e a: Result e b =
    f: result:

    try result as
        Err e: Err e
        Ok a: Ok (f a)


onOk as (a: Result e b): Result e a: Result e b =
    f: result:

    try result as
        Err e: Err e
        Ok a: f a


mapError as (e1: e2): Result e1 a: Result e2 a =
    f: result:

    try result as
        Ok a: Ok a
        Err e1: Err (f e1)


fromMaybe as err: Maybe a: Result err a =
    err: maybe:

    try maybe as
        Nothing: Err err
        Just a: Ok a


withDefault as a: Result e a: a =
    default: result:

    try result as
        Ok a: a
        Err _: default

