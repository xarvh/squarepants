
union Result error a =
    , Err error
    , Ok a


map f result =
    as (a: b): Result e a: Result e b

    try result as
        Err e:
            Err e

        Ok a:
            Ok (f a)


andThen f result =
    as (a: Result e b): Result e a: Result e b

    try result as
        Err e:
            Err e

        Ok a:
            f a


mapError f result =
    as (e1: e2): Result e1 a: Result e2 a

    try result as
        Ok a:
            Ok a

        Err e1:
            Err (f e1)
