
union Result error a =
    , Err error
    , Ok a


mapError f result =
    as (e1 -> e2) -> Result e1 a -> Result e2 a

    try result as
        Ok a:
            Ok a

        Err e1:
            Err (f e1)
