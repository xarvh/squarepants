
first t =
    as a & b: a
    t.first

second t =
    as a & b: b
    t.second

mapFirst f (a & b) =
    as (a: ma): a & b: ma & b
    f a & b

mapSecond f (a & b) =
    as (b: mb): a & b: a & mb
    a & f b

mapBoth fa fb (a & b) =
    as (a: ma): (b: mb): a & b: ma & mb
    fa a & fb b

