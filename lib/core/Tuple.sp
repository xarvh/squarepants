
first as a & b: a =
    t:
    t.first

second as a & b: b =
    t:
    t.second

mapFirst as (a: ma): a & b: ma & b =
    f: t:
    f t.first & t.second

mapSecond as (b: mb): a & b: a & mb =
    f: t:
    t.first & f t.second

mapBoth as (a: ma): (b: mb): a & b: ma & mb =
    fa: fb: t:
    fa t.first & fb t.second

pair as a: b: a & b =
    a: b:
    a & b

