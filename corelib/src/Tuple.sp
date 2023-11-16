first as fn a & b: a =
    fn t:
    t.first


second as fn a & b: b =
    fn t:
    t.second


mapFirst as fn fn a: ma, a & b: ma & b =
    fn f, t:
    f t.first & t.second


mapSecond as fn fn b: mb, a & b: a & mb =
    fn f, t:
    t.first & f t.second


mapBoth as fn fn a: ma, fn b: mb, a & b: ma & mb =
    fn fa, fb, t:
    fa t.first & fb t.second


pair as fn a, b: a & b =
    fn a, b:
    a & b
