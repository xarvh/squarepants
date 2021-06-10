
map f aList =
    as (a -> b) -> [ a ] -> [ b ]

    rec ls =
        try ls as
            SPCore.Nil:
                SPCore.Nil

            SPCore.Cons head tail:
                SPCore.Cons (f head) (rec tail)

    aList >> rec >> reverse


each ls f =
    as [ a ] -> (a -> b) -> None

    try ls as
        SPCore.Nil:
            None

        SPCore.Cons head tail:
            f head
            each tail f


reverse aList =
    as [ a ] -> [ a ]

    rec ls acc =
        try ls as
            SPCore.Nil:
                acc

            SPCore.Cons head tail:
                rec tail (SPCore.Cons head acc)

    rec aList []


repeat n a =
    as Number -> a -> [ a ]

    rec c acc =
        if c > 0 then rec (c - 1) (SPCore.Cons a acc) else acc

    rec n []

