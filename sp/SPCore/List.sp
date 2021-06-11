
map f aList =
    as (a -> b) -> [ a ] -> [ b ]

    rec ls =
        try ls as
            []:
                []

            head :: tail:
                (f head) :: (rec tail)

    aList >> rec >> reverse


each ls f =
    as [ a ] -> (a -> b) -> None

    try ls as
        []:
            None

        head :: tail:
            f head
            each tail f


reverse aList =
    as [ a ] -> [ a ]

    rec ls acc =
        try ls as
            []:
                acc

            head :: tail:
                rec tail (head :: acc)

    rec aList []


repeat n a =
    as Number -> a -> [ a ]

    rec c acc =
        if c > 0 then rec (c - 1) (a :: acc) else acc

    rec n []


foldl function aList init =
    as (item -> state -> state) -> [ item ] -> state -> state

    try aList as
      []:
          init

      head :: tail:
          foldl function tail (function head init)

