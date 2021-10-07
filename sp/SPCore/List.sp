
map f =
    as (a -> b) -> [a] -> [b]

    rec ls =
        as [a] -> [b]
        try ls as
            []:
                []

            head :: tail:
                # TODO make it TCO friendly
                (f head) :: (rec tail)

    rec


mapFirst f ls =
    as (a -> Maybe b) -> List a -> Maybe b

    try ls as
        []:
            Nothing

        head :: tail:
            r = f head
            if r == Nothing then
                mapFirst f tail
            else
                r


each ls f =
    as [a] -> (a -> b) -> None

    try ls as
        []:
            None

        head :: tail:
            f head
            each tail f


reverse aList =
    as [a] -> [a]

    rec ls acc =
        as [a] -> [a] -> [a]
        try ls as
            []:
                acc

            head :: tail:
                rec tail (head :: acc)

    rec aList []


repeat n a =
    as Int -> a -> [ a ]

    rec c acc =
        as Int -> [a] -> [a]
        if c > 0 then rec (c - 1) (a :: acc) else acc

    rec n []


foldl function aList init =
    as (item -> state -> state) -> [ item ] -> state -> state

    try aList as
      []:
          init

      head :: tail:
          foldl function tail (function head init)

