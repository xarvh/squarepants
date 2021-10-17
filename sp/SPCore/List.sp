
# HACK
alias Int = Number


length =
    is [a] -> Int

    rec n ls =
      is Int -> [a] -> Int
      try ls as
        []: n
        h :: t: rec (n + 1) t

    rec 0


map f =
    is (a -> b) -> [a] -> [b]

    rec ls =
        is [a] -> [b]
        try ls as
            []:
                []

            head :: tail:
                # TODO make it TCO friendly
                (f head) :: (rec tail)

    rec


mapFirst f ls =
    is (a -> Maybe b) -> List a -> Maybe b

    try ls as
        []:
            Nothing

        head :: tail:
            r = f head
            if r == Nothing:
                mapFirst f tail
            else
                r


each ls f =
    is [a] -> (a -> b) -> None

    try ls as
        []:
            None

        head :: tail:
            f head
            each tail f


reverse aList =
    is [a] -> [a]

    rec ls acc =
        is [a] -> [a] -> [a]
        try ls as
            []:
                acc

            head :: tail:
                rec tail (head :: acc)

    rec aList []


repeat n a =
    is Int -> a -> [ a ]

    rec c acc =
        is Int -> [a] -> [a]
        if c > 0: rec (c - 1) (a :: acc) else acc

    rec n []


foldl function aList init =
    is (item -> state -> state) -> [ item ] -> state -> state

    try aList as
      []:
          init

      head :: tail:
          foldl function tail (function head init)


drop n ls =
    is Int -> [a] -> [a]

    if n == 0:
      ls
    else:
      try ls as
          []: []
          head :: tail: drop (n - 1) tail
