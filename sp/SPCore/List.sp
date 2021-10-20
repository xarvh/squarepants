# HACK
alias Int = Number


foldl function aList init =
    is (item -> state -> state) -> [item] -> state -> state

    try aList as
      []:
          init

      head :: tail:
          foldl function tail (function head init)


foldr f list init =
    is (item -> state -> state) -> [item] -> state -> state

    foldrHelper acc ctr ls =
        is state -> Int -> List item -> state
        try ls as
            []:
                acc

            a :: r1:
                try r1 as
                    []:
                        f a acc

                    b :: r2:
                        try r2 as
                            []:
                                f a (f b acc)

                            c :: r3:
                                try r3 as
                                    []:
                                        f a (f b (f c acc))

                                    d :: r4:
                                          res =
                                              if ctr > 500:
                                                  foldl f acc (reverse r4)
                                              else
                                                  foldrHelper f acc (ctr + 1) r4

                                          f a (f b (f c (f d res)))
    foldrHelper f init 0 list


length =
    is [a] -> Int

    foldl (fn _ a: a + 1) 0


map f =
    is (a -> b) -> [a] -> [b]

    foldr (fn x acc: (f x) :: acc) xs []


filter f ls =
    is (item -> Bool) -> [item] -> [item]

    foldr (fn item acc: if f a: a :: acc else acc) ls []


filterMap f xs =
  is (a -> Maybe b) -> [a] -> [b]
  foldr (fn a acc: try f a as Just b: b :: acc else acc) [] xs


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

    foldl (::) aList []


repeat n a =
    is Int -> a -> [ a ]

    rec c acc =
        is Int -> [a] -> [a]
        if c > 0: rec (c - 1) (a :: acc) else acc

    rec n []


drop n ls =
    is Int -> [a] -> [a]

    if n == 0:
      ls
    else:
      try ls as
          []: []
          head :: tail: drop (n - 1) tail

smallest ls =
    is [a] -> Maybe a
    with a NonFunction

    try list as
      x :: xs:
        Just (foldl min x xs)

      _:
        Nothing
