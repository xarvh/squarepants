# HACK
alias Int = Number


foldl function aList init =
    is (item -> state -> state) -> [item] -> state -> state

    try aList as
      []:
          init

      h :: tail:
          foldl function tail (function h init)


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
                                                  foldl f (reverse r4) acc
                                              else
                                                  foldrHelper acc (ctr + 1) r4

                                          f a (f b (f c (f d res)))
    foldrHelper init 0 list


length list  =
    is [a] -> Int

    foldl (fn _ a: a + 1) list 0


map f list =
    is (a -> b) -> [a] -> [b]

    foldr (fn x acc: (f x) :: acc) list []


map2 f =
  is (a -> b -> c) -> [a] -> [b] -> [c]

  rec accum ax bx =
      is [c] -> [a] -> [b] -> [c]

      try ax & bx as
        ahead :: atail & bhead :: btail :
            rec (f ahead atail :: accum) atail btail
        _:
            reverse accum

  rec []


range low high =
    is Int -> Int -> [Int]

    rec accum up =
        is [Int] -> Int -> [Int]
        if up > low:
            rec (up :: accum) (up - 1)
        else:
            up :: accum

    rec [] high


indexedMap f =
    is (Int -> a -> b) -> [a] -> [b]

    rec accum n list =
      is [b] -> Int -> [a] -> [b]
      try list as
        []: reverse accum
        h :: t: rec (f n h :: accum) (n + 1) t

    rec [] 0


append xs ys =
  is List a -> List a -> List a
  try ys as
    []: xs
    _: foldr SPCore.Cons ys xs


concat lists =
  is List (List a) -> List a
  foldr append lists []


concatMap f list =
    is (a -> [b]) -> [a] -> [b]

    concat << map f list


head list =
    is [a] -> Maybe a

    try list as
      []: Nothing
      h :: t: Just h


last list =
    is [a] -> Maybe a
    try list as
        []: Nothing
        h :: []: Just h
        h :: t: last t


take n list =
    is Int -> [a] -> [a]
    if n < 1: list
    else:
      try list as
        []: []
        h :: tail: take (n - 1) tail


filter f ls =
    is (item -> Bool) -> [item] -> [item]

    foldr (fn item acc: if f item: item :: acc else acc) ls []


filterMap f xs =
  is (a -> Maybe b) -> [a] -> [b]
  foldr (fn a acc: try f a as Just b: b :: acc else acc) [] xs


mapFirst f ls =
    is (a -> Maybe b) -> List a -> Maybe b

    try ls as
        []:
            Nothing

        h :: tail:
            r = f h
            if r == Nothing:
                mapFirst f tail
            else
                r


each ls f =
    is [a] -> (a -> b) -> None

    try ls as
        []:
            None

        h :: tail:
            f h
            each tail f


reverse aList =
    is [a] -> [a]

    foldl SPCore.Cons aList []


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
          h :: tail: drop (n - 1) tail

minimum list =
    is [a] -> Maybe a
    with a NonFunction

    try list as
      x :: xs:
        Just (foldl min xs x)

      _:
        Nothing
