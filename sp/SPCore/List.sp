# HACK
alias Int = Number


foldl function aList init =
    as (item: state: state): [item]: state: state

    try aList as
      []:
          init

      h :: tail:
          foldl function tail (function h init)


foldr f list init =
    as (item: state: state): [item]: state: state

    foldrHelper acc ctr ls =
        as state: Int: List item: state
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
    as [a]: Int

    foldl (fn _ a: a + 1) list 0


map f list =
    as (a: b): [a]: [b]

    foldr (fn x acc: (f x) :: acc) list []


map2 f =
  as (a: b: c): [a]: [b]: [c]

  rec accum ax bx =
      as [c]: [a]: [b]: [c]

      try ax & bx as
        ahead :: atail & bhead :: btail :
            rec (f ahead atail :: accum) atail btail
        _:
            reverse accum

  rec []


range low high =
    as Int: Int: [Int]

    rec accum up =
        as [Int]: Int: [Int]
        if up > low:
            rec (up :: accum) (up - 1)
        else:
            up :: accum

    rec [] high


indexedMap f =
    as (Int: a: b): [a]: [b]

    rec accum n list =
      as [b]: Int: [a]: [b]
      try list as
        []: reverse accum
        h :: t: rec (f n h :: accum) (n + 1) t

    rec [] 0


append xs ys =
  as List a: List a: List a
  try ys as
    []: xs
    _: foldr SPCore.Cons ys xs


concat lists =
  as List (List a): List a
  foldr append lists []


concatMap f list =
    as (a: [b]): [a]: [b]

    concat << map f list


head list =
    as [a]: Maybe a

    try list as
      []: Nothing
      h :: t: Just h


last list =
    as [a]: Maybe a
    try list as
        []: Nothing
        h :: []: Just h
        h :: t: last t


take =
    as Int: [a]: [a]
    takeFast 0


# Shamelessly stolen from https://github.com/elm/core/blob/1.0.5/src/List.elm
takeFast ctr n list =
  as Int: Int: [a]: [a]
  if n < 1:
    []
  else
    try n & list as
        _ & []:
          list

        1 & x :: _:
          [ x ]

        2 & x :: y :: _:
          [ x, y ]

        3 & x :: y :: z :: _:
          [ x, y, z ]

        _ & x :: y :: z :: w :: tl:
          cons = SPCore.Cons
          if ctr > 1000:
            cons x (cons y (cons z (cons w (takeTailRec (n - 4) tl))))
          else
            cons x (cons y (cons z (cons w (takeFast (ctr + 1) (n - 4) tl))))

        _ :
          list


takeTailRec n list =
  as Int: [a]: [a]
  reverse (takeReverse n list [])


takeReverse n list kept =
  as Int: [a]: [a]: [a]
  if n < 1:
    kept
  else
    try list as
      []:
        kept

      x :: xs:
        takeReverse (n - 1) xs (SPCore.Cons x kept)


filter f ls =
    as (item: Bool): [item]: [item]

    foldr (fn item acc: if f item: item :: acc else acc) ls []


filterMap f xs =
  as (a: Maybe b): [a]: [b]
  foldr (fn a acc: try f a as Just b: b :: acc else acc) [] xs


mapFirst f ls =
    as (a: Maybe b): List a: Maybe b

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
    as [a]: (a: b): None

    try ls as
        []:
            None

        h :: tail:
            f h
            each tail f


reverse aList =
    as [a]: [a]

    foldl SPCore.Cons aList []


repeat n a =
    as Int: a: [ a ]

    rec c acc =
        as Int: [a]: [a]
        if c > 0: rec (c - 1) (a :: acc) else acc

    rec n []


drop n ls =
    as Int: [a]: [a]

    if n == 0:
      ls
    else:
      try ls as
          []: []
          h :: tail: drop (n - 1) tail

minimum list =
    as [a]: Maybe a
    with a NonFunction

    try list as
      x :: xs:
        Just (foldl min xs x)

      _:
        Nothing
