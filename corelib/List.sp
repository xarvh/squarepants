

for as [item]: (item: state: state): state: state =
    list: fun:
    foldl fun list


any as (a: Bool): [a]: Bool =
    fun: list:

    try list as
        []: False
        h :: t: if fun h then True else any fun t


all as (a: Bool): [a]: Bool =
    fun: list:
    try list as
        []: True
        h :: t:
            if fun h then
                all fun t
            else
                False


find as (a: Bool): [a]: Maybe a =
    test: list:
    try list as
        []: Nothing
        h :: t:
            if test h then
                Just h
            else
                find test t


member as a: [a]: Bool =
    a: list:
    try list as
        []: False

        h :: t:
            if a == h then
                True
            else
                member a t


sort as [a]: [a] =
    #with a NonFunction
    sortBy identity


sortBy as (a: b): [a]: [a] =
    #with b NonFunction
    function: list:

    todo "implemented natively"


indexBy as (a: key): [a]: Dict key a =
    #with key NonFunction
    getIndex: list:
    List.foldl (i: Dict.insert (getIndex i) i) list Dict.empty


foldl as (item: state: state): [item]: state: state =
    function: aList: init:
    try aList as
      []:
          init

      h :: tail:
          foldl function tail (function h init)


foldr as (item: state: state): [item]: state: state =
    f: list: init:

    foldrHelper as state: Int: List item: state =
        acc: ctr: ls:
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
                                              if ctr > 500 then
                                                  foldl f (reverse r4) acc
                                              else
                                                  foldrHelper acc (ctr + 1) r4

                                          f a (f b (f c (f d res)))
    foldrHelper init 0 list


length as [a]: Int =
    list:
    foldl (_: a: a + 1) list 0


map as (a: b): [a]: [b] =
    f: list:
    foldr (x: acc: (f x) :: acc) list []


map2 as (a: b: c): [a]: [b]: [c] =
  f:

  rec as [c]: [a]: [b]: [c] =
      accum: ax: bx:

      try ax & bx as
        ahead :: atail & bhead :: btail :
            rec (f ahead bhead :: accum) atail btail
        _:
            reverse accum

  rec []


mapRes as (a: Result e b): [a]: Result e [b] =
    f: list:

    fun = a: acc: Result.map (b: b :: acc) (f a)

    foldlRes fun list [] >> Result.map reverse


foldlRes as (item: accum: Result error accum): [item]: accum: Result error accum =
    f: ls: accum:

    try ls as
        []:
            Ok accum

        h :: t:
            try f h accum as
                Err x: Err x
                Ok newAccum: foldlRes f t newAccum


range as Int: Int: [Int] =
    low: high:

    rec as [Int]: Int: [Int] =
        accum: up:
        if up > low then
            rec (up :: accum) (up - 1)
        else if up == low then
            up :: accum
        else
            accum

    rec [] high


indexedMap as (Int: a: b): [a]: [b] =
    f:

    rec  as [b]: Int: [a]: [b] =
        accum: n: list:
        try list as
            []: reverse accum
            h :: t: rec (f n h :: accum) (n + 1) t

    rec [] 0


append as List a: List a: List a =
  xs: ys:
  try ys as
    []: xs
    _: foldr SPCore.Cons xs ys


concat as List (List a): List a =
  lists:
  foldr append lists []


concatMap as (a: [b]): [a]: [b] =
    f: list:
    concat << map f list


head as [a]: Maybe a =
    list:
    try list as
      []: Nothing
      h :: t: Just h


last as [a]: Maybe a =
    list:
    try list as
        []: Nothing
        h :: []: Just h
        h :: t: last t


take as Int: [a]: [a] =
    takeFast 0


# Shamelessly stolen from https://github.com/elm/core/blob/1.0.5/src/List.elm
takeFast as Int: Int: [a]: [a] =
  ctr: n: list:
  if n < 1 then
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
          if ctr > 1000 then
            cons x (cons y (cons z (cons w (takeTailRec (n - 4) tl))))
          else
            cons x (cons y (cons z (cons w (takeFast (ctr + 1) (n - 4) tl))))

        _ :
          list


takeTailRec as Int: [a]: [a] =
  n: list:
  reverse (takeReverse n list [])


takeReverse as Int: [a]: [a]: [a] =
  n: list: kept:
  if n < 1 then
    kept
  else
    try list as
      []:
        kept

      x :: xs:
        takeReverse (n - 1) xs (SPCore.Cons x kept)


takeWhile as (item: Bool): [item]: [item] =
    test:

    rec as [item]: [item]: [item] =
      accum: list:
      try list as
          []:
            reverse accum

          head :: tail:
              if test head then
                rec (head :: accum) tail
              else
                reverse accum

    rec []


filter as (item: Bool): [item]: [item] =
    f: ls:
    foldr (item: acc: if f item then item :: acc else acc) ls []


filterMap as (a: Maybe b): [a]: [b] =
  f: la:

  update as a: [b]: [b] =
      a: acc:
      try f a as
         Just b: b :: acc
         Nothing: acc

  foldr update la []


mapFirst as (a: Maybe b): List a: Maybe b =
    f: ls:
    try ls as
        []:
            Nothing

        h :: tail:
            r = f h
            if r == Nothing then
                mapFirst f tail
            else
                r


each as [a]: (a: b): None =
    ls: f:
    try ls as
        []:
            None

        h :: tail:
            f h
            each tail f


reverse as [a]: [a] =
    aList:
    foldl SPCore.Cons aList []


repeat as Int: a: [ a ] =
    n: a:

    rec as Int: [a]: [a] =
        c: acc:
        if c > 0 then rec (c - 1) (a :: acc) else acc

    rec n []


drop as Int: [a]: [a] =
    n: ls:

    if n == 0 then
      ls
    else:
      try ls as
          []: []
          h :: tail: drop (n - 1) tail

minimum as [Number]: Maybe Number =
    list:

    try list as
      x :: xs:
        Just (foldl min xs x)

      _:
        Nothing

