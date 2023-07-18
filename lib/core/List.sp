

any as fn (fn a: Bool), [a]: Bool =
    fn fun, list:

    try list as
        , []: False
        , [h, ...t]: if fun h then True else any fun t


all as fn (fn a: Bool), [a]: Bool =
    fn fun, list:
    try list as
        , []: True
        , [h, ...t]:
            if fun h then
                all fun t
            else
                False


find as fn (fn a: Bool), [a]: Maybe a =
    fn test, list:
    try list as
        , []: Nothing
        , [h, ...t]:
            if test h then
                Just h
            else
                find test t


findMap as fn (fn a: Maybe b), [a]: Maybe b =
    fn f, list:
    try list as
        , []: Nothing
        , [h, ...t]:
            try f h as
                , Just r: Just r
                , Nothing: findMap f t


member as fn a, [a]: Bool = # TODO with a NonFunction =
    fn a, list:
    try list as
        , []: False

        , h :: t:
            if a == h then
                True
            else
                member a t


sort as fn [a]: [a] = # TODO with a NonFunction =
    sortBy identity __


sortBy as fn (fn a: b), [a]: [a] = # TODO with b NonFunction =
    fn function, list:

    todo "implemented natively"


indexBy as fn (fn a: key), [a]: Dict key a with key NonFunction =
    fn getIndex, list:
    for Dict.empty list fn i, a: Dict.insert (getIndex i) i a


for as fn state, [item], (fn item, state: state): state =
    fn init, aList, function:
    try aList as
      , []:
          init

      , h :: tail:
          for (function h init) tail function


for2 as fn state, [a], [b], (fn a, b, state: state): state =
    fn init, aList, bList, function:
    try aList & bList as
      , [] & _:
          init

      , _ & []:
          init

      , [headA, ...tailA] & [headB, ...tailB]:
          for2 (function headA headB init) tailA tailB function


indexedFor as fn state, [item], (fn Int, item, state: state): state =
    fn init, aList, function:
    for (0 & init) aList (fn item, (index & accum): index + 1 & function index item accum)
    >> Tuple.second


indexedFor2 as fn state, [a], [b], (fn Int, a, b, state: state): state =
    fn init, aList, bList, function:
    for2 (0 & init) aList bList (fn a, b, (index & accum): index + 1 & function index a b accum)
    >> Tuple.second


forReversed as fn state, [item], (fn item, state: state): state =
    fn init, list, f:

    foldrHelper as fn state, Int, [item]: state =
        fn acc, ctr, ls:
        try ls as
            , []:
                acc

            , a :: r1:
                try r1 as
                    , []:
                        f a acc

                    , b :: r2:
                        try r2 as
                            , []:
                                f a (f b acc)

                            , c :: r3:
                                try r3 as
                                    , []:
                                        f a (f b (f c acc))

                                    , d :: r4:
                                          res =
                                              if ctr > 500 then
                                                  for acc (reverse r4) f
                                              else
                                                  foldrHelper acc (ctr + 1) r4

                                          f a (f b (f c (f d res)))
    foldrHelper init 0 list


forReversed2 as fn state, [a], [b], (fn a, b, state: state): state =
    fn init, listA, listB, f:

    # TODO optimize
    for2 init (reverse listA) (reverse listB) f


length as fn [a]: Int =
    fn list:
    for 0 list (fn _, a: a + 1)


map as fn (fn a: b), [a]: [b] =
    fn f, list:
    forReversed [] list (fn x, acc: (f x) :: acc)


map2 as fn (fn a, b: c), [a], [b]: [c] =
  fn f, aa, bb:

  rec as fn [c], [a], [b]: [c] =
      fn accum, ax, bx:

      try ax & bx as
        , [ahead, ...atail] & [bhead, ...btail]:
            rec [f ahead bhead, ...accum] atail btail
        , _:
            reverse accum

  rec [] aa bb


mapRes as fn (fn a: Result e b), [a]: Result e [b] =
    fn f, list:

    fun = fn a, acc: Result.map (fn b: [b, ...acc]) (f a)

    forRes [] list fun
    >> Result.map reverse __


forRes as fn accum, [item], (fn item, accum: Result error accum): Result error accum =
    fn accum, ls, f:

    try ls as
        , []:
            Ok accum

        , h :: t:
            try f h accum as
                , Err x: Err x
                , Ok newAccum: forRes newAccum t f


range as fn Int, Int: [Int] =
    fn low, high:

    rec as fn [Int], Int: [Int] =
        fn accum, up:
        if up > low then
            rec (up :: accum) (up - 1)
        else if up == low then
            up :: accum
        else
            accum

    rec [] high


indexedMap as fn (fn Int, a: b), [a]: [b] =
    fn f, aa:

    rec as fn [b], Int, [a]: [b] =
        fn accum, n, list:
        try list as
            , []: reverse accum
            , h :: t: rec (f n h :: accum) (n + 1) t

    rec [] 0 aa


indexedMap2 as fn (fn Int, a, b: c), [a], [b]: [c] =
    fn f, aaa, bbb:

    rec as fn [b], Int, [a], [b]: [c] =
        fn accum, n, aa, bb:
        try aa & bb as
            , (a :: at) & (b :: bt): rec (f n a b :: accum) (n + 1) at bt
            , _: reverse accum

    rec [] 0 aaa bbb


append as fn [a], [a]: [a] =
  fn xs, ys:
  try ys as
    , []: xs
    , _: forReversed ys xs Core.Cons


concat as fn [[a]]: [a] =
  fn lists:
  forReversed [] lists append


concatMap as fn (fn a: [b]), [a]: [b] =
    fn f, list:
    concat << map f list


partition as fn (fn item: Bool), [item]: [item] & [item] =
    fn f, ls:

    [] & [] >> forReversed __ ls fn item, (true & false):
        if f item then
          (item :: true) & false
        else
          true & (item :: false)


head as fn [a]: Maybe a =
    fn list:
    try list as
      , []: Nothing
      , h :: t: Just h


last as fn [a]: Maybe a =
    fn list:
    try list as
        , []: Nothing
        , h :: []: Just h
        , h :: t: last t


take as fn Int, [a]: [a] =
    takeFast 0 __ __


# Shamelessly stolen from https://github.com/elm/core/blob/1.0.5/src/List.elm
takeFast as fn Int, Int, [a]: [a] =
  fn ctr, n, list:
  if n < 1 then
    []
  else
    try n & list as
        , _ & []:
          list

        , 1 & [x, ..._]:
          [ x ]

        , 2 & [x, y, ..._]:
          [ x, y ]

        , 3 & [x, y, z, ..._]:
          [ x, y, z ]

        , _ & [x, y, z, w, ...tl]:
          cons = Core.Cons
          if ctr > 1000 then
            cons x (cons y (cons z (cons w (takeTailRec (n - 4) tl))))
          else
            cons x (cons y (cons z (cons w (takeFast (ctr + 1) (n - 4) tl))))

        , _ :
          list


takeTailRec as fn Int, [a]: [a] =
  fn n, list:
  reverse (takeReverse n list [])


takeReverse as fn Int, [a], [a]: [a] =
  fn n, list, kept:
  if n < 1 then
    kept
  else
    try list as
      , []:
        kept

      , x :: xs:
        takeReverse (n - 1) xs (Core.Cons x kept)


takeWhile as fn (fn item: Bool), [item]: [item] =
    fn test, its:

    rec as fn [item], [item]: [item] =
      fn accum, list:
      try list as
          , []:
            reverse accum

          , head :: tail:
              if test head then
                rec (head :: accum) tail
              else
                reverse accum

    rec [] its


filter as fn (fn item: Bool), [item]: [item] =
    fn f, ls:
    forReversed [] ls (fn item, acc: if f item then item :: acc else acc)


filterMap as fn (fn a: Maybe b), [a]: [b] =
  fn f, la:

  update as fn a, [b]: [b] =
      fn a, acc:
      try f a as
         , Just b: b :: acc
         , Nothing: acc

  forReversed [] la update


mapFirst as fn (fn a: Maybe b), [a]: Maybe b =
    fn f, ls:
    try ls as
        , []:
            Nothing

        , h :: tail:
            r = f h
            try r as
                , Nothing: mapFirst f tail
                , _: r


each as fn [a], (fn a: b): None =
    fn ls, f:
    try ls as
        , []:
            None

        , h :: tail:
            f h
            each tail f


indexedEach2 as fn [a], [b], (fn Int, a, b: None): None =

    rec as fn Int, [a], [b], (fn Int, a, b: None): None =
        fn index, aa, bb, f:

        try aa & bb as
            , (a :: at) & (b :: bt):
                f index a b
                list_eachWithIndex2 (index + 1) at bt f
            , _:
                None

    rec 0 __ __ __




reverse as fn [a]: [a] =
    fn aList:
    for [] aList Core.Cons


repeat as fn Int, a: [ a ] =
    fn n, a:

    rec as fn Int, [a]: [a] =
        fn c, acc:
        if c > 0 then rec (c - 1) (a :: acc) else acc

    rec n []


drop as fn Int, [a]: [a] =
    fn n, ls:

    if n == 0 then
      ls
    else
      try ls as
          , []: []
          , h :: tail: drop (n - 1) tail


minimum as fn [Number]: Maybe Number =
    fn list:

    try list as
      , x :: xs:
        Just (for x xs min)

      , _:
        Nothing


maximum as fn [Number]: Maybe Number =
    fn list:

    try list as
      , x :: xs:
        Just (for x xs max)

      , _:
        Nothing


circularPairs as fn [a]: [a & a] =
    fn list:

    rec =
        fn zero, tt, acc:
            try tt as
                , []: acc
                , [first, ...tail]:
                    try tail as
                        , []: [zero & first, ...acc]
                        , [second, ...moar]: rec zero tail [second & first, ...acc]

    try list as
        , []: []
        , [head, ...tail]: rec head list []


intersperse as fn a, [a], [a]: [a] =
    fn separator, items, acc:

    try items as
        , []:
            List.reverse acc

        , [last]:
            List.reverse (last :: acc)

        , head :: tail:
            intersperse separator tail (separator :: head :: acc)

