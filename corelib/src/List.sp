any as fn [ a ], (fn a: Bool): Bool =
    fn list, fun:
    try list as
        []: 'false
        [ h, t... ]: if fun h then 'true else any t fun


all as fn [ a ], (fn a: Bool): Bool =
    fn list, fun:
    try list as

        []:
            'true

        [ h, t... ]:
            if fun h then
                all t fun
            else
                'false


find as fn [ a ], (fn a: Bool): Maybe a =
    fn list, test:
    try list as

        []:
            'nothing

        [ h, t... ]:
            if test h then
                'just h
            else
                find t test


findMap as fn [ a ], (fn a: Maybe b): Maybe b =
    fn list, f:
    try list as

        []:
            'nothing

        [ h, t... ]:
            try f h as
                'just r: 'just r
                'nothing: findMap t f


contains as fn a, [ a ]: Bool =
    fn a, list:
    try list as

        []:
            'false

        h :: t:
            if a == h then
                'true
            else
                contains a t


# TODO flip arguments, rename to `sort`
sortBy as fn (fn a: b), [ a ]: [ a ] with b NonFunction =
    this_is_sp_native


indexBy as fn [ a ], (fn a: key): Dict key a with key NonFunction =
    fn list, getIndex:
    for Dict.empty list (fn a, i: Dict.insert a (getIndex i) i)


for as fn state, [ item ], (fn state, item: state): state =
    fn init, aList, function:
    try aList as
        []: init
        [ h, tail... ]: for (function init h) tail function


for2 as fn state, [ a ], [ b ], (fn state, a, b: state): state =
    fn init, aList, bList, function:
    try aList & bList as
        [] & _: init
        _ & []: init
        [ headA, tailA... ] & [ headB, tailB... ]: for2 (function init headA headB) tailA tailB function


forWithIndex as fn state, [ item ], (fn state, Int, item: state): state =
    fn init, aList, function:
    for (0 & init) aList (fn index & accum, item: index + 1 & function accum index item) >> Tuple.second


for2WithIndex as fn state, [ a ], [ b ], (fn state, Int, a, b: state): state =
    fn init, aList, bList, function:
    for2 (0 & init) aList bList (fn index & accum, a, b: index + 1 & function accum index a b) >> Tuple.second


forReversed as fn state, [ item ], (fn state, item: state): state =
    fn init, list, f:
    foldrHelper as fn state, Int, [ item ]: state =
        fn acc, ctr, ls:
        try ls as

            []:
                acc

            a :: r1:
                try r1 as

                    []:
                        f acc a

                    b :: r2:
                        try r2 as

                            []:
                                f (f acc b) a

                            c :: r3:
                                try r3 as

                                    []:
                                        f (f (f acc c) b) a

                                    d :: r4:
                                        res =
                                            if ctr > 500 then
                                                for acc (reverse r4) f
                                            else
                                                foldrHelper acc (ctr + 1) r4

                                        f (f (f (f res d) c) b) a

    foldrHelper init 0 list


length as fn [ a ]: Int =
    fn list:
    for 0 list (fn a, _: a + 1)


map as fn [ a ], (fn a: b): [ b ] =
    fn list, f:
    forReversed [] list (fn acc, x: f x :: acc)


map2 as fn [ a ], [ b ], (fn a, b: c): [ c ] =
    fn aa, bb, f:
    rec as fn [ c ], [ a ], [ b ]: [ c ] =
        fn accum, ax, bx:
        try ax & bx as
            [ ahead, atail... ] & [ bhead, btail... ]: rec [ f ahead bhead, accum... ] atail btail
            _: reverse accum

    rec [] aa bb


mapRes as fn [ a ], (fn a: Result e b): Result e [ b ] =
    fn list, f:
    fun =
        fn acc, a: Result.map (f a) (fn b: [ b, acc... ])

    forRes [] list fun >> Result.map __ reverse


forRes as fn accum, [ item ], (fn accum, item: Result error accum): Result error accum =
    fn accum, ls, f:
    try ls as

        []:
            'ok accum

        h :: t:
            try f accum h as
                'err x: 'err x
                'ok newAccum: forRes newAccum t f


range as fn Int, Int: [ Int ] =
    fn low, high:
    rec as fn [ Int ], Int: [ Int ] =
        fn accum, up:
        if up > low then
            rec (up :: accum) (up - 1)
        else if up == low then
            up :: accum
        else
            accum

    rec [] high


mapWithIndex as fn [ a ], (fn Int, a: b): [ b ] =
    fn aa, f:
    rec as fn [ b ], Int, [ a ]: [ b ] =
        fn accum, n, list:
        try list as
            []: reverse accum
            h :: t: rec (f n h :: accum) (n + 1) t

    rec [] 0 aa


map2WithIndex as fn [ a ], [ b ], (fn Int, a, b: c): [ c ] =
    fn aaa, bbb, f:
    rec as fn [ c ], Int, [ a ], [ b ]: [ c ] =
        fn accum, n, aa, bb:
        try aa & bb as
            (a :: at) & (b :: bt): rec (f n a b :: accum) (n + 1) at bt
            _: reverse accum

    rec [] 0 aaa bbb


append as fn [ a ], [ a ]: [ a ] =
    fn ys, xs:
    try ys as
        []: xs
        _: forReversed ys xs (fn a, b: Core.'cons b a)


concat as fn [ [ a ] ]: [ a ] =
    fn lists:
    forReversed [] lists append


mapConcat as fn [ a ], (fn a: [ b ]): [ b ] =
    fn list, f:
    concat << map list f


partition as fn [ item ], (fn item: Bool): [ item ] & [ item ] =
    fn ls, f:
    [] & []
    >> forReversed __ ls fn true & false, item:
        if f item then
            (item :: true) & false
        else
            true & (item :: false)


head as fn [ a ]: Maybe a =
    fn list:
    try list as
        []: 'nothing
        h :: t: 'just h


last as fn [ a ]: Maybe a =
    fn list:
    try list as
        []: 'nothing
        h :: []: 'just h
        h :: t: last t


# TODO flip params
take as fn Int, [ a ]: [ a ] =
    takeFast 0 __ __


# Shamelessly stolen from https://github.com/elm/core/blob/1.0.5/src/List.elm
takeFast as fn Int, Int, [ a ]: [ a ] =
    fn ctr, n, list:
    if n < 1 then
        []
    else
        try n & list as

            _ & []:
                list

            1 & [ x, _... ]:
                [ x ]

            2 & [ x, y, _... ]:
                [ x, y ]

            3 & [ x, y, z, _... ]:
                [ x, y, z ]

            _ & [ x, y, z, w, tl... ]:
                cons =
                    Core.'cons

                if ctr > 1000 then
                    cons x (cons y (cons z (cons w (takeTailRec (n - 4) tl))))
                else
                    cons x (cons y (cons z (cons w (takeFast (ctr + 1) (n - 4) tl))))

            _:
                list


takeTailRec as fn Int, [ a ]: [ a ] =
    fn n, list:
    reverse (takeReverse n list [])


takeReverse as fn Int, [ a ], [ a ]: [ a ] =
    fn n, list, kept:
    if n < 1 then
        kept
    else
        try list as
            []: kept
            x :: xs: takeReverse (n - 1) xs (Core.'cons x kept)


takeWhile as fn (fn item: Bool), [ item ]: [ item ] =
    fn test, its:
    rec as fn [ item ], [ item ]: [ item ] =
        fn accum, list:
        try list as

            []:
                reverse accum

            h :: tail:
                if test h then
                    rec (h :: accum) tail
                else
                    reverse accum

    rec [] its


filter as fn [ item ], (fn item: Bool): [ item ] =
    fn ls, f:
    forReversed [] ls (fn acc, item: if f item then item :: acc else acc)


filterMap as fn [ a ], (fn a: Maybe b): [ b ] =
    fn la, f:
    update as fn[ b ], a: [ b ] =
        fn acc, a:
        try f a as
            'just b: b :: acc
            'nothing: acc

    forReversed [] la update


mapFirst as fn [ a ], (fn a: Maybe b): Maybe b =
    fn ls, f:
    try ls as

        []:
            'nothing

        h :: tail:
            r =
                f h

            try r as
                'nothing: mapFirst tail f
                _: r


each as fn [ a ], (fn a: b): None =
    fn ls, f:
    try ls as

        []:
            'none

        h :: tail:
            f h

            each tail f


each2WithIndex as fn [ a ], [ b ], (fn Int, a, b: None): None =
    rec as fn Int, [ a ], [ b ], (fn Int, a, b: None): None =
        fn index, aa, bb, f:
        try aa & bb as

            (a :: at) & (b :: bt):
                f index a b

                rec (index + 1) at bt f

            _:
                'none

    rec 0 __ __ __


reverse as fn [ a ]: [ a ] =
    fn aList:
    for [] aList (fn a, b: Core.'cons b a)


repeat as fn Int, a: [ a ] =
    fn n, a:
    rec as fn Int, [ a ]: [ a ] =
        fn c, acc:
        if c > 0 then rec (c - 1) (a :: acc) else acc

    rec n []


drop as fn Int, [ a ]: [ a ] =
    fn n, ls:
    if n == 0 then
        ls
    else
        try ls as
            []: []
            h :: tail: drop (n - 1) tail


minimum as fn [ Number ]: Maybe Number =
    fn list:
    try list as
        x :: xs: 'just (for x xs min)
        _: 'nothing


maximum as fn [ Number ]: Maybe Number =
    fn list:
    try list as
        x :: xs: 'just (for x xs max)
        _: 'nothing


circularPairs as fn [ a ]: [ a & a ] =
    fn list:
    rec =
        fn zero, tt, acc:
        try tt as

            []:
                acc

            [ first, tail... ]:
                try tail as
                    []: [ zero & first, acc... ]
                    [ second, moar... ]: rec zero tail [ second & first, acc... ]

    try list as
        []: []
        [ h, tail... ]: rec h list []


# TODO remove the acc parameter
intersperse as fn a, [ a ]: [ a ] =
    rec as fn a, [ a ], [ a ]: [ a ] =
        fn separator, items, acc:
        try items as
            []: List.reverse acc
            [ last_ ]: List.reverse (last_ :: acc)
            h :: tail: rec separator tail (separator :: h :: acc)

    rec __ __ []


partitionWhile as fn [ item ], (fn item: Bool): [ item ] & [ item ] =
    fn xs, f:
    rec =
        fn acc, rest:
        try rest as

            []:
                xs & []

            h :: tail:
                if f h then
                    rec (h :: acc) tail
                else
                    List.reverse acc & rest

    rec [] xs
