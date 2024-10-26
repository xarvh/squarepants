## Shamelessly translated from https://raw.githubusercontent.com/elm/core/1.0.5/src/Dict.elm

var NColor =
    , 'red
    , 'black


var Dict key v =
    , 'node NColor key v (Dict key v) (Dict key v)
    , 'empty


empty as Dict key v with key NonFunction =
    'empty


get as fn key, Dict key v: Maybe v with key NonFunction =
    fn targetKey, dict:
    try dict as

        'empty:
            'nothing

        'node _ key value left right:
            try Basics.compare targetKey key as
                1: get targetKey right
                0: 'just value
                _: get targetKey left


has as fn key, Dict key v: Bool with key NonFunction =
    fn key, dict:
    try get key dict as
        'just _: 'true
        'nothing: 'false


length as fn Dict key v: Int with key NonFunction =
    sizeHelp as fn Int, Dict key v: Int =
        fn n, dict:
        try dict as
            'empty: n
            'node _ _ _ left right: sizeHelp (sizeHelp (n + 1) right) left

    sizeHelp 0 __


isEmpty as fn Dict key v: Bool =
    fn dict:
    try dict as
        'empty: 'true
        'node _ _ _ _ _: 'false


insert as fn Dict key v, key, v: Dict key v with key NonFunction =
    fn dict, key, value:
    # Root node as always Black
    try insertHelp key value dict as
        'node 'red k v l r: 'node 'black k v l r
        x: x


insertHelp as fn key, v, Dict key v: Dict key v with key NonFunction =
    fn key, value, dict:
    try dict as

        'empty:
            # New nodes are always red. If it violates the rules, it will be fixed
            # when balancing.
            'node 'red key value 'empty 'empty

        'node nColor nKey nValue nLeft nRight:
            try Basics.compare key nKey as
                1: balance nColor nKey nValue nLeft (insertHelp key value nRight)
                0: 'node nColor nKey value nLeft nRight
                _: balance nColor nKey nValue (insertHelp key value nLeft) nRight


balance as fn NColor, key, v, Dict key v, Dict key v: Dict key v =
    fn color, key, value, left, right:
    try right as

        'node 'red rK rV rLeft rRight:
            try left as
                'node 'red lK lV lLeft lRight: 'node 'red key value ('node 'black lK lV lLeft lRight) ('node 'black rK rV rLeft rRight)
                _: 'node color rK rV ('node 'red key value left rLeft) rRight

        _:
            try left as
                'node 'red lK lV ('node 'red llK llV llLeft llRight) lRight: 'node 'red lK lV ('node 'black llK llV llLeft llRight) ('node 'black key value lRight right)
                _: 'node color key value left right


remove as fn Dict key v, key: Dict key v with key NonFunction =
    fn dict, key:
    # Root node as always Black
    try removeHelp key dict as
        'node 'red k v l r: 'node 'black k v l r
        x: x


removeHelp as fn key, Dict key v: Dict key v with key NonFunction =
    fn targetKey, dict:
    try dict as

        'empty:
            'empty

        'node color key value left right:
            if Basics.compare targetKey key == 0 - 1 then
                try left as

                    'node 'black _ _ lLeft _:
                        try lLeft as

                            'node 'red _ _ _ _:
                                'node color key value (removeHelp targetKey left) right

                            _:
                                try moveRedLeft dict as
                                    'node nColor nKey nValue nLeft nRight: balance nColor nKey nValue (removeHelp targetKey nLeft) nRight
                                    'empty: 'empty

                    _:
                        'node color key value (removeHelp targetKey left) right
            else
                removeHelpEQGT targetKey (removeHelpPrepEQGT targetKey dict color key value left right)


removeHelpPrepEQGT as fn key, Dict key v, NColor, key, v, Dict key v, Dict key v: Dict key v with key NonFunction =
    fn targetKey, dict, color, key, value, left, right:
    try left as

        'node 'red lK lV lLeft lRight:
            'node color lK lV lLeft ('node 'red key value lRight right)

        _:
            try right as
                'node 'black _ _ ('node 'black _ _ _ _) _: moveRedRight dict
                'node 'black _ _ 'empty _: moveRedRight dict
                _: dict


removeHelpEQGT as fn key, Dict key v: Dict key v with key NonFunction =
    fn targetKey, dict:
    try dict as

        'node color key value left right:
            if targetKey == key then
                try getMin right as
                    'node _ minKey minValue _ _: balance color minKey minValue left (removeMin right)
                    'empty: 'empty
            else
                balance color key value left (removeHelp targetKey right)

        'empty:
            'empty


getMin as fn Dict key v: Dict key v =
    fn dict:
    try dict as

        'node _ _ _ left _:
            try left as
                'node _ _ _ _ _: getMin left
                _: dict

        _:
            dict


removeMin as fn Dict key v: Dict key v =
    fn dict:
    try dict as

        'node color key value left right:
            try left as

                'node lColor _ _ lLeft _:
                    try lColor as

                        'black:
                            try lLeft as

                                'node 'red _ _ _ _:
                                    'node color key value (removeMin left) right

                                _:
                                    try moveRedLeft dict as
                                        'node nColor nKey nValue nLeft nRight: balance nColor nKey nValue (removeMin nLeft) nRight
                                        'empty: 'empty

                        _:
                            'node color key value (removeMin left) right

                _:
                    'empty

        _:
            'empty


moveRedLeft as fn Dict key v: Dict key v =
    fn dict:
    try dict as

        'node clr k v ('node lClr lK lV lLeft lRight) ('node rClr rK rV ('node 'red rlK rlV rlL rlR) rRight):
            'node 'red rlK rlV ('node 'black k v ('node 'red lK lV lLeft lRight) rlL) ('node 'black rK rV rlR rRight)

        'node clr k v ('node lClr lK lV lLeft lRight) ('node rClr rK rV rLeft rRight):
            try clr as
                'black: 'node 'black k v ('node 'red lK lV lLeft lRight) ('node 'red rK rV rLeft rRight)
                'red: 'node 'black k v ('node 'red lK lV lLeft lRight) ('node 'red rK rV rLeft rRight)

        _:
            dict


moveRedRight as fn Dict key v: Dict key v =
    fn dict:
    try dict as

        'node clr k v ('node lClr lK lV ('node 'red llK llV llLeft llRight) lRight) ('node rClr rK rV rLeft rRight):
            'node 'red lK lV ('node 'black llK llV llLeft llRight) ('node 'black k v lRight ('node 'red rK rV rLeft rRight))

        'node clr k v ('node lClr lK lV lLeft lRight) ('node rClr rK rV rLeft rRight):
            try clr as
                'black: 'node 'black k v ('node 'red lK lV lLeft lRight) ('node 'red rK rV rLeft rRight)
                'red: 'node 'black k v ('node 'red lK lV lLeft lRight) ('node 'red rK rV rLeft rRight)

        _:
            dict


update as fn key, (fn Maybe v: Maybe v), Dict key v: Dict key v with key NonFunction =
    fn targetKey, alter, dictionary:
    try alter (get targetKey dictionary) as
        'just value: insert dictionary targetKey value
        'nothing: remove dictionary targetKey


ofOne as fn key, v: Dict key v with key NonFunction =
    fn key, value:
    # Root node as always Black
    'node 'black key value 'empty 'empty


# COMBINE

join as fn Dict key v, Dict key v: Dict key v with key NonFunction =
    for __ __ insert


intersect as fn Dict key a, Dict key b: Dict key a with key NonFunction =
    fn t1, t2:
    filterWithKey (fn k, _: has k t2) t1


diff as fn Dict key a, Dict key b: Dict key a with key NonFunction =
    fn t1, t2:
    for t1 t2 (fn t, k, v: remove t k)


merge as fn (fn key, a, res: res), (fn key, a, b, res: res), (fn key, b, res: res), Dict key a, Dict key b, res: res with key NonFunction =
    fn leftStep, bothStep, rightStep, leftDict, rightDict, initialResult:
    #
    stepState as fn key, b, [ key & a ] & res: [ key & a ] & res =
        fn rKey, rValue, q:
        list & res =
            q

        try list as

            []:
                list & rightStep rKey rValue res

            [ lKey & lValue, rest... ]:
                try Basics.compare lKey rKey as
                    1: list & rightStep rKey rValue res
                    0: rest & bothStep lKey lValue rValue res
                    _: stepState rKey rValue (rest & leftStep lKey lValue res)

    (leftovers as [ key & a ]) & (intermediateResult as res) =
        for (toList leftDict & initialResult) rightDict (fn a, k, v: stepState k v a)

    List.for intermediateResult leftovers (fn res, key & a: leftStep key a res)


onlyBothOnly as fn Dict key a, Dict key b: Dict key a & Dict key (a & b) & Dict key b =
    fn da, db:
    onAOnly =
        fn key, a, aOnly & both & bOnly:
        insert aOnly key a & both & bOnly

    onBOnly =
        fn key, b, aOnly & both & bOnly:
        aOnly & both & insert bOnly key b

    onBoth =
        fn key, a, b, aOnly & both & bOnly:
        aOnly & insert both key (a & b) & bOnly

    merge onAOnly onBoth onBOnly da db (empty & empty & empty)


# TRANSFORM

map as fn Dict k a, (fn a: b): Dict k b =
    fn dict, func:
    try dict as
        'empty: 'empty
        'node color key value left right: 'node color key (func value) (map left func) (map right func)


mapWithKey as fn Dict k a, (fn k, a: b): Dict k b =
    fn dict, func:
    try dict as
        'empty: 'empty
        'node color key value left right: 'node color key (func key value) (mapWithKey left func) (mapWithKey right func)


mapKeys as fn (fn k: j), Dict k a: Dict j a =
    fn func, dict:
    for Dict.empty dict (fn d, k, v: insert d (func k) v)


each as fn Dict k v, (fn k, v: None): None =
    fn dict, func:
    try dict as

        'empty:
            'none

        'node _ key value left right:
            func key value

            each left func

            each right func


for as fn b, Dict k v, (fn b, k, v: b): b =
    fn acc, dict, func:
    try dict as
        'empty: acc
        'node _ key value left right: for (func (for acc left func) key value) right func


forRes as fn b, Dict k v, (fn k, v, b: Result e b): Result e b =
    fn acc, dict, func:
    try dict as

        'empty:
            'ok acc

        'node _ key value left right:
            forRes acc left func
            >> Result.onOk fn l:
            func key value l
            >> Result.onOk fn f:
            forRes f right func


forReversed as fn b, Dict k v, (fn k, v, b: b): b =
    fn acc, t, func:
    try t as
        'empty: acc
        'node _ key value left right: forReversed (func key value (forReversed acc right func)) left func


filter as fn (fn v: Bool), Dict key v: Dict key v with key NonFunction =
    fn isGood, dict:
    for empty dict (fn d, k, v: if isGood v then insert d k v else d)


filterWithKey as fn (fn key, v: Bool), Dict key v: Dict key v with key NonFunction =
    fn isGood, dict:
    for empty dict (fn d, k, v: if isGood k v then insert d k v else d)


partition as fn (fn key, v: Bool), Dict key v: Dict key v & Dict key v with key NonFunction =
    fn isGood, dict:
    add =
        fn key, value, t:
        t1 & t2 =
            t

        if isGood key value then
            insert t1 key value & t2
        else
            t1 & insert t2 key value

    for (empty & empty) dict add


# QUERY?

any as fn (fn k, v: Bool), Dict k v: Bool =
    fn f, dict:
    try dict as

        'node color key v left right:
            if f key v then
                'true
            else
                any f left or any f right

        'empty:
            'false


# LISTS

keys as fn Dict k v: [ k ] =
    forReversed [] __ (fn key, value, keyList: [ key, keyList... ])


values as fn Dict k v: [ v ] =
    forReversed [] __ (fn key, value, valueList: [ value, valueList... ])


toList as fn Dict k v: [ k & v ] =
    f as fn k, v, [ k & v ]: [ k & v ] =
        fn key, value, list:
        [ key & value, list... ]

    forReversed [] __ f


fromList as fn [ key & v ]: Dict key v with key NonFunction =
    List.for empty __ (fn dict, keyAndValue: insert dict keyAndValue.first keyAndValue.second)
