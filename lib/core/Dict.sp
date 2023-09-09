## Shamelessly translated from https://raw.githubusercontent.com/elm/core/1.0.5/src/Dict.elm

var NColor =
    , 'red
    , 'black


var Dict key v =
    , 'RBNode_elm_builtin NColor key v (Dict key v) (Dict key v)
    , 'RBEmpty_elm_builtin


empty as Dict key v with key NonFunction =
    'RBEmpty_elm_builtin


get as fn key, Dict key v: Maybe v with key NonFunction =
    fn targetKey, dict:
    try dict as

        'RBEmpty_elm_builtin:
            'nothing

        'RBNode_elm_builtin _ key value left right:
            try Basics.compare targetKey key as
                1: get targetKey right
                0: 'just value
                _: get targetKey left


member as fn key, Dict key v: Bool with key NonFunction =
    fn key, dict:
    try get key dict as
        'just _: 'true
        'nothing: 'false


size as fn Dict key v: Int with key NonFunction =
    sizeHelp as fn Int, Dict key v: Int =
        fn n, dict:
        try dict as
            'RBEmpty_elm_builtin: n
            'RBNode_elm_builtin _ _ _ left right: sizeHelp (sizeHelp (n + 1) right) left

    sizeHelp 0 __


isEmpty as fn Dict key v: Bool =
    fn dict:
    try dict as
        'RBEmpty_elm_builtin: 'true
        'RBNode_elm_builtin _ _ _ _ _: 'false


insert as fn key, v, Dict key v: Dict key v with key NonFunction =
    fn key, value, dict:
    # Root node as always Black
    try insertHelp key value dict as
        'RBNode_elm_builtin 'red k v l r: 'RBNode_elm_builtin 'black k v l r
        x: x


insertHelp as fn key, v, Dict key v: Dict key v with key NonFunction =
    fn key, value, dict:
    try dict as

        'RBEmpty_elm_builtin:
            # New nodes are always red. If it violates the rules, it will be fixed
            # when balancing.
            'RBNode_elm_builtin 'red key value 'RBEmpty_elm_builtin 'RBEmpty_elm_builtin

        'RBNode_elm_builtin nColor nKey nValue nLeft nRight:
            try Basics.compare key nKey as
                1: balance nColor nKey nValue nLeft (insertHelp key value nRight)
                0: 'RBNode_elm_builtin nColor nKey value nLeft nRight
                _: balance nColor nKey nValue (insertHelp key value nLeft) nRight


balance as fn NColor, key, v, Dict key v, Dict key v: Dict key v =
    fn color, key, value, left, right:
    try right as

        'RBNode_elm_builtin 'red rK rV rLeft rRight:
            try left as
                'RBNode_elm_builtin 'red lK lV lLeft lRight: 'RBNode_elm_builtin 'red key value ('RBNode_elm_builtin 'black lK lV lLeft lRight) ('RBNode_elm_builtin 'black rK rV rLeft rRight)
                _: 'RBNode_elm_builtin color rK rV ('RBNode_elm_builtin 'red key value left rLeft) rRight

        _:
            try left as
                'RBNode_elm_builtin 'red lK lV ('RBNode_elm_builtin 'red llK llV llLeft llRight) lRight: 'RBNode_elm_builtin 'red lK lV ('RBNode_elm_builtin 'black llK llV llLeft llRight) ('RBNode_elm_builtin 'black key value lRight right)
                _: 'RBNode_elm_builtin color key value left right


remove as fn key, Dict key v: Dict key v with key NonFunction =
    fn key, dict:
    # Root node as always Black
    try removeHelp key dict as
        'RBNode_elm_builtin 'red k v l r: 'RBNode_elm_builtin 'black k v l r
        x: x


removeHelp as fn key, Dict key v: Dict key v with key NonFunction =
    fn targetKey, dict:
    try dict as

        'RBEmpty_elm_builtin:
            'RBEmpty_elm_builtin

        'RBNode_elm_builtin color key value left right:
            if Basics.compare targetKey key == 0 - 1 then
                try left as

                    'RBNode_elm_builtin 'black _ _ lLeft _:
                        try lLeft as

                            'RBNode_elm_builtin 'red _ _ _ _:
                                'RBNode_elm_builtin color key value (removeHelp targetKey left) right

                            _:
                                try moveRedLeft dict as
                                    'RBNode_elm_builtin nColor nKey nValue nLeft nRight: balance nColor nKey nValue (removeHelp targetKey nLeft) nRight
                                    'RBEmpty_elm_builtin: 'RBEmpty_elm_builtin

                    _:
                        'RBNode_elm_builtin color key value (removeHelp targetKey left) right
            else
                removeHelpEQGT targetKey (removeHelpPrepEQGT targetKey dict color key value left right)


removeHelpPrepEQGT as fn key, Dict key v, NColor, key, v, Dict key v, Dict key v: Dict key v with key NonFunction =
    fn targetKey, dict, color, key, value, left, right:
    try left as

        'RBNode_elm_builtin 'red lK lV lLeft lRight:
            'RBNode_elm_builtin color lK lV lLeft ('RBNode_elm_builtin 'red key value lRight right)

        _:
            try right as
                'RBNode_elm_builtin 'black _ _ ('RBNode_elm_builtin 'black _ _ _ _) _: moveRedRight dict
                'RBNode_elm_builtin 'black _ _ 'RBEmpty_elm_builtin _: moveRedRight dict
                _: dict


removeHelpEQGT as fn key, Dict key v: Dict key v with key NonFunction =
    fn targetKey, dict:
    try dict as

        'RBNode_elm_builtin color key value left right:
            if targetKey == key then
                try getMin right as
                    'RBNode_elm_builtin _ minKey minValue _ _: balance color minKey minValue left (removeMin right)
                    'RBEmpty_elm_builtin: 'RBEmpty_elm_builtin
            else
                balance color key value left (removeHelp targetKey right)

        'RBEmpty_elm_builtin:
            'RBEmpty_elm_builtin


getMin as fn Dict key v: Dict key v =
    fn dict:
    try dict as

        'RBNode_elm_builtin _ _ _ left _:
            try left as
                'RBNode_elm_builtin _ _ _ _ _: getMin left
                _: dict

        _:
            dict


removeMin as fn Dict key v: Dict key v =
    fn dict:
    try dict as

        'RBNode_elm_builtin color key value left right:
            try left as

                'RBNode_elm_builtin lColor _ _ lLeft _:
                    try lColor as

                        'black:
                            try lLeft as

                                'RBNode_elm_builtin 'red _ _ _ _:
                                    'RBNode_elm_builtin color key value (removeMin left) right

                                _:
                                    try moveRedLeft dict as
                                        'RBNode_elm_builtin nColor nKey nValue nLeft nRight: balance nColor nKey nValue (removeMin nLeft) nRight
                                        'RBEmpty_elm_builtin: 'RBEmpty_elm_builtin

                        _:
                            'RBNode_elm_builtin color key value (removeMin left) right

                _:
                    'RBEmpty_elm_builtin

        _:
            'RBEmpty_elm_builtin


moveRedLeft as fn Dict key v: Dict key v =
    fn dict:
    try dict as

        'RBNode_elm_builtin clr k v ('RBNode_elm_builtin lClr lK lV lLeft lRight) ('RBNode_elm_builtin rClr rK rV ('RBNode_elm_builtin 'red rlK rlV rlL rlR) rRight):
            'RBNode_elm_builtin 'red rlK rlV ('RBNode_elm_builtin 'black k v ('RBNode_elm_builtin 'red lK lV lLeft lRight) rlL) ('RBNode_elm_builtin 'black rK rV rlR rRight)

        'RBNode_elm_builtin clr k v ('RBNode_elm_builtin lClr lK lV lLeft lRight) ('RBNode_elm_builtin rClr rK rV rLeft rRight):
            try clr as
                'black: 'RBNode_elm_builtin 'black k v ('RBNode_elm_builtin 'red lK lV lLeft lRight) ('RBNode_elm_builtin 'red rK rV rLeft rRight)
                'red: 'RBNode_elm_builtin 'black k v ('RBNode_elm_builtin 'red lK lV lLeft lRight) ('RBNode_elm_builtin 'red rK rV rLeft rRight)

        _:
            dict


moveRedRight as fn Dict key v: Dict key v =
    fn dict:
    try dict as

        'RBNode_elm_builtin clr k v ('RBNode_elm_builtin lClr lK lV ('RBNode_elm_builtin 'red llK llV llLeft llRight) lRight) ('RBNode_elm_builtin rClr rK rV rLeft rRight):
            'RBNode_elm_builtin 'red lK lV ('RBNode_elm_builtin 'black llK llV llLeft llRight) ('RBNode_elm_builtin 'black k v lRight ('RBNode_elm_builtin 'red rK rV rLeft rRight))

        'RBNode_elm_builtin clr k v ('RBNode_elm_builtin lClr lK lV lLeft lRight) ('RBNode_elm_builtin rClr rK rV rLeft rRight):
            try clr as
                'black: 'RBNode_elm_builtin 'black k v ('RBNode_elm_builtin 'red lK lV lLeft lRight) ('RBNode_elm_builtin 'red rK rV rLeft rRight)
                'red: 'RBNode_elm_builtin 'black k v ('RBNode_elm_builtin 'red lK lV lLeft lRight) ('RBNode_elm_builtin 'red rK rV rLeft rRight)

        _:
            dict


update as fn key, fn Maybe v: Maybe v, Dict key v: Dict key v with key NonFunction =
    fn targetKey, alter, dictionary:
    try alter (get targetKey dictionary) as
        'just value: insert targetKey value dictionary
        'nothing: remove targetKey dictionary


ofOne as fn key, v: Dict key v with key NonFunction =
    fn key, value:
    # Root node as always Black
    'RBNode_elm_builtin 'black key value 'RBEmpty_elm_builtin 'RBEmpty_elm_builtin


# COMBINE

join as fn Dict key v, Dict key v: Dict key v with key NonFunction =
    for __ __ insert


intersect as fn Dict key a, Dict key b: Dict key a with key NonFunction =
    fn t1, t2:
    filter (fn k, _: member k t2) t1


diff as fn Dict key a, Dict key b: Dict key a with key NonFunction =
    fn t1, t2:
    for t1 t2 (fn k, v, t: remove k t)


merge as fn fn key, a, res: res, fn key, a, b, res: res, fn key, b, res: res, Dict key a, Dict key b, res: res with key NonFunction =
    fn leftStep, bothStep, rightStep, leftDict, rightDict, initialResult:
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
        for (toList leftDict & initialResult) rightDict stepState

    liftLeftStep as fn key & a, res: res =
        fn t, res:
        k & v =
            t

        leftStep k v res

    List.for intermediateResult leftovers liftLeftStep


onlyBothOnly as fn Dict key a, Dict key b: Dict key a & Dict key (a & b) & Dict key b =
    fn da, db:
    onAOnly =
        fn key, a, aOnly & both & bOnly:
        insert key a aOnly & both & bOnly

    onBOnly =
        fn key, b, aOnly & both & bOnly:
        aOnly & both & insert key b bOnly

    onBoth =
        fn key, a, b, aOnly & both & bOnly:
        aOnly & insert key (a & b) both & bOnly

    merge onAOnly onBoth onBOnly da db (empty & empty & empty)


# TRANSFORM

map as fn fn k, a: b, Dict k a: Dict k b =
    fn func, dict:
    try dict as
        'RBEmpty_elm_builtin: 'RBEmpty_elm_builtin
        'RBNode_elm_builtin color key value left right: 'RBNode_elm_builtin color key (func key value) (map func left) (map func right)


mapRes as fn fn k, a: Result e b, Dict k a: Result e (Dict k b) =
    fn func, dict:
    try dict as

        'RBEmpty_elm_builtin:
            'ok 'RBEmpty_elm_builtin

        'RBNode_elm_builtin color key value left right:
            func key value
            >> Result.onOk fn one:
            mapRes func left
            >> Result.onOk fn two:
            mapRes func right
            >> Result.onOk fn three:
            'ok << 'RBNode_elm_builtin color key one two three


mapKeys as fn fn k: j, Dict k a: Dict j a =
    fn func, dict:
    for Dict.empty dict (fn k, v, d: Dict.insert (func k) v d)


each as fn Dict k v, fn k, v: None: None =
    fn dict, func:
    try dict as

        'RBEmpty_elm_builtin:
            'none

        'RBNode_elm_builtin _ key value left right:
            func key value

            each left func

            each right func


for as fn b, Dict k v, fn k, v, b: b: b =
    fn acc, dict, func:
    try dict as
        'RBEmpty_elm_builtin: acc
        'RBNode_elm_builtin _ key value left right: for (func key value (for acc left func)) right func


forRes as fn b, Dict k v, fn k, v, b: Result e b: Result e b =
    fn acc, dict, func:
    try dict as

        'RBEmpty_elm_builtin:
            'ok acc

        'RBNode_elm_builtin _ key value left right:
            forRes acc left func
            >> Result.onOk fn l:
            func key value l
            >> Result.onOk fn f:
            forRes f right func


forReversed as fn b, Dict k v, fn k, v, b: b: b =
    fn acc, t, func:
    try t as
        'RBEmpty_elm_builtin: acc
        'RBNode_elm_builtin _ key value left right: forReversed (func key value (forReversed acc right func)) left func


filter as fn fn key, v: Bool, Dict key v: Dict key v with key NonFunction =
    fn isGood, dict:
    for empty dict (fn k, v, d: if isGood k v then insert k v d else d)


partition as fn fn key, v: Bool, Dict key v: Dict key v & Dict key v with key NonFunction =
    fn isGood, dict:
    add =
        fn key, value, t:
        t1 & t2 =
            t

        if isGood key value then
            insert key value t1 & t2
        else
            t1 & insert key value t2

    for (empty & empty) dict add


# QUERY?

any as fn fn k, v: Bool, Dict k v: Bool =
    fn f, dict:
    try dict as

        'RBNode_elm_builtin color key v left right:
            if f key v then
                'true
            else
                any f left or any f right

        'RBEmpty_elm_builtin:
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
    List.for empty __ (fn keyAndValue, dict: insert keyAndValue.first keyAndValue.second dict)
