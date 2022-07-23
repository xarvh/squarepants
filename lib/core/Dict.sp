## Shamelessly translated from https://raw.githubusercontent.com/elm/core/1.0.5/src/Dict.elm


union NColor = Red, Black

union Dict key v =
    , RBNode_elm_builtin NColor key v (Dict key v) (Dict key v)
    , RBEmpty_elm_builtin


empty as Dict key v =
  #with key NonFunction
  RBEmpty_elm_builtin


get as key: Dict key v: Maybe v =
  #with key NonFunction
  targetKey: dict:

  try dict as
    RBEmpty_elm_builtin:
      Nothing

    RBNode_elm_builtin _ key value left right:
      try Core.compare targetKey key as
        1:
          get targetKey right

        0:
          Just value

        _:
          get targetKey left




member as key: Dict key v: Bool =
  #with key NonFunction
  key: dict:

  try get key dict as
    Just _:
      True

    Nothing:
      False



size as Dict key v: Int =
  #with key NonFunction

  sizeHelp as Int: Dict key v: Int =
    n: dict:

    try dict as
      RBEmpty_elm_builtin:
        n

      RBNode_elm_builtin _ _ _ left right:
        sizeHelp (sizeHelp (n + 1) right) left

  sizeHelp 0



isEmpty as Dict key v: Bool =
  dict:
  try dict as
    RBEmpty_elm_builtin:
      True

    RBNode_elm_builtin _ _ _ _ _:
      False



insert as key: v: Dict key v: Dict key v =
  #with key NonFunction
  key: value: dict:
  # Root node as always Black
  try insertHelp key value dict as
    RBNode_elm_builtin Red k v l r:
      RBNode_elm_builtin Black k v l r

    x:
      x


insertHelp as key: v: Dict key v: Dict key v =
  #with key NonFunction
  key: value: dict:
  try dict as
    RBEmpty_elm_builtin:
      # New nodes are always red. If it violates the rules, it will be fixed
      # when balancing.
      RBNode_elm_builtin Red key value RBEmpty_elm_builtin RBEmpty_elm_builtin

    RBNode_elm_builtin nColor nKey nValue nLeft nRight:
      try Core.compare key nKey as
        1:
          balance nColor nKey nValue nLeft (insertHelp key value nRight)

        0:
          RBNode_elm_builtin nColor nKey value nLeft nRight

        _:
          balance nColor nKey nValue (insertHelp key value nLeft) nRight



balance as NColor: key: v: Dict key v: Dict key v: Dict key v =
  color: key: value: left: right:
  try right as
    RBNode_elm_builtin Red rK rV rLeft rRight:
      try left as
        RBNode_elm_builtin Red lK lV lLeft lRight:
          RBNode_elm_builtin
            Red
            key
            value
            (RBNode_elm_builtin Black lK lV lLeft lRight)
            (RBNode_elm_builtin Black rK rV rLeft rRight)

        _:
          RBNode_elm_builtin color rK rV (RBNode_elm_builtin Red key value left rLeft) rRight

    _:
      try left as
        RBNode_elm_builtin Red lK lV (RBNode_elm_builtin Red llK llV llLeft llRight) lRight:
          RBNode_elm_builtin
            Red
            lK
            lV
            (RBNode_elm_builtin Black llK llV llLeft llRight)
            (RBNode_elm_builtin Black key value lRight right)

        _:
          RBNode_elm_builtin color key value left right



remove as key: Dict key v: Dict key v =
  #with key NonFunction
  key: dict:
  # Root node as always Black
  try removeHelp key dict as
    RBNode_elm_builtin Red k v l r:
      RBNode_elm_builtin Black k v l r

    x:
      x



removeHelp as key: Dict key v: Dict key v =
  #with key NonFunction
  targetKey: dict:
  try dict as
    RBEmpty_elm_builtin:
      RBEmpty_elm_builtin

    RBNode_elm_builtin color key value left right:
      if Core.compare targetKey key == 0 - 1 then
        try left as
          RBNode_elm_builtin Black _ _ lLeft _:
            try lLeft as
              RBNode_elm_builtin Red _ _ _ _:
                RBNode_elm_builtin color key value (removeHelp targetKey left) right

              _:
                try moveRedLeft dict as
                  RBNode_elm_builtin nColor nKey nValue nLeft nRight:
                    balance nColor nKey nValue (removeHelp targetKey nLeft) nRight

                  RBEmpty_elm_builtin:
                    RBEmpty_elm_builtin

          _:
            RBNode_elm_builtin color key value (removeHelp targetKey left) right
      else
        removeHelpEQGT targetKey (removeHelpPrepEQGT targetKey dict color key value left right)


removeHelpPrepEQGT as key: Dict key v: NColor: key: v: Dict key v: Dict key v: Dict key v =
  #with key NonFunction
  targetKey: dict: color: key: value: left: right:
  try left as
    RBNode_elm_builtin Red lK lV lLeft lRight:
      RBNode_elm_builtin
        color
        lK
        lV
        lLeft
        (RBNode_elm_builtin Red key value lRight right)

    _:
      try right as
        RBNode_elm_builtin Black _ _ (RBNode_elm_builtin Black _ _ _ _) _:
          moveRedRight dict

        RBNode_elm_builtin Black _ _ RBEmpty_elm_builtin _:
          moveRedRight dict

        _:
          dict



removeHelpEQGT as key: Dict key v: Dict key v =
  #with key NonFunction
  targetKey: dict:
  try dict as
    RBNode_elm_builtin color key value left right:
      if targetKey == key then
        try getMin right as
          RBNode_elm_builtin _ minKey minValue _ _:
            balance color minKey minValue left (removeMin right)

          RBEmpty_elm_builtin:
            RBEmpty_elm_builtin
      else
        balance color key value left (removeHelp targetKey right)

    RBEmpty_elm_builtin:
      RBEmpty_elm_builtin


getMin as Dict key v: Dict key v =
  dict:
  try dict as
    RBNode_elm_builtin _ _ _ left _:
      try left as
         RBNode_elm_builtin _ _ _ _ _:
            getMin left
         _:
            dict

    _:
      dict


removeMin as Dict key v: Dict key v =
  dict:
  try dict as
    RBNode_elm_builtin color key value left right:
      try left as
        RBNode_elm_builtin lColor _ _ lLeft _:
          try lColor as
            Black:
              try lLeft as
                RBNode_elm_builtin Red _ _ _ _:
                  RBNode_elm_builtin color key value (removeMin left) right

                _:
                  try moveRedLeft dict as
                    RBNode_elm_builtin nColor nKey nValue nLeft nRight:
                      balance nColor nKey nValue (removeMin nLeft) nRight

                    RBEmpty_elm_builtin:
                      RBEmpty_elm_builtin

            _:
              RBNode_elm_builtin color key value (removeMin left) right

        _:
          RBEmpty_elm_builtin
    _:
      RBEmpty_elm_builtin


moveRedLeft as Dict key v: Dict key v =
  dict:
  try dict as
    RBNode_elm_builtin clr k v (RBNode_elm_builtin lClr lK lV lLeft lRight) (RBNode_elm_builtin rClr rK rV (RBNode_elm_builtin Red rlK rlV rlL rlR) rRight):
      RBNode_elm_builtin
        Red
        rlK
        rlV
        (RBNode_elm_builtin Black k v (RBNode_elm_builtin Red lK lV lLeft lRight) rlL)
        (RBNode_elm_builtin Black rK rV rlR rRight)

    RBNode_elm_builtin clr k v (RBNode_elm_builtin lClr lK lV lLeft lRight) (RBNode_elm_builtin rClr rK rV rLeft rRight):
      try clr as
        Black:
          RBNode_elm_builtin
            Black
            k
            v
            (RBNode_elm_builtin Red lK lV lLeft lRight)
            (RBNode_elm_builtin Red rK rV rLeft rRight)

        Red:
          RBNode_elm_builtin
            Black
            k
            v
            (RBNode_elm_builtin Red lK lV lLeft lRight)
            (RBNode_elm_builtin Red rK rV rLeft rRight)

    _:
      dict


moveRedRight as Dict key v: Dict key v =
  dict:
  try dict as
    RBNode_elm_builtin clr k v (RBNode_elm_builtin lClr lK lV (RBNode_elm_builtin Red llK llV llLeft llRight) lRight) (RBNode_elm_builtin rClr rK rV rLeft rRight):
      RBNode_elm_builtin
        Red
        lK
        lV
        (RBNode_elm_builtin Black llK llV llLeft llRight)
        (RBNode_elm_builtin Black k v lRight (RBNode_elm_builtin Red rK rV rLeft rRight))

    RBNode_elm_builtin clr k v (RBNode_elm_builtin lClr lK lV lLeft lRight) (RBNode_elm_builtin rClr rK rV rLeft rRight):
      try clr as
        Black:
          RBNode_elm_builtin
            Black
            k
            v
            (RBNode_elm_builtin Red lK lV lLeft lRight)
            (RBNode_elm_builtin Red rK rV rLeft rRight)

        Red:
          RBNode_elm_builtin
            Black
            k
            v
            (RBNode_elm_builtin Red lK lV lLeft lRight)
            (RBNode_elm_builtin Red rK rV rLeft rRight)

    _:
      dict



update as key: (Maybe v: Maybe v): Dict key v: Dict key v =
  #with key NonFunction
  targetKey: alter: dictionary:
  try alter (get targetKey dictionary) as
    Just value:
      insert targetKey value dictionary

    Nothing:
      remove targetKey dictionary



singleton as key: v: Dict key v =
  #with key NonFunction
  key: value:
  # Root node as always Black
  RBNode_elm_builtin Black key value RBEmpty_elm_builtin RBEmpty_elm_builtin


# COMBINE



join as Dict key v: Dict key v: Dict key v =
  #with key NonFunction
  a:
  for a insert



intersect as Dict key a: Dict key b: Dict key a =
  #with key NonFunction
  t1: t2:
  filter (k: _: member k t2) t1



diff as Dict key a: Dict key b: Dict key a =
  #with key NonFunction
  t1: t2:
  for t2 (k: v: t: remove k t) t1


merge as (key: a: res: res): (key: a: b: res: res): (key: b: res: res): Dict key a: Dict key b: res: res =
  #with key NonFunction
  leftStep: bothStep: rightStep: leftDict: rightDict: initialResult:

  stepState as key: b: [key & a] & res: [key & a] & res =
    rKey: rValue: q:
    list & res = q
    try list as
      []:
        (list & rightStep rKey rValue res)

      (lKey & lValue) :: rest:
          try Core.compare lKey rKey as
              1: list & rightStep rKey rValue res
              0: rest & bothStep lKey lValue rValue res
              _: stepState rKey rValue (rest & leftStep lKey lValue res)

#        if Basics.compare lKey rKey == 0 - 1:
#          stepState rKey rValue (rest & leftStep lKey lValue res)
#
#        else if Basics.compare lKey rKey == 1:
#          (list & rightStep rKey rValue res)
#
#        else
#          (rest & bothStep lKey lValue rValue res)

  (leftovers as [key & a]) & (intermediateResult as res) =
      for rightDict stepState (toList leftDict & initialResult)

  liftLeftStep as (key & a): res: res =
      t: res:
      (k & v) = t
      leftStep k v res

  List.for leftovers liftLeftStep intermediateResult


# TRANSFORM



map as (k: a: b): Dict k a: Dict k b =
  func: dict:
  try dict as
    RBEmpty_elm_builtin:
      RBEmpty_elm_builtin

    RBNode_elm_builtin color key value left right:
      RBNode_elm_builtin color key (func key value) (map func left) (map func right)


mapRes as (k: a: Result e b): Dict k a: Result e (Dict k b) =
  func: dict:
  try dict as
    RBEmpty_elm_builtin:
        Ok RBEmpty_elm_builtin

    RBNode_elm_builtin color key value left right:
        func key value >> Result.onOk one:
        mapRes func left >> Result.onOk two:
        mapRes func right >> Result.onOk three:
        Ok << RBNode_elm_builtin color key one two three


mapKeys as (k: j): Dict k a: Dict j a =
    func: dict:
    for dict (k: Dict.insert (func k)) Dict.empty


for as Dict k v: (k: v: b: b): b: b =
  dict: func: acc:
  try dict as
    RBEmpty_elm_builtin:
      acc

    RBNode_elm_builtin _ key value left right:
      for right func (func key value (for left func acc))


forRes as Dict k v: (k: v: b: Result e b): b: Result e b =
    dict: func: acc:
    try dict as
        RBEmpty_elm_builtin:
            Ok acc

        RBNode_elm_builtin _ key value left right:
            forRes left func acc >> Result.onOk l:
            func key value l >> Result.onOk f:
            forRes right func f


forReversed as Dict k v: (k: v: b: b): b: b =
  t: func: acc:
  try t as
    RBEmpty_elm_builtin:
      acc

    RBNode_elm_builtin _ key value left right:
      forReversed left func (func key value (forReversed right func acc))


filter as (key: v: Bool): Dict key v: Dict key v =
  #with key NonFunction
  isGood: dict:
  for dict (k: v: d: if isGood k v then insert k v d else d) empty



partition as (key: v: Bool): Dict key v: (Dict key v & Dict key v) =
  #with key NonFunction
  isGood: dict:

  add =
      key: value: t:
      (t1 & t2) = t
      if isGood key value then
        (insert key value t1 & t2)

      else
        (t1 & insert key value t2)

  for dict add (empty & empty)


# QUERY?


any as (k: v: Bool): Dict k v: Bool =
    f: dict:

    try dict as
        RBNode_elm_builtin color key v left right:
            if f key v then
                True
            else
                any f left or any f right

        RBEmpty_elm_builtin:
            False


# LISTS



keys as Dict k v: [k] =
  dict:
  forReversed dict (key: value: keyList: key :: keyList) []



values as Dict k v: [v] =
  dict:
  forReversed dict (key: value: valueList: value :: valueList) []



toList as Dict k v: [k & v] =
  dict:

  f as k: v: [k & v]: [k & v] =
      key: value: list:
      key & value :: list

  forReversed dict f []




fromList as [key & v]: Dict key v =
  #with key NonFunction
  assocs:
  List.for assocs (keyAndValue: dict: insert keyAndValue.first keyAndValue.second dict) empty

