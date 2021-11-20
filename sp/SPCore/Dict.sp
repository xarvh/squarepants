## Shamelessly translated from https://raw.githubusercontent.com/elm/core/1.0.5/src/Dict.elm


union NColor = Red, Black

union Dict key v =
    , RBNode_elm_builtin NColor key v (Dict key v) (Dict key v)
    , RBEmpty_elm_builtin


empty =
  as Dict key v
  with key NonFunction
  RBEmpty_elm_builtin


get targetKey dict =
  as key: Dict key v: Maybe v
  with key NonFunction

  try dict as
    RBEmpty_elm_builtin:
      Nothing

    RBNode_elm_builtin _ key value left right:
      try SPCore/Basics.compare targetKey key as
        1:
          get targetKey right

        0:
          Just value

        _:
          get targetKey left




member key dict =
  as key: Dict key v: Bool
  with key NonFunction

  try get key dict as
    Just _:
      True

    Nothing:
      False



size =
  as Dict key v: Int
  with key NonFunction

  sizeHelp n dict =
    as Int: Dict key v: Int

    try dict as
      RBEmpty_elm_builtin:
        n

      RBNode_elm_builtin _ _ _ left right:
        sizeHelp (sizeHelp (n+1) right) left

  sizeHelp 0



isEmpty dict =
  as Dict key v: Bool
  try dict as
    RBEmpty_elm_builtin:
      True

    RBNode_elm_builtin _ _ _ _ _:
      False



insert key value dict =
  as key: v: Dict key v: Dict key v
  with key NonFunction
  # Root node as always Black
  try insertHelp key value dict as
    RBNode_elm_builtin Red k v l r:
      RBNode_elm_builtin Black k v l r

    x:
      x


insertHelp key value dict =
  as key: v: Dict key v: Dict key v
  with key NonFunction
  try dict as
    RBEmpty_elm_builtin:
      # New nodes are always red. If it violates the rules, it will be fixed
      # when balancing.
      RBNode_elm_builtin Red key value RBEmpty_elm_builtin RBEmpty_elm_builtin

    RBNode_elm_builtin nColor nKey nValue nLeft nRight:
      try SPCore/Basics.compare key nKey as
        1:
          balance nColor nKey nValue nLeft (insertHelp key value nRight)

        0:
          RBNode_elm_builtin nColor nKey value nLeft nRight

        _:
          balance nColor nKey nValue (insertHelp key value nLeft) nRight



balance color key value left right =
  as NColor: key: v: Dict key v: Dict key v: Dict key v
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



remove key dict =
  as key: Dict key v: Dict key v
  with key NonFunction
  # Root node as always Black
  try removeHelp key dict as
    RBNode_elm_builtin Red k v l r:
      RBNode_elm_builtin Black k v l r

    x:
      x



removeHelp targetKey dict =
  as key: Dict key v: Dict key v
  with key NonFunction
  try dict as
    RBEmpty_elm_builtin:
      RBEmpty_elm_builtin

    RBNode_elm_builtin color key value left right:
      if SPCore/Basics.compare targetKey key == 0 - 1:
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


removeHelpPrepEQGT targetKey dict color key value left right =
  as key: Dict key v: NColor: key: v: Dict key v: Dict key v: Dict key v
  with key NonFunction
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



removeHelpEQGT targetKey dict =
  as key: Dict key v: Dict key v
  with key NonFunction
  try dict as
    RBNode_elm_builtin color key value left right:
      if targetKey == key:
        try getMin right as
          RBNode_elm_builtin _ minKey minValue _ _:
            balance color minKey minValue left (removeMin right)

          RBEmpty_elm_builtin:
            RBEmpty_elm_builtin
      else
        balance color key value left (removeHelp targetKey right)

    RBEmpty_elm_builtin:
      RBEmpty_elm_builtin


getMin dict =
  as Dict key v: Dict key v
  try dict as
    RBNode_elm_builtin _ _ _ left _:
      try left as
         RBNode_elm_builtin _ _ _ _ _:
            getMin left
         _:
            dict

    _:
      dict


removeMin dict =
  as Dict key v: Dict key v
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


moveRedLeft dict =
  as Dict key v: Dict key v
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


moveRedRight dict =
  as Dict key v: Dict key v
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



update targetKey alter dictionary =
  as key: (Maybe v: Maybe v): Dict key v: Dict key v
  with key NonFunction
  try alter (get targetKey dictionary) as
    Just value:
      insert targetKey value dictionary

    Nothing:
      remove targetKey dictionary



singleton key value =
  as key: v: Dict key v
  with key NonFunction
  # Root node as always Black
  RBNode_elm_builtin Black key value RBEmpty_elm_builtin RBEmpty_elm_builtin


# COMBINE



join t1 t2 =
  as Dict key v: Dict key v: Dict key v
  with key NonFunction
  foldl insert t1 t2



intersect t1 t2 =
  as Dict key v: Dict key v: Dict key v
  with key NonFunction
  filter (fn k _: member k t2) t1



diff t1 t2 =
  as Dict key a: Dict key b: Dict key a
  with key NonFunction
  foldl (fn k v t: remove k t) t2 t1


merge leftStep bothStep rightStep leftDict rightDict initialResult =
  as  (key: a: res: res): (key: a: b: res: res): (key: b: res: res): Dict key a: Dict key b: res: res
  with key NonFunction

  stepState rKey rValue (list & res) =
    as key: b: [key & a] & res: [key & a] & res
    try list as
      []:
        (list & rightStep rKey rValue res)

      (lKey & lValue) :: rest:
          try SPCore/Basics.compare lKey rKey as
              1: list & rightStep rKey rValue res
              0: rest & bothStep lKey lValue rValue res
              _: stepState rKey rValue (rest & leftStep lKey lValue res)

#        if SPCore/Basics.compare lKey rKey == 0 - 1:
#          stepState rKey rValue (rest & leftStep lKey lValue res)
#
#        else if SPCore/Basics.compare lKey rKey == 1:
#          (list & rightStep rKey rValue res)
#
#        else
#          (rest & bothStep lKey lValue rValue res)

  (leftovers & intermediateResult) =
      as [key & a] & res
      foldl stepState rightDict (toList leftDict & initialResult)

  liftLeftStep (k & v) res =
      as (key & a): res: res
      leftStep k v res

  List.foldl liftLeftStep leftovers intermediateResult


# TRANSFORM



map func dict =
  as (k: a: b): Dict k a: Dict k b
  try dict as
    RBEmpty_elm_builtin:
      RBEmpty_elm_builtin

    RBNode_elm_builtin color key value left right:
      RBNode_elm_builtin color key (func key value) (map func left) (map func right)



foldl func dict acc =
  as (k: v: b: b): Dict k v: b: b
  try dict as
    RBEmpty_elm_builtin:
      acc

    RBNode_elm_builtin _ key value left right:
      foldl func right (func key value (foldl func left acc))



foldlRes func dict acc =
    as (k: v: b: Result e b): Dict k v: b: Result e b
    try dict as
        RBEmpty_elm_builtin:
            Ok acc

        RBNode_elm_builtin _ key value left right:
            foldlRes func left acc >> Result.andThen fn l:
            func key value l >> Result.andThen fn f:
            foldlRes func right f



foldr func t acc =
  as (k: v: b: b): Dict k v: b: b
  try t as
    RBEmpty_elm_builtin:
      acc

    RBNode_elm_builtin _ key value left right:
      foldr func left (func key value (foldr func right acc))



filter isGood dict =
  as (key: v: Bool): Dict key v: Dict key v
  with key NonFunction
  foldl (fn k v d: if isGood k v: insert k v d else d) dict empty



partition isGood dict =
  as (key: v: Bool): Dict key v: (Dict key v & Dict key v)
  with key NonFunction

  add key value (t1 & t2) =
      if isGood key value:
        (insert key value t1 & t2)

      else
        (t1 & insert key value t2)

  foldl add dict (empty & empty)


# LISTS



keys dict =
  as Dict k v: List k
  foldr (fn key value keyList: key :: keyList) dict []



values dict =
  as Dict k v: List v
  foldr (fn key value valueList: value :: valueList) dict []



toList dict =
  as Dict k v: [k & v]

  f =
    as k: v: [k & v]: [k & v]
    fn key value list:
       key & value :: list

  foldr f dict []




fromList assocs =
  as List (key & v): Dict key v
  with key NonFunction

  List.foldl (fn (key & value) dict: insert key value dict) assocs empty

