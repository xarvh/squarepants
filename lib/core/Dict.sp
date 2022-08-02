## Shamelessly translated from https://raw.githubusercontent.com/elm/core/1.0.5/src/Dict.elm


union Dict key v =
    Dict__ (Dict key v)


empty as Dict key v =
  #with key NonFunction
  Dict__ << todo "native"


get as key: Dict key v: Maybe v =
  #with key NonFunction
  targetKey: dict:

  todo "native"


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
  d:

  todo "native"




isEmpty as Dict key v: Bool =
  dict:

  todo "native"



insert as key: v: Dict key v: Dict key v =
  #with key NonFunction
  key: value: dict:

  todo "native"


remove as key: Dict key v: Dict key v =
  #with key NonFunction
  key: dict:

  todo "native"


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

  insert key value empty


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


  aa as key: a: res: res =
      key: a:

      try Dict.get key rightDict as
          Nothing:
              leftStep key a

          Just b:
              bothStep key a b


  bb as key: b: res: res =
      key: b:

      try Dict.get key leftDict as
          Nothing:
              rightStep key b

          Just b:
              identity


  initialResult
  >> Dict.for leftDict aa
  >> Dict.for rightDict bb




# TRANSFORM



map as (k: a: b): Dict k a: Dict k b =
  func: dict:

  todo "native"


mapRes as (k: a: Result e b): Dict k a: Result e (Dict k b) =
  func: dict:

  todo "native"


mapKeys as (k: j): Dict k a: Dict j a =
    func: dict:
    for dict (k: Dict.insert (func k)) Dict.empty


for as Dict k v: (k: v: b: b): b: b =
  dict: func: acc:

  todo "native"


forRes as Dict k v: (k: v: b: Result e b): b: Result e b =
    dict: func: acc:

    todo "native"


forReversed as Dict k v: (k: v: b: b): b: b =
  t: func: acc:

  todo "native"


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

