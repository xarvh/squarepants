

union Array a =
    Array__ (Array a)


push as Array a @: a: None =
    array@: item:
    todo "native"


pop as Array a @: Maybe a =
    array@:
    todo "native"


get as Array a: Int: Maybe a =
    array: index:
    todo "native"


set as Array a @: Int: a: Bool =
    array@: index: item:
    todo "native"


sortBy as Array a @: (a: b): None with b NonFunction =
    array@: f:
    todo "native"


# TODO List.toArray
fromList as [a]: Array a =
    list:
    todo "native"


toList as Array a: [a] =
    array:
    todo "native"
