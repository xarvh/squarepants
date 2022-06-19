

union Hash k v =
    Hash__ (Hash k v)


empty as Hash k v with k NonFunction =
    # I can't believe it worked
    Hash__ << todo "native"


insert as Hash k v @: k: v: None with k NonFunction =
    h@: k: v:
    todo "native"


remove as Hash k v @: k: None with k NonFunction =
    h@: k:
    todo "native"


get as Hash k v: k: Maybe v with k NonFunction =
    h: k:
    todo "native"


for as Hash k v: (k: v: a: a): a: a with k NonFunction =
    h: f: a:
    todo "native"


each as Hash k v: (k: v: None): None with k NonFunction =
    h: f:
    todo "native"


fromList as [k & v]: Hash k v with k NonFunction =
    l:
    h @= empty
    List.each l (k & v):
        insert @h k v
    h


toList as Hash k v: [k & v] with k NonFunction =
    h:
    [] >> for h k: v: l: (k & v) :: l


# TODO
#fromDict as Dict k v: Hash k v with k NonFunction =
#    d:
#
#
#toDict as Hash k v: Dict k v: with k NonFunction =
#    h:

