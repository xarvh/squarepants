var Hash k v =
    , 'hash__ (Hash k v)


insert as fn @Hash k v, k, v: None with k NonFunction =
    fn @h, k, v:
    todo "native"


remove as fn @Hash k v, k: None with k NonFunction =
    fn @h, k:
    todo "native"


get as fn @Hash k v, k: Maybe v with k NonFunction =
    fn @h, k:
    todo "native"


for_ as fn a, @Hash k v, fn k, v, a: a: a with k NonFunction =
    fn a, @h, f:
    for @h f a


for as fn @Hash k v, fn k, v, a: a, a: a with k NonFunction =
    fn @h, f, a:
    todo "native"


each as fn @Hash k v, fn k, v: None: None with k NonFunction =
    fn @h, f:
    todo "native"


fromList as fn [ k & v ]: !Hash k v with k NonFunction =
    fn l:
    todo "native"


toList as fn @Hash k v: [ k & v ] with k NonFunction =
    fn @h:
    for @h (fn k, v, l: [ k & v, l... ]) []
