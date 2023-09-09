var Hash k v =
    , 'hash__ (Hash k v)


insert as fn @Hash k v, k, v: None with k NonFunction =
    this_is_sp_native


remove as fn @Hash k v, k: None with k NonFunction =
    this_is_sp_native


get as fn @Hash k v, k: Maybe v with k NonFunction =
    this_is_sp_native


for_ as fn a, @Hash k v, fn k, v, a: a: a with k NonFunction =
    fn a, @h, f:
    for @h f a


for as fn @Hash k v, fn k, v, a: a, a: a with k NonFunction =
    this_is_sp_native


each as fn @Hash k v, fn k, v: None: None with k NonFunction =
    this_is_sp_native


fromList as fn [ k & v ]: !Hash k v with k NonFunction =
    this_is_sp_native


toList as fn @Hash k v: [ k & v ] with k NonFunction =
    fn @h:
    for @h (fn k, v, l: [ k & v, l... ]) []


pop as fn @Hash k v: Maybe (k & v) with k NonFunction =
    this_is_sp_native
