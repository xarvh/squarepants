var Array a =
    , 'array__ (Array a)


push as fn @Array a, a: None =
    fn @array, item:
    todo "native"


pop as fn @Array a: Maybe a =
    fn @array:
    todo "native"


get as fn @Array a, Int: Maybe a =
    fn @array, index:
    todo "native"


set as fn @Array a, Int, a: Bool =
    fn @array, index, item:
    todo "native"


sortBy as fn @Array a, fn a: b: None with b NonFunction =
    fn @array, f:
    todo "native"


fromList as fn [ a ]: !Array a =
    fn list:
    todo "native"


toList as fn @Array a: [ a ] =
    fn @array:
    todo "native"


each as fn @Array a, fn a: None: None =
    fn @array, f:
    todo "native"
