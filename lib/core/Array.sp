var Array a =
    , 'never (Array a)


push as fn @Array a, a: None =
    this_is_sp_native


pop as fn @Array a: Maybe a =
    this_is_sp_native


get as fn @Array a, Int: Maybe a =
    this_is_sp_native


set as fn @Array a, Int, a: Bool =
    this_is_sp_native


sortBy as fn @Array a, fn a: b: None with b NonFunction =
    this_is_sp_native


fromList as fn [ a ]: !Array a =
    this_is_sp_native


toList as fn @Array a: [ a ] =
    this_is_sp_native


each as fn @Array a, fn a: None: None =
    this_is_sp_native
