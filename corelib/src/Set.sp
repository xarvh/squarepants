Set a =
    Dict a None


empty as Set a with a NonFunction =
    Dict.empty


member as fn a, Set a: Bool with a NonFunction =
    Dict.has


size as fn Set a: Int with a NonFunction =
    Dict.length


isEmpty as fn Set a: Bool with a NonFunction =
    Dict.isEmpty


insert as fn a, Set a: Set a with a NonFunction =
    fn element, set: Dict.insert set element 'none


remove as fn a, Set a: Set a with a NonFunction =
    Dict.remove


ofOne as fn a: Set a with a NonFunction =
    Dict.ofOne __ 'none


join as fn Set a, Set a: Set a with a NonFunction =
    Dict.join


intersect as fn Set a, Set a: Set a with a NonFunction =
    Dict.intersect


diff as fn Set a, Set a: Set a with a NonFunction =
    Dict.diff


map as fn (fn a: b), Set a: Set b with a NonFunction =
    fn f, set:
    Dict.for empty set (fn d, k, _: Dict.insert d (f k) 'none)


for as fn a, Set b, (fn b, a: a): a =
    fn init, set, f:
    Dict.for init set (fn d, k, _: f k d)


toList as fn Set a: [ a ] with a NonFunction =
    Dict.keys


fromList as fn [ a ]: Set a with a NonFunction =
    List.for empty __ (fn s, k: insert k s)
