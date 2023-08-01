

alias Set a =
    Dict a None


empty as Set a with a NonFunction =
    Dict.empty


member as fn a, Set a: Bool with a NonFunction =
    Dict.member


size as fn Set a: Int with a NonFunction =
    Dict.size


isEmpty as fn Set a: Bool with a NonFunction =
    Dict.isEmpty


insert as fn a, Set a: Set a with a NonFunction =
    Dict.insert __ None __


remove as fn a, Set a: Set a with a NonFunction =
    Dict.remove


ofOne as fn a: Set a with a NonFunction =
    Dict.ofOne __ None


join as fn Set a, Set a: Set a with a NonFunction =
    Dict.join


intersect as fn Set a, Set a: Set a with a NonFunction =
    Dict.intersect


diff as fn Set a, Set a: Set a with a NonFunction =
    Dict.diff


map as fn (fn a: b), Set a: Set b with a NonFunction =
    fn f, set:
    Dict.for empty set (fn k, _, d: Dict.insert (f k) None d)


for as fn a, Set b, (fn b, a: a): a =
    fn init, set, f:
    Dict.for init set (fn k, _, d: f k d)


toList as fn Set a: [a] with a NonFunction =
    Dict.keys


fromList as fn [a]: Set a with a NonFunction =
    List.for empty __ insert

