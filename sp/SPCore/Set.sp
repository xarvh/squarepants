

alias Set a =
    Dict a None


empty =
    as Set a
    with a NonFunction
    Dict.empty


member =
    as a: Set a: Bool
    with a NonFunction
    Dict.member


size =
    as Set a: Int
    with a NonFunction
    Dict.size


isEmpty =
    as Set a: Bool
    with a NonFunction
    Dict.isEmpty


insert a =
    as a: Set a: Set a
    with a NonFunction
    Dict.insert a None


remove =
    as a: Set a: Set a
    with a NonFunction
    Dict.remove


singleton a =
    as a: Set a
    with a NonFunction
    Dict.singleton a None


join =
    as Set a: Set a: Set a
    with a NonFunction
    Dict.join


intersect =
    as Set a: Set a: Set a
    with a NonFunction
    Dict.intersect


diff =
    as Set a: Set a: Set a
    with a NonFunction
    Dict.diff


map f set =
    as (a: b): Set a: Set b
    with a, b NonFunction
    Dict.foldl (fn k None: Dict.insert (f k) None) set Dict.empty


toList =
    as Set a: [a]
    with a NonFunction
    Dict.keys


fromList list =
    as [a]: Set a
    with a NonFunction
    List.foldl insert list Set.empty

