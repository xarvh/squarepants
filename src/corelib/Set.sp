

alias Set a =
    Dict a None


empty as Set a =
    #with a NonFunction
    Dict.empty


member as a: Set a: Bool =
    #with a NonFunction
    Dict.member


size as Set a: Int =
    #with a NonFunction
    Dict.size


isEmpty as Set a: Bool =
    #with a NonFunction
    Dict.isEmpty


insert as a: Set a: Set a =
    a:
    #with a NonFunction
    Dict.insert a None


remove as a: Set a: Set a =
    #with a NonFunction
    Dict.remove


singleton as a: Set a =
    a:
    #with a NonFunction
    Dict.singleton a None


join as Set a: Set a: Set a =
    #with a NonFunction
    Dict.join


intersect as Set a: Set a: Set a =
    #with a NonFunction
    Dict.intersect


diff as Set a: Set a: Set a =
    #with a NonFunction
    Dict.diff


map as (a: b): Set a: Set b =
    f: set:
    #with a, b NonFunction
    Dict.foldl (k: _: Dict.insert (f k) None) set Dict.empty


toList as Set a: [a] =
    #with a NonFunction
    Dict.keys


fromList as [a]: Set a =
    list:
    #with a NonFunction
    List.foldl insert list Set.empty

