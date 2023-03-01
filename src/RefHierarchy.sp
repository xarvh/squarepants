#
# Dependincy resolution, ie, transform a directed graph into a tree, possibly noting circular dependencies
#

alias State key = {
    , resolved as [key]
    , circular as Dict (Set key) [key]
    }


#
# Core algorithm
#
# https://www.electricmonk.nl/docs/dependency_resolving_algorithm/dependency_resolving_algorithm.html
#
resolve as fn (fn key: Set key), key, [key], State key: State key with key NonFunction =
    fn getEdges, target, path, state0:

    if List.member target state0.resolved then
        state0

    else if List.member target path then

        circ as [key] =
            target :: List.takeWhile (fn key: key /= target) path

        { state0 with circular = Dict.insert (Set.fromList circ) circ .circular }

    else
        s =
            state0 >> Dict.for __ (getEdges target) (fn a, None, d: resolve getEdges a (target :: path) d)

        { s with resolved = target :: .resolved }


#
# Interface function
#
reorder as fn (fn node: Set key), Dict key node: [[key]] & [key] with key NonFunction =
    fn nodeToEdges, nodesById:

    keyToEdges as fn key: Set key =
        fn id:
        try Dict.get id nodesById as
            , Nothing: Set.empty
            , Just node: nodeToEdges node

    state0 = {
        , resolved = []
        , circular = Dict.empty
        }

    stateF =
        state0 >> Dict.for __ nodesById (fn k, v, d: resolve keyToEdges k [] d)

    Dict.values stateF.circular & List.reverse stateF.resolved

