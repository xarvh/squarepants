#
# Dependincy resolution, ie, transform a directed graph into a tree, possibly noting circular dependencies
#

State key =
    {
    , circular as Dict (Set key) [ key ]
    , resolved as [ key ]
    }


#
# Core algorithm
#
# https://www.electricmonk.nl/docs/dependency_resolving_algorithm/dependency_resolving_algorithm.html
#
resolve as fn (fn key: Dict key whatever), key, [ key ], State key: State key with key NonFunction =
    fn getEdges, target, path, state0:
    if List.contains target state0.resolved then
        state0
    else if List.contains target path then
        circ as [ key ] =
            target :: List.takeWhile (fn key: key /= target) path

        { state0 with circular = Dict.insert .circular (Set.fromList circ) circ }
    else
        s =
            state0 >> Dict.for __ (getEdges target) (fn d, a, _: resolve getEdges a (target :: path) d)

        { s with resolved = target :: .resolved }


#
# Interface function
#
reorder as fn fn node: Dict key whatever, Dict key node: [ [ key ] ] & [ key ] with key NonFunction =
    fn nodeToEdges, nodesById:
    keyToEdges as fn key: Dict key whatever =
        fn id:
        try Dict.get id nodesById as
            'nothing: Dict.empty
            'just node: nodeToEdges node

    state0 =
        {
        , circular = Dict.empty
        , resolved = []
        }

    stateF =
        state0 >> Dict.for __ nodesById (fn d, k, v: resolve keyToEdges k [] d)

    Dict.values stateF.circular & List.reverse stateF.resolved
