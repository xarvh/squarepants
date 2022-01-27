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
resolve as (key: Set key): key: [key]: State key: State key with key NonFunction =
    getEdges: target: path: state0:

    if List.member target state0.resolved then
        state0

    else if List.member target path then

        circ as [key] =
            target :: List.takeWhile (key: key /= target) path

        { state0 with circular = Dict.insert (Set.fromList circ) circ .circular }

    else
        s =
            state0 >> Dict.for (getEdges target) (a: None: resolve getEdges a (target :: path))

        { s with resolved = target :: .resolved }


#
# Interface function
#
reorder as (node: Set key): Dict key node: [[key]] & [key] with key NonFunction =
    nodeToEdges: nodesById:

    keyToEdges as key: Set key =
        id:
        try Dict.get id nodesById as
            Nothing: Set.empty
            Just node: nodeToEdges node

    state0 = {
        , resolved = []
        , circular = Dict.empty
        }

    stateF =
        state0 >> Dict.for nodesById (k: v: resolve keyToEdges k [])

    Dict.values stateF.circular & List.reverse stateF.resolved

