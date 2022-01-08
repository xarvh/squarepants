
# Dependincy resolution, ie, transform a directed graph into a tree, possibly noting circular dependencies
#
#
#
[# TODO tests?

   Maybe rewrite them when we have a new algorithm?


   findAllNestedSiblingReferences : Test
   findAllNestedSiblingReferences =
       Test.Group "findAllNestedSiblingReferences"
           [ simpleTest
               { name = "Two-way recursion"
               , run = \_: TI.findAllNestedSiblingReferences referencedSiblingDefs "a" Set.empty
               , expected = Set.fromList [ "a", "b", "blah", "meh" ]
               }
           , simpleTest
               { name = "Three way recursion"
               , run = \_: TI.findAllNestedSiblingReferences referencedSiblingDefs "c" Set.empty
               , expected = Set.fromList [ "c", "d", "e" ]
               }
           , simpleTest
               { name = "Self recursion"
               , run = \_: TI.findAllNestedSiblingReferences referencedSiblingDefs "f" Set.empty
               , expected = Set.fromList [ "f" ]
               }
           , simpleTest
               { name = "No recursion"
               , run = \_: TI.findAllNestedSiblingReferences referencedSiblingDefs "g" Set.empty
               , expected = Set.fromList [ "g", "h" ]
               }
           , simpleTest
               { name = "Single self recursion"
               , run = \_: TI.findAllNestedSiblingReferences referencedSiblingDefs "cc" Set.empty
               , expected = Set.fromList [ "cc", "dd", "ee" ]
               }
           ]

      findMutualRecursions : List Test
      findMutualRecursions =
          let
          in
          [ simpleTest
              { name =
                  "Two-way mutual recursion A"
              , run =
                  \_:
                      TI.findMutualRecursions referencedSiblingDefs "a" Set.empty
              , expected =
                  Set.fromList [ "a", "b" ]
              }
          , simpleTest
              { name =
                  "Two-way mutual recursion B"
              , run =
                  \_:
                      TI.findMutualRecursions referencedSiblingDefs "b" Set.empty
              , expected =
                  Set.fromList [ "a", "b" ]
              }
          , simpleTest
              { name =
                  "Three-way mutual recursion D"
              , run =
                  \_:
                      TI.findMutualRecursions referencedSiblingDefs "d" Set.empty
              , expected =
                  Set.fromList [ "c", "d", "e" ]
              }
          , simpleTest
              { name =
                  "Self recursion"
              , run =
                  \_:
                      TI.findMutualRecursions referencedSiblingDefs "f" Set.empty
              , expected =
                  Set.fromList [ "f" ]
              }
          , simpleTest
              { name =
                  "No recursion 1"
              , run =
                  \_:
                      TI.findMutualRecursions referencedSiblingDefs "g" Set.empty
              , expected =
                  Set.fromList [ "g" ]
              }
          , simpleTest
              { name =
                  "No recursion 2"
              , run =
                  \_:
                      TI.findMutualRecursions referencedSiblingDefs "cc" Set.empty
              , expected =
                  Set.fromList [ "cc" ]
              }
          ]
#]


alias State key = {
    , unresolved as Set key
    , resolved as [key]
    }


#
# <https://www.electricmonk.nl/docs/dependency_resolving_algorithm/dependency_resolving_algorithm.html>
#
resolve as (key: Set key): key: State key: Result [key] (State key) with key NonFunction =
    getEdges: target: state0:

    if List.member target state0.resolved then
        Ok state0

    else if Set.member target state0.unresolved then
        Err [ target ]

    else
        s as State key =
            { state0 with unresolved = Set.insert target state0.unresolved }

        # TODO use Set.foldlRes
        try Dict.foldlRes (a: None: resolve getEdges a) (getEdges target) s as
            Err path:
                Err << target :: path

            Ok stateF:
                Ok
                    {
                    , unresolved = Set.remove target stateF.unresolved
                    , resolved = target :: stateF.resolved
                    }


reorder as (node: Set key): Dict key node: Result [key] [node] with key NonFunction =
    nodeToEdges: nodesById:

    keyToEdges as key: Set key =
        id:
        try Dict.get id nodesById as
            Nothing: Set.empty
            Just node: nodeToEdges node

    {
    , unresolved = Set.empty
    , resolved = []
    }
        >> Dict.foldlRes (k: v: resolve keyToEdges k) nodesById
        >> Result.map (x: x.resolved >> List.filterMap (id: Dict.get id nodesById) >> List.reverse)
