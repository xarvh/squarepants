module RefHierarchy exposing (..)

import Dict exposing (Dict)
import Lib
import Set exposing (Set)



-- Dependincy resolution, ie, transform a directed graph into a tree, possibly noting circular dependencies
--
--
--
{- TODO tests?

   Maybe rewrite them when we have a new algorithm?


   findAllNestedSiblingReferences : Test
   findAllNestedSiblingReferences =
       Test.Group "findAllNestedSiblingReferences"
           [ simpleTest
               { name = "Two-way recursion"
               , run = \_ -> TI.findAllNestedSiblingReferences referencedSiblingDefs "a" Set.empty
               , expected = Set.fromList [ "a", "b", "blah", "meh" ]
               }
           , simpleTest
               { name = "Three way recursion"
               , run = \_ -> TI.findAllNestedSiblingReferences referencedSiblingDefs "c" Set.empty
               , expected = Set.fromList [ "c", "d", "e" ]
               }
           , simpleTest
               { name = "Self recursion"
               , run = \_ -> TI.findAllNestedSiblingReferences referencedSiblingDefs "f" Set.empty
               , expected = Set.fromList [ "f" ]
               }
           , simpleTest
               { name = "No recursion"
               , run = \_ -> TI.findAllNestedSiblingReferences referencedSiblingDefs "g" Set.empty
               , expected = Set.fromList [ "g", "h" ]
               }
           , simpleTest
               { name = "Single self recursion"
               , run = \_ -> TI.findAllNestedSiblingReferences referencedSiblingDefs "cc" Set.empty
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
                  \_ ->
                      TI.findMutualRecursions referencedSiblingDefs "a" Set.empty
              , expected =
                  Set.fromList [ "a", "b" ]
              }
          , simpleTest
              { name =
                  "Two-way mutual recursion B"
              , run =
                  \_ ->
                      TI.findMutualRecursions referencedSiblingDefs "b" Set.empty
              , expected =
                  Set.fromList [ "a", "b" ]
              }
          , simpleTest
              { name =
                  "Three-way mutual recursion D"
              , run =
                  \_ ->
                      TI.findMutualRecursions referencedSiblingDefs "d" Set.empty
              , expected =
                  Set.fromList [ "c", "d", "e" ]
              }
          , simpleTest
              { name =
                  "Self recursion"
              , run =
                  \_ ->
                      TI.findMutualRecursions referencedSiblingDefs "f" Set.empty
              , expected =
                  Set.fromList [ "f" ]
              }
          , simpleTest
              { name =
                  "No recursion 1"
              , run =
                  \_ ->
                      TI.findMutualRecursions referencedSiblingDefs "g" Set.empty
              , expected =
                  Set.fromList [ "g" ]
              }
          , simpleTest
              { name =
                  "No recursion 2"
              , run =
                  \_ ->
                      TI.findMutualRecursions referencedSiblingDefs "cc" Set.empty
              , expected =
                  Set.fromList [ "cc" ]
              }
          ]
-}


type alias State comparable =
    { unresolved : Set comparable
    , resolved : List comparable
    }


{-| <https://www.electricmonk.nl/docs/dependency_resolving_algorithm/dependency_resolving_algorithm.html>
-}
resolve : (comparable -> List comparable) -> comparable -> State comparable -> Result (List comparable) (State comparable)
resolve getEdges target state0 =
    if List.member target state0.resolved then
        Ok state0

    else if Set.member target state0.unresolved then
        Err [ target ]

    else
        case Lib.list_foldlRes (resolve getEdges) (getEdges target) { state0 | unresolved = Set.insert target state0.unresolved } of
            Err path ->
                Err <| target :: path

            Ok stateF ->
                Ok
                    { unresolved = Set.remove target stateF.unresolved
                    , resolved = target :: stateF.resolved
                    }


{-| TODO this function is a dumpster fire and I don't care because I'll rewrite it anyway in SP
-}
reorder : (node -> comparable) -> (node -> Set comparable) -> List node -> Result (List comparable) (List node)
reorder getId getTooManyEdges nodes =
    let
        nodesById : Dict comparable node
        nodesById =
            List.foldl (\n -> Dict.insert (getId n) n) Dict.empty nodes

        idsList : List comparable
        idsList =
            Dict.keys nodesById

        idsSet : Set comparable
        idsSet =
            Set.fromList idsList

        getEdges : comparable -> List comparable
        getEdges id =
            Dict.get id nodesById
                |> Maybe.map getTooManyEdges
                |> Maybe.withDefault Set.empty
                -- TODO getTooManyEdges will produce references to things that aren't nodes (ex: aliases referencing non-alias types)
                -- This is a problem of the caller, not one that this function should deal with
                |> Set.intersect idsSet
                |> Set.toList
    in
    { unresolved = Set.empty
    , resolved = []
    }
        |> Lib.list_foldlRes (resolve getEdges) idsList
        |> Result.map (.resolved >> List.filterMap (\id -> Dict.get id nodesById) >> List.reverse)
