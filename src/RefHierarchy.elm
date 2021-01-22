module RefHierarchy exposing (..)

import Dict exposing (Dict)
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


{-| |

Function and alias definitions can reference each other, so we order them from
the ones which hopefully do not reference any sibling to those that reference many.

The algorithm is terrible but for now it will do.

-}
reorder : (item -> comparable) -> (item -> Set comparable) -> List item -> List item
reorder getName getSiblingRefs items =
    let
        names : Set comparable
        names =
            items
                |> List.map getName
                |> Set.fromList

        -- For each name, find all names it directly references
        -- And remove those that are not direct siblings
        referencedSiblings : Dict comparable (Set comparable)
        referencedSiblings =
            List.foldl (\item -> Dict.insert (getName item) (Set.intersect names (getSiblingRefs item))) Dict.empty items

        findAllNestedSiblingReferences : comparable -> Set comparable -> Set comparable
        findAllNestedSiblingReferences name accum =
            if Set.member name accum then
                accum

            else
                Set.foldl
                    findAllNestedSiblingReferences
                    (Set.insert name accum)
                    (Dict.get name referencedSiblings |> Maybe.withDefault Set.empty)

        -- For each name, find all the sibling it references AND the sibling /they/ reference (ie, recurse over the referenced sibs)
        --
        -- This is brute and horribly inefficient, but I don't have any brain for anything better.
        --
        allNestedSiblingRefs : Dict comparable (Set comparable)
        allNestedSiblingRefs =
            Dict.map (\k v -> findAllNestedSiblingReferences k Set.empty) referencedSiblings

        oneReferencesTwo : comparable -> comparable -> Bool
        oneReferencesTwo one two =
            case Dict.get one allNestedSiblingRefs of
                Nothing ->
                    -- should not happen
                    False

                Just set ->
                    Set.member two set

        order : item -> item -> Order
        order aItem bItem =
            let
                a =
                    getName aItem

                b =
                    getName bItem
            in
            case ( oneReferencesTwo a b, oneReferencesTwo b a ) of
                ( True, True ) ->
                    -- Mutually recursive, order doesn't matter
                    EQ

                ( True, False ) ->
                    -- A should go after B
                    GT

                ( False, True ) ->
                    -- A should go before B
                    LT

                ( False, False ) ->
                    -- Neither references the other, order doesn't matter
                    EQ
    in
    List.sortWith order items
