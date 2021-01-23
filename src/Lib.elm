module Lib exposing (..)

import Dict exposing (Dict)


{-| TODO rename to list\_foldlRes
-}
result_fold : (item -> accum -> Result error accum) -> List item -> accum -> Result error accum
result_fold f ls accum =
    case ls of
        [] ->
            Ok accum

        head :: tail ->
            case f head accum of
                Err x ->
                    Err x

                Ok newAccum ->
                    result_fold f tail newAccum


list_mapRes : (a -> Result error b) -> List a -> Result error (List b)
list_mapRes f ls =
    result_fold (\a acc -> Result.map (\b -> b :: acc) (f a)) ls []
        |> Result.map List.reverse


result_do a b =
    Result.andThen b a


{-| TODO Doesn't Elm have a way to interrupt iterating over a Dict?
TODO rename to dict\_foldRes
-}
dict_resFold : (comparable -> item -> accum -> Result error accum) -> Dict comparable item -> accum -> Result error accum
dict_resFold f dict accum =
    Dict.foldl (\k v -> Result.andThen (f k v)) (Ok accum) dict


{-| TODO rename to dict\_mapRes
-}
dict_resMap : (comparable -> a -> Result error b) -> Dict comparable a -> Result error (Dict comparable b)
dict_resMap f aDict =
    dict_resFold (\k a bAcc -> Result.map (\b -> Dict.insert k b bAcc) (f k a)) aDict Dict.empty



----
---
--


type Either a b
    = Left a
    | Right b


{-| Unlike List.partition, this actually allows to unwrap constructors
-}
partition : (a -> Either b c) -> List a -> ( List b, List c )
partition f ls =
    let
        fold item ( left, right ) =
            case f item of
                Left l ->
                    ( l :: left, right )

                Right r ->
                    ( left, r :: right )
    in
    List.foldr fold ( [], [] ) ls
