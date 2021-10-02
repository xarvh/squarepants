module Lib exposing (..)

import Dict exposing (Dict)


ifThenJust : Bool -> a -> Maybe a
ifThenJust condition value =
    if condition then
        Just value

    else
        Nothing



----
---
--


list_foldlRes : (item -> accum -> Result error accum) -> List item -> accum -> Result error accum
list_foldlRes f ls accum =
    case ls of
        [] ->
            Ok accum

        head :: tail ->
            case f head accum of
                Err x ->
                    Err x

                Ok newAccum ->
                    list_foldlRes f tail newAccum


list_mapRes : (a -> Result error b) -> List a -> Result error (List b)
list_mapRes f ls =
    list_foldlRes (\a acc -> Result.map (\b -> b :: acc) (f a)) ls []
        |> Result.map List.reverse


result_do a b =
    Result.andThen b a


{-| TODO Doesn't Elm have a way to interrupt iterating over a Dict?
-}
dict_foldRes : (comparable -> item -> accum -> Result error accum) -> Dict comparable item -> accum -> Result error accum
dict_foldRes f dict accum =
    Dict.foldl (\k v -> Result.andThen (f k v)) (Ok accum) dict


dict_mapRes : (comparable -> a -> Result error b) -> Dict comparable a -> Result error (Dict comparable b)
dict_mapRes f aDict =
    dict_foldRes (\k a bAcc -> Result.map (\b -> Dict.insert k b bAcc) (f k a)) aDict Dict.empty


tuple_mapSecondRes : (b -> Result err c) -> ( a, b ) -> Result err ( a, c )
tuple_mapSecondRes f ( a, b ) =
    Result.map (\c -> ( a, c )) (f b)



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
