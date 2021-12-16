module StateMonad exposing (..)

{-| My understanding of monads makes me think that this is some sort of state monad.
Because it has state.
But I don't know, Haskell explanations are still impenetrable to me.
-}

import Dict exposing (Dict)


{-| I could wrap the definition inside an opaque type, but I don't think the benefits are worth the extra fiddling

(Which is why Squarepants allows opaque types only inside libraries: you should use one only when you have a complete idea of how it will be used,
otherwise you just end up with wrap/unwrap helpers which moot the whole point of opacity...)

-}
type alias M state a =
    state -> ( a, state )


do : M state a -> (a -> M state b) -> M state b
do m f state0 =
    let
        ( a, state1 ) =
            m state0
    in
    f a state1


return : a -> M state a
return a state =
    ( a, state )


update : (state -> state) -> M state state
update f state =
    let
        s =
            f state
    in
    ( s, s )


run : state -> M state a -> ( a, state )
run state m =
    m state


get : (state -> a) -> M state a
get getter state =
    ( getter state
    , state
    )



--


map : (a -> b) -> M state a -> M state b
map f m =
    do m (f >> return)



-- Maybe


maybe_map : (a -> M state b) -> Maybe a -> M state (Maybe b)
maybe_map f ma =
    case ma of
        Nothing ->
            return Nothing

        Just a ->
            do (f a) <| \b ->
            return <| Just b



-- List


list_foldl : (item -> accum -> M state accum) -> List item -> accum -> M state accum
list_foldl f items accum =
    case items of
        [] ->
            return accum

        head :: tail ->
            do (f head accum) <| list_foldl f tail


list_map : (a -> M state b) -> List a -> M state (List b)
list_map f la =
    let
        apply a accum =
            do (f a) <| \b ->
            return (b :: accum)
    in
    do (list_foldl apply la []) (List.reverse >> return)


list_map2 : (a -> b -> M state c) -> List a -> List b -> M state (List c)
list_map2 f la lb =
    let
        apply ( a, b ) accum =
            do (f a b) <| \c ->
            return (c :: accum)
    in
    do (list_foldl apply (List.map2 Tuple.pair la lb) []) (List.reverse >> return)



-- Dict


dict_foldl : (comparable -> item -> accum -> M state accum) -> Dict comparable item -> accum -> M state accum
dict_foldl f =
    Dict.toList >> list_foldl (\( k, v ) -> f k v)


dict_map : (comparable -> a -> M state b) -> Dict comparable a -> M state (Dict comparable b)
dict_map f dict =
    let
        insert : comparable -> a -> Dict comparable b -> M state (Dict comparable b)
        insert c a d =
            do (f c a) <| \b ->
            return (Dict.insert c b d)
    in
    dict_foldl insert dict Dict.empty
