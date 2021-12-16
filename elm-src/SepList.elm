module SepList exposing (..)


type alias SepList sep item =
    ( item, List ( sep, item ) )


mapItem : (a -> b) -> SepList sep a -> SepList sep b
mapItem f ( a, la ) =
    ( f a
    , List.map (Tuple.mapSecond f) la
    )
