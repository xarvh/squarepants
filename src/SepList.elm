module SepList exposing (..)


type alias SepList sep item =
    ( item, List ( sep, item ) )
