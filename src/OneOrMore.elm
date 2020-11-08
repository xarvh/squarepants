module OneOrMore exposing (..)


type alias OneOrMore a =
    ( a, List a )


toList : OneOrMore a -> List a
toList ( h, ts ) =
    h :: ts
