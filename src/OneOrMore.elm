module OneOrMore exposing (..)

{-| TODO rename to List1
-}


type alias OneOrMore a =
    ( a, List a )


toList : OneOrMore a -> List a
toList ( h, ts ) =
    h :: ts


reverse : OneOrMore a -> OneOrMore a
reverse ( a, ls ) =
    reverseRec a ls []


reverseRec : a -> List a -> List a -> ( a, List a )
reverseRec a ls accum =
    case ls of
        [] ->
            ( a, accum )

        head :: tail ->
            reverseRec head tail (a :: accum)
