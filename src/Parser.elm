module Parser exposing (..)

{-

   debuggability:

   a state should be added to track
     * the current stack of combinators
     * the current input head

   How do you do this without
     1) making the gramar declaration messier
     2) mkaing the parsing slower?

-}


type alias Parser input output =
    List input -> Maybe ( output, List input )



----
--- Basic parsers
--


fromFn : (input -> Maybe output) -> Parser input output
fromFn test tokens =
    case tokens of
        [] ->
            Nothing

        head :: tail ->
            case test head of
                Nothing ->
                    Nothing

                Just output ->
                    Just ( output, tail )


end : Parser input ()
end tokens =
    if tokens == [] then
        Just ( (), [] )

    else
        Nothing


map : (a -> b) -> Parser input a -> Parser input b
map f p tokens =
    -- `case` is probably faster than `p >> Maybe.map (Tuple.mapFirst f)`
    case p tokens of
        Nothing ->
            Nothing

        Just ( a, newTokens ) ->
            Just ( f a, newTokens )


andThen : (a -> Parser input b) -> Parser input a -> Parser input b
andThen f p tokens =
    case p tokens of
        Nothing ->
            Nothing

        Just ( a, newTokens ) ->
            f a newTokens


{-| This is just andThen with flipped arguments, which is useful for chaining
-}
do : Parser i a -> (a -> Parser i b) -> Parser i b
do p f tokens =
    case p tokens of
        Nothing ->
            Nothing

        Just ( a, newTokens ) ->
            f a newTokens


return : a -> Parser input a
return a tokens =
    Just ( a, tokens )


breakCircularReference : (() -> Parser i b) -> Parser i b
breakCircularReference f =
    f ()



----
--- Combinator
--


oneOf : List (Parser i o) -> Parser i o
oneOf ps tokens =
    case ps of
        [] ->
            Nothing

        p :: p_tail ->
            case p tokens of
                Just stuff ->
                    Just stuff

                Nothing ->
                    oneOf p_tail tokens


optional : Parser i o -> Parser i (Maybe o)
optional p tokens =
    case p tokens of
        Nothing ->
            Just ( Nothing, tokens )

        Just ( thing, newTokens ) ->
            Just ( Just thing, newTokens )


without : Parser i o -> Parser i ()
without p tokens =
    case p tokens of
        Nothing ->
            Just ( (), tokens )

        Just _ ->
            Nothing


t2 : Parser i a -> Parser i b -> Parser i ( a, b )
t2 pa pb =
    do pa <| \a ->
    do pb <| \b ->
    return ( a, b )


t3 : Parser i a -> Parser i b -> Parser i c -> Parser i ( a, b, c )
t3 pa pb pc =
    do pa <| \a ->
    do pb <| \b ->
    do pc <| \c ->
    return ( a, b, c )


zeroOrMore : Parser i o -> Parser i (List o)
zeroOrMore p =
    let
        recursive : List o -> List i -> ( List o, List i )
        recursive accum tokens =
            case p tokens of
                Nothing ->
                    ( [], tokens )

                Just ( thing, newTokens ) ->
                    recursive (thing :: accum) newTokens
    in
    recursive [] >> Tuple.mapFirst List.reverse >> Just


oneOrMore : Parser i o -> Parser i ( o, List o )
oneOrMore p =
    t2 p (zeroOrMore p)
