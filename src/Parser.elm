module Parser exposing (..)


type alias Parser input output =
    List input -> Maybe ( output, List input )


single : (input -> Maybe output) -> Parser input output
single test tokens =
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


p2 : Parser i a -> Parser i b -> Parser i ( a, b )
p2 pa pb tokens =
    case pa tokens of
        Nothing ->
            Nothing

        Just ( a, tokens_a ) ->
            case pb tokens_a of
                Nothing ->
                    Nothing

                Just ( b, token_b ) ->
                    Just ( ( a, b ), token_b )


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
    p2 p (zeroOrMore p)



{-
   if expr then expr else expr

   p3(
       (secondOfTwo p_if p_expr)
       (secondOfTwo p_then p_expr)
       (secondOfTwo p_else p_expr)
       )
       |> map (\(condition, thenExpr, elseExpr) -> node_if condition thenExpr elseExpr)
-}
