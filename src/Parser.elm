module Parser exposing (..)

{-| -}

{-

   debuggability:

   a state should be added to track
     * the current stack of combinators
     * the current input head

   How do you do this without
     1) making the grammar declaration messier
     2) mkaing the parsing slower?

-}


{-| The Parser is a function that reads an input and tries to match it to a specific pattern.

The input is modelled by the two arguments:

1.  The first argument is a function that extracts a token from the input
2.  The second argument is the input state itself

For example, if the input is a `List Tokens`, we can use List.Extra.uncons to extract the next token.

    tryToParseExpression : List Token -> Result String Expression
    tryToParseExpression input =
        case expressionParser List.Extra.uncons input of
            Nothing ->
                Err "input does not contain an expression"

            Just ( expression, [] ) ->
                Ok expression

            Just ( expression, remainingInput ) ->
                Err "I found an expression, but then there was some more stuff left!!!"

-}
-- TODO rename `input` to `readState`?
type alias Parser token input output =
    (input -> Maybe ( token, input )) -> input -> Maybe ( output, input )



----
--- Basic parsers
--


fromFn : (token -> Maybe output) -> Parser token input output
fromFn test next input =
    case next input of
        Nothing ->
            Nothing

        Just ( token, nextInput ) ->
            case test token of
                Nothing ->
                    Nothing

                Just output ->
                    Just ( output, nextInput )


end : Parser token input ()
end next input =
    case next input of
        Nothing ->
            Just ( (), input )

        Just _ ->
            Nothing


map : (a -> b) -> Parser token input a -> Parser token input b
map f p next input =
    case p next input of
        Nothing ->
            Nothing

        Just ( a, nextInput ) ->
            Just ( f a, nextInput )


andThen : (a -> Parser token input b) -> Parser token input a -> Parser token input b
andThen f p next input =
    case p next input of
        Nothing ->
            Nothing

        Just ( a, nextInput ) ->
            f a next nextInput


{-| This is just andThen with flipped arguments, which is useful for chaining
-}
do : Parser t i a -> (a -> Parser t i b) -> Parser t i b
do p f next input =
    case p next input of
        Nothing ->
            Nothing

        Just ( a, nextInput ) ->
            f a next nextInput


return : a -> Parser token input a
return a next tokens =
    Just ( a, tokens )


breakCircularReference : (() -> Parser t i b) -> Parser t i b
breakCircularReference f =
    f ()



----
--- Combinator
--


oneOf : List (Parser t i o) -> Parser t i o
oneOf ps next input =
    case ps of
        [] ->
            Nothing

        p :: p_tail ->
            case p next input of
                Just stuff ->
                    Just stuff

                Nothing ->
                    oneOf p_tail next input


optional : Parser t i o -> Parser t i (Maybe o)
optional p next input =
    case p next input of
        Nothing ->
            Just ( Nothing, input )

        Just ( output, nextInput ) ->
            Just ( Just output, nextInput )


without : Parser t i o -> Parser t i ()
without p next input =
    case p next input of
        Nothing ->
            Just ( (), input )

        Just _ ->
            Nothing


t2 : Parser t i a -> Parser t i b -> Parser t i ( a, b )
t2 pa pb =
    do pa <| \a ->
    do pb <| \b ->
    return ( a, b )


t3 : Parser t i a -> Parser t i b -> Parser t i c -> Parser t i ( a, b, c )
t3 pa pb pc =
    do pa <| \a ->
    do pb <| \b ->
    do pc <| \c ->
    return ( a, b, c )


zeroOrMore : Parser t i o -> Parser t i (List o)
zeroOrMore p next =
    let
        recursive : List o -> i -> ( List o, i )
        recursive accum input =
            case p next input of
                Nothing ->
                    ( [], input )

                Just ( thing, nextInput ) ->
                    recursive (thing :: accum) nextInput
    in
    recursive [] >> Tuple.mapFirst List.reverse >> Just


oneOrMore : Parser t i o -> Parser t i ( o, List o )
oneOrMore p =
    t2 p (zeroOrMore p)
