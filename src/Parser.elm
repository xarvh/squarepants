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

The first argument is a function that extracts a single token from the input and updates the "read state" to reflect the fact that the token has been read.

The second argument is the current reading state.

For example, if your input is a String, you can use the string itself as your read state and `String.uncons` to extract the token:

    tryToParseExpression : String -> Result String Expression
    tryToParseExpression input =
        case someExpressionParserDefinedElsewhere String.uncons input of
            Just ( expression, [] ) ->
                Ok expression

            Just ( expression, remainingInput ) ->
                Err "I found an expression, but then there was some more stuff left!"

            Nothing ->
                Err "input does not contain an expression"

(Note: String.uncons might not be the fastest thing ever...)

If instead your input is an `Array Char`, you can use an Int as your read state:

    extractToken : Int -> Maybe ( Char, Int )
    extractToken currentPosition =
        myInputArrayOfChars
            |> Array.get currentPosition
            |> Maybe.map (\char -> ( char, currentPosition + 1 ))

You can use more complicated read states to keep track of the context, for example if you need to parse indentation.

-}



-- TODO rename `input` to `readState`?


type alias Parser token input output =
    (input -> Maybe ( token, input )) -> input -> Maybe ( output, input )



{-
   type alias Combinators =
       { do : ...
       , oneOf : ...
       }
-}
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
