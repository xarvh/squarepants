module Parser exposing (..)

{-| A monad is a very general way to structure a computation.

`return` is the trivial computation that produces the argument as result.

`bind` is how you chain the result of a computation to another computation.

`zero` is the value that is independent of type (Nothing, [], ...)

`Result` is a monad without a zero?

-}

-- TODO? separedBy / separedBy1
--
-- TODO bracket
--
{- TODO "Debug mode"

   It should collect:

   * the current path of combinators
       --> this means that each combinator should be able to add its name to the stack, how do I do this?

   * the current input head

   It may require a closure to redefine all combinators in a way that saves the non-debug-mode performance.

-}
{- TODO Do we need these?

   breakCircularReference : (() -> Parser t i b) -> Parser t i b
   breakCircularReference f =
       f ()


   sat : (token -> Bool) -> Parser token readState token
   sat test =
       do consumeOne <| \token ->
       if test token then
           succeed token

       else
           fail


   map : (a -> b) -> Parser token input a -> Parser token input b
   map f p =
     do p <| \a ->
       succeed (f a)

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
type alias Parser token readState output =
    (readState -> Maybe ( token, readState )) -> readState -> Maybe ( output, readState )



----
--- Primitives
--
--
-- They are aware of `getNext` and `readState`


{-| Always fail
-}
fail : Parser token readState output
fail =
    \getNext readState -> Nothing


{-| Always succeed, without consuming any input
-}
succeed : a -> Parser token readState a
succeed a =
    \getNext readState -> Just ( a, readState )


{-| Consume and return the next token
-}
consumeOne : Parser token readState token
consumeOne =
    \getNext readState -> getNext readState


{-| -}
doWithDefault : Parser t i b -> Parser t i a -> (a -> Parser t i b) -> Parser t i b
doWithDefault fallbackParser firstParser chainedParser =
    \getNext readState ->
        case firstParser getNext readState of
            Nothing ->
                fallbackParser getNext readState

            Just ( a, nextReadState ) ->
                chainedParser a getNext nextReadState



----
--- Base combinators
--


{-| Parse something and if successful, use the result to produce another parser.

If you talk Elm, this is `andThen` with its arguments flipped, so that you can write

    do parseThing <| \thing ->
    do parseItem <| \item ->
    succeed (buildTree thing item)

If you talk monads, this is the monadic `bind`, Haskell's `>>=`.

-}
do : Parser t i a -> (a -> Parser t i b) -> Parser t i b
do =
    doWithDefault fail


fromFn : (token -> Maybe output) -> Parser token readState output
fromFn f =
    do consumeOne <| \token ->
    case f token of
        Just output ->
            succeed output

        Nothing ->
            fail



----
--- Combinators
--


without : Parser t i o -> Parser t i ()
without p =
    doWithDefault (succeed ()) p (\_ -> fail)


end : Parser token input ()
end =
    without consumeOne


oneOf : List (Parser t i o) -> Parser t i o
oneOf ps =
    case ps of
        [] ->
            fail

        p :: p_tail ->
            doWithDefault (oneOf p_tail) p succeed


optional : Parser t i o -> Parser t i (Maybe o)
optional p =
    doWithDefault (succeed Nothing) p (Just >> succeed)


t2 : Parser t i a -> Parser t i b -> Parser t i ( a, b )
t2 pa pb =
    do pa <| \a ->
    do pb <| \b ->
    succeed ( a, b )


t3 : Parser t i a -> Parser t i b -> Parser t i c -> Parser t i ( a, b, c )
t3 pa pb pc =
    do pa <| \a ->
    do pb <| \b ->
    do pc <| \c ->
    succeed ( a, b, c )


zeroOrMore : Parser t i o -> Parser t i (List o)
zeroOrMore p =
    doWithDefault (succeed []) p <| \head ->
    do (zeroOrMore p) <| \tail ->
    succeed (head :: tail)


oneOrMore : Parser t i o -> Parser t i ( o, List o )
oneOrMore p =
    t2 p (zeroOrMore p)
