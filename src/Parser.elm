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



      type alias Parser token readState output =
          { name : List String
          , getNext : (readState -> Maybe ( token, readState ))
          }
          ->
          -> readState
          -> trackState
          -> (trackState, Maybe ( output, readState ))


      type alias TrackState =
        {.....?






-}
{- TODO Do we need these?



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
    GetNext token readState
    -> List String
    -> readState
    -> Outcome readState output


type alias GetNext token readState =
    readState -> Maybe ( token, readState )


type Outcome readState output
    = Success output readState
    | Failure
    | Abort String


parse : Parser token readState output -> GetNext token readState -> readState -> Result String output
parse parser getNext readState =
    case parser getNext [] readState of
        Success output finalState ->
            Ok output

        Failure ->
            Err "doesn't make sense"

        Abort reason ->
            Err reason



----
--- Primitives
--
--
-- These functions are aware of the internal structure of the parser (ie, it's a function)


{-| Abort

This is a fatal error that will interrupt the whole execution

-}
abort : String -> Parser token readState output
abort reason =
    \getNext path readState ->
        Abort reason


{-| Always fail
-}
fail : Parser token readState output
fail =
    \getNext path readState ->
        Failure


{-| Always succeed, without consuming any input
-}
succeed : a -> Parser token readState a
succeed a =
    \getNext path readState ->
        Success a readState


{-| Consume and return the next token
-}
consumeOne : Parser token readState token
consumeOne =
    \getNext path readState ->
        case getNext readState of
            Nothing ->
                Failure

            Just ( token, nextState ) ->
                Success token nextState


{-| -}
doWithDefault : Parser t i b -> Parser t i a -> (a -> Parser t i b) -> Parser t i b
doWithDefault fallbackParser firstParser chainedParser =
    \getNext path readState ->
        case firstParser getNext path readState of
            Abort reason ->
                Abort reason

            Failure ->
                fallbackParser getNext path readState

            Success a nextReadState ->
                chainedParser a getNext path nextReadState


doWithDebug : ({ path : List String, first : Outcome i a } -> discarded) -> String -> Parser t i a -> (a -> Parser t i b) -> Parser t i b
doWithDebug log name firstParser chainedParser =
    \getNext p readState ->
        let
            path =
                p ++ [ name ]

            out =
                firstParser getNext path readState

            _ =
                log { path = path, first = out }
        in
        case out of
            Abort reason ->
                Abort reason

            Failure ->
                fail getNext path readState

            Success a nextReadState ->
                chainedParser a getNext path nextReadState


updState : (readState -> readState) -> Parser t readState readState
updState upd =
    \getNext path readState ->
        let
            newReadState =
                upd readState
        in
        Success newReadState newReadState



----
--- Base combinators
--


getState : Parser t i i
getState =
    updState identity


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



----
--- Common combinators
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


tuple2 : Parser t i a -> Parser t i b -> Parser t i ( a, b )
tuple2 pa pb =
    do pa <| \a ->
    do pb <| \b ->
    succeed ( a, b )


tuple3 : Parser t i a -> Parser t i b -> Parser t i c -> Parser t i ( a, b, c )
tuple3 pa pb pc =
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
    tuple2 p (zeroOrMore p)



{- If Elm allowed cyclic values in let expressions, we could use this


   type alias ExpressionArgs t i o ignored =
       { term : Parser t i o
       , openParen : Parser t i ignored
       , closedParen : Parser t i ignored
       , ops : List (Parser t i o -> Parser t i o)
       }


   expression : ExpressionArgs t i o ignored -> Parser t i o
   expression args =
       let
           parens : Parser t i o -> Parser t i o
           parens higher =
               oneOf
                   [ higher
                   , surroundWith args.openParen args.closedParen (do (succeed ()) <| \_ -> expr)
                   ]

           expr : Parser t i o
           expr =
               expressionRec args.term (parens :: args.ops)
       in
       expr


-}


{-| This is how you put together an expression so that you avoid left recursion and set your operations precedence

<https://github.com/glebec/left-recursion>

<https://stackoverflow.com/a/4165483>

-}
expression : Parser t i o -> List (Parser t i o -> Parser t i o) -> Parser t i o
expression term ops =
    case ops of
        [] ->
            term

        op :: rest ->
            expression (op term) rest


surroundWith : Parser t i ignoredOutput1 -> Parser t i ignoredOutput2 -> Parser t i output -> Parser t i output
surroundWith left right parser =
    do left <| \_ ->
    do parser <| \p ->
    do right <| \_ ->
    succeed p


breakCircularDefinition : (() -> Parser t i b) -> Parser t i b
breakCircularDefinition a =
    -- TODO: this is equivalent to `a ()` but if I use that the function doesn't break circular definitions any more
    do (succeed ()) a
