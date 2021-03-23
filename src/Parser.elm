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
type alias Parser token readState error output =
    GetNext token readState
    -> List String
    -> readState
    -> Outcome readState error output


type alias GetNext token readState =
    readState -> Maybe ( token, readState )


type Outcome readState error output
    = Success readState output
    | Failure readState
    | Abort readState error


parse : Parser token readState error output -> GetNext token readState -> readState -> Outcome readState error output
parse parser getNext readState =
    parser getNext [] readState



----
--- Primitives
--
--
-- These functions are aware of the internal structure of the parser (ie, it's a function)
--
-- TODO rename to `accept`, `reject`, `abort`
-- TODO allow `abort` to produce any type of error, not only string


{-| Abort

This is a fatal error that will interrupt the whole execution

-}
abort : error -> Parser token readState error output
abort error =
    \getNext path readState ->
        Abort readState error


{-| Always fail
-}
fail : Parser token readState error output
fail =
    \getNext path readState ->
        Failure readState


{-| Always succeed, without consuming any input
-}
succeed : a -> Parser token readState error a
succeed a =
    \getNext path readState ->
        Success readState a


{-| Consume and return the next token
-}
consumeOne : Parser token readState error token
consumeOne =
    \getNext path readState ->
        case getNext readState of
            Nothing ->
                Failure readState

            Just ( token, nextState ) ->
                Success nextState token


{-| -}
doWithDefault : Parser t i e b -> Parser t i e a -> (a -> Parser t i e b) -> Parser t i e b
doWithDefault fallbackParser firstParser chainedParser =
    \getNext path readState ->
        case firstParser getNext path readState of
            Abort rs reason ->
                Abort rs reason

            Failure _ ->
                fallbackParser getNext path readState

            Success nextReadState a ->
                chainedParser a getNext path nextReadState


doWithDebug : ({ path : List String, first : Outcome i e a } -> discarded) -> String -> Parser t i e a -> (a -> Parser t i e b) -> Parser t i e b
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
            Abort rs reason ->
                Abort rs reason

            Failure _ ->
                fail getNext path readState

            Success nextReadState a ->
                chainedParser a getNext path nextReadState


updState : (readState -> readState) -> Parser t readState e readState
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


map : (a -> b) -> Parser token input e a -> Parser token input e b
map f p =
    do p <| \a ->
    succeed (f a)


getState : Parser t i e i
getState =
    updState identity


{-| Parse something and if successful, use the result to produce another parser.

If you talk Elm, this is `andThen` with its arguments flipped, so that you can write

    do parseThing <| \thing ->
    do parseItem <| \item ->
    succeed (buildTree thing item)

If you talk monads, this is the monadic `bind`, Haskell's `>>=`.

-}
do : Parser t i e a -> (a -> Parser t i e b) -> Parser t i e b
do =
    doWithDefault fail



----
--- Common combinators
--


without : Parser t i e o -> Parser t i e ()
without p =
    doWithDefault (succeed ()) p (\_ -> fail)


end : Parser t i e ()
end =
    without consumeOne


oneOf : List (Parser t i e o) -> Parser t i e o
oneOf ps =
    case ps of
        [] ->
            fail

        p :: p_tail ->
            doWithDefault (oneOf p_tail) p succeed


maybe : Parser t i e o -> Parser t i e (Maybe o)
maybe p =
    doWithDefault (succeed Nothing) p (Just >> succeed)


tuple2 : Parser t i e a -> Parser t i e b -> Parser t i e ( a, b )
tuple2 pa pb =
    do pa <| \a ->
    do pb <| \b ->
    succeed ( a, b )


tuple3 : Parser t i e a -> Parser t i e b -> Parser t i e c -> Parser t i e ( a, b, c )
tuple3 pa pb pc =
    do pa <| \a ->
    do pb <| \b ->
    do pc <| \c ->
    succeed ( a, b, c )


zeroOrMore : Parser t i e o -> Parser t i e (List o)
zeroOrMore p =
    doWithDefault (succeed []) p <| \head ->
    do (zeroOrMore p) <| \tail ->
    succeed (head :: tail)


oneOrMore : Parser t i e o -> Parser t i e ( o, List o )
oneOrMore p =
    tuple2 p (zeroOrMore p)



{- If Elm allowed cyclic values in let expressions, we could use this


   type alias ExpressionArgs t i o ignored =
       { term : Parser t i e o
       , openParen : Parser t i e ignored
       , closedParen : Parser t i e ignored
       , ops : List (Parser t i e o -> Parser t i e o)
       }


   expression : ExpressionArgs t i o ignored -> Parser t i e o
   expression args =
       let
           parens : Parser t i e o -> Parser t i e o
           parens higher =
               oneOf
                   [ higher
                   , surroundWith args.openParen args.closedParen (do (succeed ()) <| \_ -> expr)
                   ]

           expr : Parser t i e o
           expr =
               expressionRec args.term (parens :: args.ops)
       in
       expr


-}


{-| This is how you put together an expression so that you avoid left recursion and set your operations precedence

<https://github.com/glebec/left-recursion>

<https://stackoverflow.com/a/4165483>

-}
expression : Parser t i e o -> List (Parser t i e o -> Parser t i e o) -> Parser t i e o
expression term ops =
    case ops of
        [] ->
            term

        op :: rest ->
            expression (op term) rest


surroundWith : Parser t i e ignoredOutput1 -> Parser t i e ignoredOutput2 -> Parser t i e output -> Parser t i e output
surroundWith left right parser =
    do left <| \_ ->
    do parser <| \p ->
    do right <| \_ ->
    succeed p



{- TODO
   oomSeparatedBy : Parser t i e ignoredOutput -> Parser t i e output -> Parser t i e (output, List output)
   oomSeparatedBy separator parser =
       do parser <| \_ ->
       do parser <| \p ->
       do right <| \_ ->
       succeed p
-}


breakCircularDefinition : (() -> Parser t i e b) -> Parser t i e b
breakCircularDefinition a =
    -- This is equivalent to `a ()` but we need the lambda contained inside `do` to actually break the circula def
    do (succeed ()) a
