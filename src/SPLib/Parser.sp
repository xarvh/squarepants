[#

The Parser is a function that reads an input and tries to match it to a specific pattern.

Arguments:

  1. rejectedStates: a list of the states that could not be parsed. Should help understandig what went wrong?

  1. readstate: the current reading state

#]
alias Parser state output =
    fn Rejections state, state: Rejections state & Outcome state output


alias Rejections state =
    [state]


union Outcome state output =
    , Accepted state output
    , Rejected
    # `Text` should be a variable `error`
    , Aborted state Text


runParser as fn Parser state output, state: Rejections state & Outcome state output =
    fn parser, readState:
    parser [ readState ] readState


#
# Primitives
#
#
# These functions are aware of the internal structure of the parser (ie, it's a function)
#


[# This is a fatal error that will interrupt the whole execution.

Use as little as possible, ideally the parser should always accept.

#]
abort as fn Text: Parser state output =
    fn error:
    fn rejections, readState:
    rejections & Aborted readState error


[#| Reject a read state
#]
# TODO can take one more function to actually put together the rejected states, and maybe merge it with the error type?
reject as Parser state output =
    fn rejections, readState:
    (readState :: rejections) & Rejected


[#| Accept the read state, without consuming any input
#]
accept as fn a: Parser state a =
    fn a:
    fn rejections, readState:
        rejections & Accepted readState a


[#| Parse something and if accepted, use the result to produce another parser.

If you talk monads, this is the monadic `bind`, Haskell's `>>=`.

#]
andThen as fn (fn a: Parser t b): fn Parser t a: Parser t b =
    fn chainedParser:
    fn firstParser:
    fn re0, readState:
    try firstParser re0 readState as
        , re1 & Accepted nextReadState a:
            (chainedParser a) re1 nextReadState

        , re1 & Rejected:
            re1 & Rejected

        , re1 & Aborted rs e:
            re1 & Aborted rs e


[#| #]
thenWithDefault as fn Parser t b, (fn a: Parser t b): fn Parser t a: Parser t b =
    fn fallbackParser, chainedParser:
    fn firstParser:
    fn re0, readState:
    try firstParser re0 readState as
        , re1 & Aborted rs reason:
            re1 & Aborted rs reason

        , re1 & Rejected:
            fallbackParser re1 readState

        , re1 & Accepted nextReadState a:
            (chainedParser a) re1 nextReadState


[#| Pulls out the current state
#]
here as Parser t t =
    fn rejections, readState:
        rejections & Accepted readState readState


#
# Base combinators
#


map as fn (fn a: b), Parser state a: Parser state b =
    fn f, p:
    p >> andThen fn b:
    accept (f b)


without as fn Parser t o: Parser t None =
    fn p:
    p >> thenWithDefault (accept None) fn _:
    reject


[# DOC

`oneOf` can become very inefficient; consider:

    evaluation: expr
    definition: expr '=' expr

    statement = oneOf [ evaluation, definition ]

The `statement` parser will parse the first `expr` twice!

It is much faster to have:

    expr ('=' expr)?

and then decide whether it is an evaluation or a definition depending on whether the second chunk is there or not.

#]
oneOf as fn [Parser t o]: Parser t o =
    fn ps:
    fn rejections, readState:

    try ps as
        , []:
            rejections & Rejected

        , headParser :: tailParsers:
            try headParser rejections readState as
                , re1 & Rejected:
                    (oneOf tailParsers) re1 readState

                , acceptedOrAborted:
                    acceptedOrAborted


maybe as fn Parser t o: Parser t (Maybe o) =
    fn p:
    p >> thenWithDefault (accept Nothing) fn x:
    accept << Just x


tuple2 as fn Parser t a, Parser t b: Parser t ( a & b ) =
    fn pa, pb:
    pa >> andThen fn a:
    pb >> andThen fn b:
    accept ( a & b )


tuple3 as fn Parser t a, Parser t b, Parser t c: Parser t ( a & b & c ) =
    fn pa, pb, pc:
    pa >> andThen fn a:
    pb >> andThen fn b:
    pc >> andThen fn c:
    accept ( a & b & c )


zeroOrMore as fn Parser t o: Parser t [o] =
    fn p:
    p >> thenWithDefault (accept []) fn head:
    zeroOrMore p >> andThen fn tail:
    accept (head :: tail)


oneOrMore as fn Parser t o: Parser t ( o & [o] ) =
    fn p:
    tuple2 p (zeroOrMore p)


# This is used so that `expression` functions don't keep calling themselves right at their definition
breakCircularDefinition as fn (fn None: Parser t o): Parser t o =
    fn a:
    accept None >> andThen a


[#| This is how you put together an expression so that you avoid left recursion and set your operations precedence

<https://github.com/glebec/left-recursion>

<https://stackoverflow.com/a/4165483>


TODO this actually make the parsing super slow.
Should get the trail (Pratt?) parsing in here instead.


expression as fn Parser t o, [fn Parser t o: Parser t o]: Parser t o =
    fn term, ops:
    try ops as
        , []:
            term

        , op :: rest:
            expression (op term) rest


higherOr as fn Parser t o: fn Parser t o: Parser t o =
    fn parser: fn higher:
    oneOf [ higher, parser ]
#]


surroundWith as fn Parser t ignoredOutput1, Parser t ignoredOutput2, Parser t output: Parser t output =
    fn left, right, parser:
    left >> andThen fn _:
    parser >> andThen fn p:
    right >> andThen fn _:
    accept p

