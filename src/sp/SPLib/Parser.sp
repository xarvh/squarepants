[#

The Parser is a function that reads an input and tries to match it to a specific pattern.

Arguments:

  1. rejectedStates: a list of the states that could not be parsed. Should help understandig what went wrong?

  1. readstate: the current reading state

#]
alias Parser token output =
    [[token]]: [token]: [[token]] & Outcome token output


#alias State token = [token]
#alias Rejections token = [State token]

union Outcome token output =
    , Accepted [token] output
    , Rejected
    , Aborted [token] Text


runParser as Parser token output: [token]: [[token]] & Outcome token output =
    parser: readState:
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
abort as Text: Parser token output =
    error:
    rejections: readState:
    rejections & Aborted readState error


[#| Reject a read state
#]
# TODO can take one more function to actually put together the rejected states, and maybe merge it with the error type?
reject as Parser token output =
    rejections: readState:
    (readState :: rejections) & Rejected


[#| Accept the read state, without consuming any input
#]
accept as a: Parser token a =
    a:
    rejections: readState:
        rejections & Accepted readState a


[#| Consume and return the next token
#]
consumeOne as Parser token token =
    rejections: readState:
    try readState as
        []:
            (readState :: rejections) & Rejected

        token :: nextState:
            rejections & Accepted nextState token


[#| Parse something and if accepted, use the result to produce another parser.

If you talk monads, this is the monadic `bind`, Haskell's `>>=`.

#]
andThen as (a: Parser t b): Parser t a: Parser t b =
    chainedParser: firstParser:
    re0: readState:
    try firstParser re0 readState as
        re1 & Accepted nextReadState a:
            chainedParser a re1 nextReadState

        re1 & Rejected:
            re1 & Rejected

        re1 & Aborted rs e:
            re1 & Aborted rs e


[#| #]
thenWithDefault as Parser t b: (a: Parser t b): Parser t a: Parser t b =
    fallbackParser: chainedParser: firstParser:
    re0: readState:
    try firstParser re0 readState as
        re1 & Aborted rs reason:
            re1 & Aborted rs reason

        re1 & Rejected:
            fallbackParser re1 readState

        re1 & Accepted nextReadState a:
            chainedParser a re1 nextReadState


[#| Pulls out the current state
#]
here as Parser t [t] =
    rejections: readState:
        rejections & Accepted readState readState


#
# Base combinators
#


map as (a: b): Parser token a: Parser token b =
    f: p:
    p >> andThen b:
    accept (f b)


without as Parser t o: Parser t None =
    p:
    p >> thenWithDefault (accept None) _:
    reject


end as Parser t None =
    without consumeOne


oneOf as [Parser t o]: Parser t o =
    ps: rejections: readState:

    try ps as
        []:
            rejections & Rejected

        headParser :: tailParsers:
            try headParser rejections readState as
                re1 & Rejected:
                    oneOf tailParsers re1 readState

                acceptedOrAborted:
                    acceptedOrAborted


maybe as Parser t o: Parser t (Maybe o) =
    p:
    p >> thenWithDefault (accept Nothing) x:
    accept << Just x


tuple2 as Parser t a: Parser t b: Parser t ( a & b ) =
    pa: pb:
    pa >> andThen a:
    pb >> andThen b:
    accept ( a & b )


tuple3 as Parser t a: Parser t b: Parser t c: Parser t ( a & b & c ) =
    pa: pb: pc:
    pa >> andThen a:
    pb >> andThen b:
    pc >> andThen c:
    accept ( a & b & c )


zeroOrMore as Parser t o: Parser t [o] =
    p:
    p >> thenWithDefault (accept []) head:
    zeroOrMore p >> andThen tail:
    accept (head :: tail)


oneOrMore as Parser t o: Parser t ( o & [o] ) =
    p:
    tuple2 p (zeroOrMore p)


# This is used so that `expression` functions don't keep calling themselves right at their definition
breakCircularDefinition as (None: Parser t o): Parser t o =
    a:
    accept None >> andThen a


[# If SP allowed cyclic values in let expressions, we could use this


   type alias ExpressionArgs t i o ignored =
       { term : Parser t o
       , openParen : Parser t ignored
       , closedParen : Parser t ignored
       , ops : [Parser t o: Parser t o]
       }


   expression : ExpressionArgs t i o ignored: Parser t o
   expression args =
       let
           parens : Parser t o: Parser t o
           parens higher =
               surroundWith args.openParen args.closedParen (do (accept None) <| \_: expr)

           expr : Parser t o
           expr =
               expressionRec args.term (higherOr parens :: args.ops)
       in
       expr

#]


[#| This is how you put together an expression so that you avoid left recursion and set your operations precedence

<https://github.com/glebec/left-recursion>

<https://stackoverflow.com/a/4165483>

#]
expression as Parser t o: [Parser t o: Parser t o]: Parser t o =
    term: ops:
    try ops as
        []:
            term

        op :: rest:
            expression (op term) rest


higherOr as Parser t o: Parser t o: Parser t o =
    parser: higher:
    oneOf [ higher, parser ]


surroundWith as Parser t ignoredOutput1: Parser t ignoredOutput2: Parser t output: Parser t output =
    left: right: parser:
    left >> andThen _:
    parser >> andThen p:
    right >> andThen _:
    accept p

