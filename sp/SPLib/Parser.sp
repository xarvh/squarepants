[#

The Parser is a function that reads an input and tries to match it to a specific pattern.

Arguments:

  1. rejectedStates: a list of the states that could not be parsed. Should help understandig what went wrong?

  1. readstate: the current reading state

#]
alias Parser token output =
    Rejections token: (State token): Rejections token & Outcome token output


alias State token = [token]
alias Rejections token = [State token]

union Outcome token output =
    , Accepted (State token) output
    , Rejected
    , Aborted (State token) Text


runParser parser readState =
    as Parser token output: State token: Rejections token & Outcome token output

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
abort error =
    as Text: Parser token output

    fn rejections readState:
        rejections & Aborted readState error


[#| Reject a read state
#]
# TODO can take one more function to actually put together the rejected states, and maybe merge it with the error type?
reject =
    as Parser token output

    fn rejections readState:
        (readState :: rejections) & Rejected


[#| Accept the read state, without consuming any input
#]
accept a =
    as a: Parser token a

    fn rejections readState:
        rejections & Accepted readState a


[#| Consume and return the next token
#]
consumeOne =
    as Parser token token

    fn rejections readState:
        try readState as
            []:
                (readState :: rejections) & Rejected

            token :: nextState:
                rejections & Accepted nextState token


[#| Parse something and if accepted, use the result to produce another parser.

If you talk monads, this is the monadic `bind`, Haskell's `>>=`.

#]
then chainedParser firstParser =
    as (a: Parser t b): Parser t a: Parser t b

    fn re0 readState:
        try firstParser re0 readState as
            re1 & Accepted nextReadState a:
                chainedParser a re1 nextReadState

            re1 & Rejected:
                re1 & Rejected

            re1 & Aborted rs e:
                re1 & Aborted rs e


[#| #]
thenWithDefault fallbackParser chainedParser firstParser =
    as Parser t b: (a: Parser t b): Parser t a: Parser t b

    fn re0 readState:
        try firstParser re0 readState as
            re1 & Aborted rs reason:
                re1 & Aborted rs reason

            re1 & Rejected:
                fallbackParser re1 readState

            re1 & Accepted nextReadState a:
                chainedParser a re1 nextReadState


[#| Pulls out the current state
#]
here =
    as Parser t readState

    fn rejections readState:
        rejections & Accepted readState readState



#
# Base combinators
#


map f p =
    as (a: b): Parser token a: Parser token b

    p >> then fn b:
    accept (f b)


without p =
    as Parser t o: Parser t None

    p >> thenWithDefault (accept None) fn _:
    reject


end =
    as Parser t None

    without consumeOne


oneOf parsers =
    as [Parser t o]: Parser t o

    rec rejections readState ps =
        as [i]: i: [Parser t o]: [i] & Outcome t o

        try ps as
            []:
                rejections & Rejected

            headParser :: tailParsers:
                try headParser rejections readState as
                    re1 & Rejected:
                        rec re1 readState tailParsers

                    acceptedOrAborted:
                        acceptedOrAborted

    fn rejections readState:
        rec rejections readState parsers


maybe p =
    as Parser t o: Parser t (Maybe o)

    p >> thenWithDefault (accept Nothing) fn x:
    accept << Just x


tuple2 pa pb =
    as Parser t a: Parser t b: Parser t ( a & b )

    pa >> then fn a:
    pb >> then fn b:
    accept ( a & b )


tuple3 pa pb pc =
    as Parser t a: Parser t b: Parser t c: Parser t ( a & b & c )

    pa >> then fn a:
    pb >> then fn b:
    pc >> then fn c:
    accept ( a & b & c )


zeroOrMore p =
    as Parser t o: Parser t [o]

    p >> thenWithDefault (accept []) fn head:
    zeroOrMore p >> then fn tail:
    accept (head :: tail)


oneOrMore p =
    as Parser t o: Parser t ( o & [o] )

    tuple2 p (zeroOrMore p)



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
expression term ops =
    as Parser t o: [Parser t o: Parser t o]: Parser t o

    try ops as
        []:
            term

        op :: rest:
            expression (op term) rest


higherOr parser higher =
    as Parser t o: Parser t o: Parser t o

    oneOf [ higher, parser ]


surroundWith left right parser =
    as Parser t ignoredOutput1: Parser t ignoredOutput2: Parser t output: Parser t output

    left >> then fn _:
    parser >> then fn p:
    right >> then fn _:
    accept p

