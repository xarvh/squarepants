[#

The Parser is a function that reads an input and tries to match it to a specific pattern.

Arguments:

  1. getNext: a function that extracts a single token from the input and updates the "read state" to reflect the fact that the token has been read.

  2. rejectedStates: a list of the states that could not be parsed. Should help understandig what went wrong?

  3. readstate: the current reading state

#]
alias Parser token readState error output =
    GetNext token readState: [readState]: readState: [readState] & Outcome readState error output


alias GetNext token readState =
    readState: Maybe ( token & readState )


union Outcome readState error output =
    , Accepted readState output
    , Rejected
    , Aborted readState error


runParser parser getNext readState =
    as Parser token readState error output: GetNext token readState: readState: [readState] & Outcome readState error output

    parser getNext [ readState ] readState


[#
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
    as error: Parser token readState error output

    fn getNext rejections readState:
        rejections & Aborted readState error


[#| Reject a read state
#]
# TODO can take one more function to actually put together the rejected states, and maybe merge it with the error type?
reject =
    as Parser token readState error output

    fn getNext rejections readState:
        (readState :: rejections) & Rejected


[#| Accept the read state, without consuming any input
#]
accept a =
    as a: Parser token readState error a

    fn getNext rejections readState:
        rejections & Accepted readState a


[#| Consume and return the next token
#]
consumeOne =
    as Parser token readState error token

    fn getNext rejections readState:
        try getNext readState as
            Nothing:
                (readState :: rejections) & Rejected

            Just ( token & nextState ):
                rejections & Accepted nextState token


[#| Parse something and if accepted, use the result to produce another parser.

If you talk monads, this is the monadic `bind`, Haskell's `>>=`.

#]
then chainedParser firstParser =
    as (a: Parser t i e b): Parser t i e a: Parser t i e b

    fn getNext re0 readState:
        try firstParser getNext re0 readState as
            re1 & Accepted nextReadState a:
                chainedParser a getNext re1 nextReadState

            re1 & Rejected:
                re1 & Rejected

            re1 & Aborted rs e:
                re1 & Aborted rs e


[#| #]
thenWithDefault fallbackParser firstParser chainedParser =
    as Parser t i e b: Parser t i e a: (a: Parser t i e b): Parser t i e b

    fn getNext re0 readState:
        try firstParser getNext re0 readState as
            re1 & Aborted rs reason:
                re1 & Aborted rs reason

            re1 & Rejected:
                fallbackParser getNext re1 readState

            re1 & Accepted nextReadState a:
                chainedParser a getNext re1 nextReadState


[#| Pulls out the current state
#]
here =
    as Parser t readState e readState

    fn getNext rejections readState:
        rejections & Accepted readState readState



#
# Base combinators
#


map f p =
    as (a: b): Parser token input e a: Parser token input e b

    p >> then fn b:
    accept (f b)


without p =
    as Parser t i e o: Parser t i e None

    p >> thenWithDefault (accept None) fn _:
    reject


end =
    as Parser t i e None

    without consumeOne


oneOf parsers getNext =
    as [Parser t i e o]: Parser t i e o

    rec rejections readState ps =
        as [i]: i: [Parser t i e o]: [i] & Outcome e o

        try ps as
            []:
                rejections & Rejected

            headParser :: tailParsers:
                try headParser getNext rejections readState as
                    re1 & Rejected:
                        rec re1 readState tailParsers

                    acceptedOrAborted:
                        acceptedOrAborted

    fn rejections readState:
        rec rejections readState parsers


maybe p =
    as Parser t i e o: Parser t i e (Maybe o)

    p >> thenWithDefault (accept Nothing) fn x:
    accept << Just x


tuple2 pa pb =
    as Parser t i e a: Parser t i e b: Parser t i e ( a & b )

    pa >> then fn a:
    pb >> then fn b:
    accept ( a & b )


tuple3 pa pb pc =
    as Parser t i e a: Parser t i e b: Parser t i e c: Parser t i e ( a & b & c )

    pa >> then fn a:
    pb >> then fn b:
    pc >> then fn c:
    accept ( a & b & c )


zeroOrMore p =
    as Parser t i e o: Parser t i e [o]

    p >> thenWithDefault (accept []) fn head:
    zeroOrMore p >> then fn tail:
    accept (head :: tail)


oneOrMore p =
    as Parser t i e o: Parser t i e ( o & [o] )

    tuple2 p (zeroOrMore p)



[# If P allowed cyclic values in let expressions, we could use this


   type alias ExpressionArgs t i o ignored =
       { term : Parser t i e o
       , openParen : Parser t i e ignored
       , closedParen : Parser t i e ignored
       , ops : [Parser t i e o: Parser t i e o]
       }


   expression : ExpressionArgs t i o ignored: Parser t i e o
   expression args =
       let
           parens : Parser t i e o: Parser t i e o
           parens higher =
               surroundWith args.openParen args.closedParen (do (accept None) <| \_: expr)

           expr : Parser t i e o
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
    as Parser t i e o: [Parser t i e o: Parser t i e o]: Parser t i e o

    try ops as
        []:
            term

        op :: rest:
            expression (op term) rest


higherOr parser higher =
    as Parser t i e o: Parser t i e o: Parser t i e o

    oneOf [ higher, parser ]


surroundWith left right parser =
    as Parser t i e ignoredOutput1: Parser t i e ignoredOutput2: Parser t i e output: Parser t i e output

    left >> then fn _:
    parser >> then fn p:
    right >> then fn _:
    accept p
    #]
