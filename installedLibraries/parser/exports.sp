
module =
    path = Parser
    exposes =
        Parser
        Rejections

        Outcome
        'accepted
        'rejected
        'aborted

        runParser
        andThen
        accept
        reject
        maybe
        here
        thenWithDefault
        surroundWith
        zeroOrMore
        oneOf
        abort
        breakCircularDefinition
