Env =
    {
    , errorModule as Error.Module
    , keepComments as Bool
    , stripLocations as Bool
    }


ReadState =
    {
    # All comments are forced into statements
    # If we wanted the formatter to support also inline comments, I imagine we'd add a `commentInlines as [FA.Comment]`?
    , commentsReversed as [ FA.Comment ]
    , tokens as [ Token ]
    }


Parser a =
    Parser.Parser ReadState a


eatComments as fn ReadState: ReadState =
    fn readState:
    try readState.tokens as

        [ 'token start end (Token.'comment { indent, isBlock, isFollowedByBlank }), tail... ]:
            {
            , commentsReversed =
                [
                , { end, indent, isBlock, isFollowedByBlank, start }
                , readState.commentsReversed...
                ]
            , tokens = tail
            }
            >> eatComments

        _:
            readState


oneToken as Parser Token =
    fn rejections, rs:
    readState =
        eatComments rs

    try readState.tokens as
        []: [ readState, rejections... ] & Parser.'rejected
        [ token, tail... ]: rejections & Parser.'accepted { readState with tokens = tail } token


pullCommentsReversed as Parser [ FA.Comment ] =
    fn rejections, rs:
    readState =
        eatComments rs

    rejections & Parser.'accepted { readState with commentsReversed = [] } readState.commentsReversed


#
# Helpers
#
on as fn fn a: Parser b: fn Parser a: Parser b =
    Parser.andThen


ok as fn a: Parser a =
    Parser.accept


maybe as fn Parser a: Parser (Maybe a) =
    Parser.maybe


here as Parser Int =
    Parser.here
    >> on fn readState:
    try readState.tokens as
        [ 'token start end _, rest... ]: start
        []: 0
    >> ok


# TODO rename to mkpos?
pos as fn Env, Int, Int: Pos =
    fn env, start, end:
    if env.stripLocations then
        Pos.'t
    else
        Pos.'p start end


mkLine as fn Env, Int: Int =
    fn env, line:
    if env.stripLocations then
        -1
    else
        line


#
# Utility
#
forZeroOrMore as fn o, fn o: Parser o: Parser o =
    fn init, getParser:
    getParser init >> Parser.thenWithDefault (ok init) (forZeroOrMore __ getParser)


# TODO rename to "exact"?
kind as fn Token.Kind: Parser Token =
    fn targetKind:
    oneToken
    >> on fn token:
    'token _ _ k =
        token

    if targetKind == k then
        ok token
    else
        Parser.reject


discardFirst as fn Parser a, Parser b: Parser b =
    fn a, b:
    a >> on (fn _: b)


discardSecond as fn Parser a, Parser b: Parser a =
    fn a, b:
    a
    >> on fn aa:
    b
    >> on fn _:
    ok aa


surroundStrict as fn Token.Kind, Token.Kind, Parser a: Parser a =
    fn left, right, p:
    Parser.surroundWith (kind left) (kind right) p


surroundMultiline as fn Token.Kind, Token.Kind, Parser a: Parser a =
    fn left, right, content:
    discardFirst (kind left) (inlineOrBelowOrIndented (discardSecond content (inlineOrBelowOrIndented (kind right))))


oomSeparatedBy as fn Parser a, Parser b: Parser [ b ] =
    fn sep, pa:
    pa
    >> on fn head:
    Parser.zeroOrMore (discardFirst sep pa)
    >> on fn tail:
    ok << head :: tail


[#| TODO make it more flexible

    x = [ a ]
    x = [, a ]
    x = [
      , a
      , b
      ]
    x = [
      , a, b
      , c, d
      ]

#]
rawList as fn Parser a: Parser [ a ] =
    fn item:
    sibsep =
        # TODO was:
        #             (Parser.maybe << kind Token.NewSiblingLine) >> on _:
        #             kind Token.Comma
        # but I didn't test it properly
        inlineOrBelowOrIndented << kind Token.'comma

    discardFirst (Parser.maybe sibsep) (oomSeparatedBy sibsep item)


lowerName as fn Env: Parser (Pos & Name) =
    fn env:
    oneToken
    >> on fn 'token start end k:
    try k as
        Token.'lowercase { attrPath = [], maybeModule = 'nothing, name }: ok (pos env start end & name)
        _: Parser.reject


upperName as fn Env: Parser (Pos & Name) =
    fn env:
    oneToken
    >> on fn 'token start end k:
    try k as
        Token.'uppercase { maybeModule = 'nothing, name }: ok (pos env start end & name)
        _: Parser.reject


#
# Indentation
#

[#|

    a >> b >> c

    a
        >> b
        >> c

    a
        >> b
        >> c

    a
        >> b
        >> c

#]
block as fn Parser a: Parser a =
    surroundStrict Token.'blockStart Token.'blockEnd __


sib as fn Parser a: Parser a =
    discardFirst (kind Token.'newSiblingLine) __


maybeNewLine as fn Parser a: Parser a =
    discardFirst (Parser.maybe (kind Token.'newSiblingLine)) __


inlineOrBelowOrIndented as fn Parser a: Parser a =
    fn p:
    Parser.oneOf
        [
        , block p
        , sib p
        , p
        ]


indentedOrInlineStatements as fn Env: Parser FA.Expression =
    fn env:
    [
    , block (siblingStatements env)
    , expr env
    ]
    >> Parser.oneOf


alignedOrInlineStatements as fn Env: Parser (FA.Layout & FA.Expression) =
    fn env:
    Parser.oneOf
        [
        , block (siblingStatements env) >> on (fn e: ok (FA.'indented & e))
        , sib (siblingStatements env) >> on (fn e: ok (FA.'aligned & e))
        , expr env >> on (fn e: ok (FA.'inline & e))
        ]


#
# Statement blocks
#

siblingStatements as fn Env: Parser FA.Expression =
    fn env:
    here
    >> on fn start:
    statementParser env []
    >> on fn acc:
    forZeroOrMore acc (fn a: discardFirst (kind Token.'newSiblingLine) (statementParser env a))
    >> on fn reversedStatements:
    here
    >> on fn end:
    try reversedStatements as

        [ FA.'evaluation e ]:
            ok e

        many:
            many
            >> List.reverse
            >> FA.'statements
            >> FA.'expression [] (pos env start end) __
            >> ok


#
# Statements
#
aliasDef as fn Env: Parser FA.Statement =
    fn env:
    upperName env
    >> on fn name:
    Parser.zeroOrMore (lowerName env)
    >> on fn args:
    kind Token.'defop
    >> on fn _:
    inlineOrBelowOrIndented (expr env)
    >> on fn type:
    {
    , args
    , name
    , type
    }
    # TODO use ty end instead
    >> FA.'aliasDef
    >> ok


unionDef as fn Env: Parser FA.Statement =
    fn env:
    variantKind as Token.Kind =
        {
        , attrPath = []
        , maybeModule = 'nothing
        , name = "var"
        }
        >> Token.'lowercase

    kind variantKind
    >> on fn _:
    upperName env
    >> on fn name:
    Parser.zeroOrMore (lowerName env)
    >> on fn args:
    kind Token.'defop
    >> on fn _:
    inlineOrBelowOrIndented (rawList (expr env))
    >> on fn constructors:
    {
    , args
    , constructors
    , name
    }
    >> FA.'unionDef
    >> ok


#
# Expression
#

var Ee =
    , 'e_full FA.Expression
    , 'e_under FA.Expr_


expressionWithUnambiguousStart as fn Env: Parser FA.Expression =
    fn env:
    pullCommentsReversed
    >> on fn commentsReversed:
    oneToken
    >> on fn 'token start end kk:
    expressionOk as fn FA.Expr_: Parser Ee =
        fn e: e >> 'e_under >> ok

    b =
        # We try all tokens that unambiguously mark the start of an expression
        try kk as

            Token.'lowercase { attrPath, maybeModule, name }:
                maybe (discardFirst (kind Token.'as) (expr env))
                >> on fn maybeType:
                {
                , attrPath
                , maybeModule
                , maybeType
                , name
                }
                >> FA.'lowercase
                >> expressionOk

            Token.'constructor pas:
                FA.'constructor pas >> expressionOk

            Token.'uppercase pas:
                FA.'uppercase pas >> expressionOk

            Token.'recordShorthand pas:
                FA.'recordShorthand pas >> expressionOk

            Token.'argumentPlaceholder:
                FA.'argumentPlaceholder >> expressionOk

            Token.'numberLiteral isPercent s:
                maybe (discardFirst (kind Token.'uniquenessPolymorphismBinop) (expr env))
                >> on fn maybeUniPoly:
                try maybeUniPoly as
                    'nothing: FA.'literalNumber isPercent s
                    'just exp: FA.'poly s exp
                # TODO FA.Poly should reject if isPercent!

                >> expressionOk

            Token.'textLiteral singleOrTriple s:
                FA.'literalText singleOrTriple s >> expressionOk

            Token.'roundParen Token.'open:
                discardSecond (inlineOrBelowOrIndented (expr env)) (inlineOrBelowOrIndented (kind (Token.'roundParen Token.'closed)))
                >> on fn e:
                ok ('e_full e)

            Token.'squareBracket openRow Token.'open:
                item as Parser (Bool & FA.Expression) =
                    expr env
                    >> on fn exp:
                    maybe (kind Token.'threeDots)
                    >> on fn maybeDots:
                    ok (maybeDots /= 'nothing & exp)

                closeBracket as Parser Int =
                    oneToken
                    >> on fn 'token _ _ k:
                    try k as
                        Token.'squareBracket line Token.'closed: ok line
                        _: Parser.reject

                inlineOrBelowOrIndented (maybe << rawList item)
                >> on fn exps:
                inlineOrBelowOrIndented closeBracket
                >> on fn closeRow:
                FA.'list (closeRow > openRow) (Maybe.withDefault [] exps) >> expressionOk

            Token.'curlyBrace openRow Token.'open:
                extension as Parser (Maybe FA.Expression) =
                    discardSecond (maybe (expr env)) (kind Token.'with)

                attribute as Parser { maybeExpr as Maybe FA.Expression, name as FA.Expression } =
                    maybe (kind Token.'newSiblingLine)
                    >> on fn _:
                    expr env
                    >> on fn name:
                    maybe (discardFirst (kind Token.'defop) (inlineOrBelowOrIndented (expr env)))
                    >> on fn maybeExpr:
                    ok { maybeExpr, name }

                closeBrace as Parser Int =
                    oneToken
                    >> on fn 'token _ _ k:
                    try k as
                        Token.'curlyBrace line Token.'closed: ok line
                        _: Parser.reject

                inlineOrBelowOrIndented (maybe extension)
                >> on fn maybeExtension:
                inlineOrBelowOrIndented (maybe (rawList attribute))
                >> on fn attrs:
                inlineOrBelowOrIndented closeBrace
                >> on fn closeRow:
                {
                , attrs = Maybe.withDefault [] attrs
                , isMultiline = closeRow > openRow
                , maybeExtension
                }
                >> FA.'record
                >> expressionOk

            Token.'fn:
                rawList (expr env)
                >> on fn args:
                kind Token.'colon
                >> on fn _:
                alignedOrInlineStatements env
                >> on fn isMultiline & body:
                FA.'fn isMultiline args body >> expressionOk

            Token.'if ifLine:
                elseParser as Parser Int =
                    oneToken
                    >> on fn 'token _ _ k:
                    try k as
                        Token.'else line: ok line
                        _: Parser.reject

                expr env
                >> on fn condition:
                inlineOrBelowOrIndented (kind Token.'then)
                >> on fn _:
                alignedOrInlineStatements env
                >> on fn _ & true:
                inlineOrBelowOrIndented elseParser
                >> on fn elseLine:
                alignedOrInlineStatements env
                >> on fn _ & false:
                FA.'if { condition, false, isMultiline = elseLine > ifLine, true } >> expressionOk

            Token.'try:
                maybeNewLineKind as fn Token.Kind: Parser Token =
                    fn k:
                    maybeNewLine (kind k)

                patternAndValue as Parser (FA.Expression & FA.Expression) =
                    expr env
                    >> on fn p:
                    kind Token.'colon
                    >> on fn _:
                    indentedOrInlineStatements env
                    >> on fn value:
                    ok (p & value)

                inlineOrBelowOrIndented (expr env)
                >> on fn value:
                inlineOrBelowOrIndented (kind Token.'as)
                >> on fn _:
                inlineOrBelowOrIndented (oomSeparatedBy (kind Token.'newSiblingLine) patternAndValue)
                >> on fn patterns:
                {
                , patterns
                , value
                }
                >> FA.'try
                >> expressionOk

            Token.'unop op:
                expressionWithUnambiguousStart env
                >> on fn e:
                FA.'unopCall op e >> expressionOk

            Token.'this_is_sp_native:
                FA.'native >> expressionOk

            Token.'sp_introspect introspect:
                oneToken
                >> on fn 'token start2 end2 kind2:

                try introspect & kind2 as
                    Token.'type & Token.'uppercase { maybeModule, name }:
                        FA.'introspect introspect maybeModule name >> expressionOk

                    Token.'typeOpen & Token.'uppercase { maybeModule, name }:
                        FA.'introspect introspect maybeModule name >> expressionOk

                    Token.'value & Token.'lowercase { attrPath = [], maybeModule, name }:
                        FA.'introspect introspect maybeModule name >> expressionOk

                    _:
                        Parser.abort "innvalid introspect expression"

            _:
                Parser.reject

    b
    >> on fn ee:
    try ee as

        'e_under expr_:
            FA.'expression (List.reverse commentsReversed) (pos env start end) expr_ >> ok

        'e_full expression:
            if commentsReversed == [] then
                ok expression
            else
                [ FA.'evaluation expression ]
                #:: List.map FA.CommentStatement commentsReversed
                #>> List.reverse
                >> FA.'statements
                >> FA.'expression (List.reverse commentsReversed) (pos env start end) __
                >> ok


[#

    ref args

    ref a1
        a2

    ref
        a1 a2

    ref
        a1
        a2

    ref
        a1
          a2

#]
functionApplication as fn Env: Parser FA.Expression =
    fn env:
    # it is "term" in the sense that we are sure we won't need backtracking?
    # TODO need to think more about this and clarify it better.
    # I have it in my head, I understand it intuitively, but I cannot explain it properly.
    term =
        expressionWithUnambiguousStart env

    # First we need at least one "term".
    term
    >> on fn ref:
    # Then let's check for inline arguments: `ref a1 a2 a3...`
    term
    >> Parser.zeroOrMore
    >> on fn inlineArgs:
    # Then we can see if there's indented arguments
    # After the first indentation we allow arguments to be stacked directly below each other
    #
    #   ref a1 a2 a3
    #       a4
    #       a5
    #       a6
    #       ...
    #
    term
    >> inlineOrBelowOrIndented
    >> Parser.zeroOrMore
    >> block
    >> Parser.maybe
    >> on fn indentedArgs:
    args =
        List.concat [ inlineArgs, Maybe.withDefault [] indentedArgs ]

    if args == [] then
        # No application
        ok ref
    else
        p =
            posRange [ ref, args... ]

        # `@Array a` must parse as `@(Array a)`
        # `atan2 -a b` must parse as `atan2 (-a) b`
        # AVOID: `atan2 -a b` parsing as `atan2 -(a b)`!!!
        #
        # TODO: this is ugly because it loses information: `(-a) b c` cannot be distinguished by `-(a b c)`
        # which is not something that will ever happen with the current unops because none of them applies to
        # actual functions, but still...
        #
        # TODO: more importantly, this should handle multiple nested unops
        #
        # TODO: should this live here? Isn't this something we can do in MakeCanonical instead?
        try ref as

            FA.'expression comments p1 (FA.'unopCall op unoped):
                FA.'call unoped args
                >> FA.'expression [] p __
                >> FA.'unopCall op __
                >> FA.'expression comments p1 __
                >> ok

            _:
                FA.'call ref args
                >> FA.'expression [] p __
                >> ok


binop as fn Env: Parser FA.Binop =
    fn env:
    oneToken
    >> on fn 'token start end k:
    try k as

        Token.'binop line { with  precedence, symbol, usr }:
            pullCommentsReversed
            >> on fn commentsReversed:
            {
            , comments = List.reverse commentsReversed
            , line = mkLine env line
            , pos = pos env start end
            , precedence
            , symbol
            , usr
            }
            >> ok

        _:
            Parser.reject


binopChain as fn Env: Parser FA.BinopChain =
    fn env:
    app =
        functionApplication env

    app
    >> on fn left:
    binopAndApp as Parser (FA.Binop & FA.Expression) =
        inlineOrBelowOrIndented (binop env)
        >> on fn b:
        inlineOrBelowOrIndented app
        >> on fn a:
        ok (b & a)

    Parser.zeroOrMore binopAndApp
    >> on fn rights:
    ok (left & rights)


expr as fn Env: Parser FA.Expression =
    fn env:
    binopChain env
    >> on fn x:
    reorderAccordingToBinopPrecedence x >> ok


#
# Reorder binop chain according to binop precedence
#

findLowestPrecedence as fn FA.BinopChain: Int =
    rec as fn Int, [ FA.Binop & expression ]: Int =
        fn lowest, exprs:
        try exprs as
            []: lowest
            [ b & _, tail... ]: rec (min lowest b.precedence) tail

    fn chain:
    rec 1000 chain.second


chain_append as fn FA.Binop & FA.Expression, FA.BinopChain: FA.BinopChain =
    fn binopAndExpr, left & rights:
    left & List.reverse (binopAndExpr :: List.reverse rights)


blah as fn Int, FA.BinopChain, FA.BinopChain, FA.Binop: FA.Expression =
    fn lowestPrecedence, remainingChain, accChain, accOp:
    abovePrecedence & rest =
        List.partitionWhile (fn op & exp: op.precedence > lowestPrecedence) remainingChain.second

    ee =
        reorderAccordingToBinopPrecedence (remainingChain.first & abovePrecedence)

    updatedChain =
        chain_append (accOp & ee) accChain

    try rest as

        []:
            p =
                updatedChain.first :: List.map (fn x: x.second) updatedChain.second >> posRange

            updatedChain
            >> FA.'binopChain lowestPrecedence __
            >> FA.'expression [] p __

        # We are guaranteed that this op is at lowestPrecedence
        [ op & e, rem... ]:
            blah lowestPrecedence (e & rem) updatedChain op


# TODO this needs to be cleaned up?
reorderAccordingToBinopPrecedence as fn FA.BinopChain: FA.Expression =
    fn chain:
    try findLowestPrecedence chain as

        1000:
            chain.first

        lowestPrecedence:
            abovePrecedence & rest =
                List.partitionWhile (fn op & exp: op.precedence > lowestPrecedence) chain.second

            # This is going to be the "left" part of our final chain
            left =
                reorderAccordingToBinopPrecedence (chain.first & abovePrecedence)

            try rest as
                []: left
                [ op & ee, tail... ]: blah lowestPrecedence (ee & tail) (left & []) op


posRange as fn [ FA.Expression ]: Pos =
    fn exprs:
    try exprs as

        []:
            Pos.'g

        [ FA.'expression _ start _, tail... ]:
            try List.last tail as
                'just (FA.'expression _ end _): Pos.range start end
                'nothing: start


#
# Statements
#

stackCommentsReversedAsStatements as fn [ FA.Comment ], [ FA.Statement ]: [ FA.Statement ] =
    fn comments, acc:
    List.forReversed acc comments fn comment, accN:
        FA.'commentStatement comment :: accN


statementParser as fn Env, [ FA.Statement ]: Parser [ FA.Statement ] =
    fn env, acc0:
    Parser.breakCircularDefinition fn _:
    Parser.zeroOrMore (kind Token.'newSiblingLine)
    >> on fn _:
    pullCommentsReversed
    >> on fn commentsReversed:
    [
    , aliasDef env
    , unionDef env
    , definitionOrEvaluation env
    ]
    >> Parser.oneOf
    >> on fn statement:
    [ statement, stackCommentsReversedAsStatements commentsReversed acc0... ] >> ok


definitionOrEvaluation as fn Env: Parser FA.Statement =
    fn env:
    # This one is needed by both definitions and evaluations
    expr env
    >> on fn ex:
    # This is the part that definitions have and evaluations don't have
    definitionTail =
        maybe (inlineOrBelowOrIndented (nonFunction env))
        >> on fn maybeNf:
        inlineOrBelowOrIndented (kind Token.'defop)
        >> on fn _:
        indentedOrInlineStatements env
        >> on fn body:
        maybeNf & body >> ok

    maybe definitionTail
    >> on fn maybeDefTail:
    try maybeDefTail as

        'nothing:
            FA.'evaluation ex

        'just (maybeNf & body):
            {
            , body
            , nonFn = Maybe.withDefault [] maybeNf
            , pattern = ex
            }
            >> FA.'valueDef
    >> ok


nonFunction as fn Env: Parser [ Pos & Name ] =
    fn env:
    kind Token.'with
    >> on fn _:
    rawList (lowerName env)
    >> on fn names:
    upperName env
    >> on fn _ & name:
    if name /= "NonFunction" then
        Parser.abort "Only NonFunction is supported for now"
    else
        ok names


#
# Module
#
rootStatement as fn Env, [ FA.Statement ]: Parser [ FA.Statement ] =
    fn env, acc0:
    forZeroOrMore acc0 (statementParser env __)
    >> on fn acc1:
    # TODO I don't remember why this happens, but sometimes we are missing the BlockStart tokens!?
    Parser.zeroOrMore (kind Token.'blockEnd)
    >> on fn _:
    Parser.here
    >> on fn rs:
    readState =
        eatComments rs

    if readState.tokens == [] then
        acc1
        >> stackCommentsReversedAsStatements readState.commentsReversed __
        >> ok
    else
        Parser.reject


#
# Main
#
makeError as fn Env, [ Token ], Text: Res a =
    fn env, farthestParsed, message:
    try farthestParsed as
        []: Pos.'p 0 1
        [ 'token start end k, rest... ]: Pos.'p start end
    >> Error.res env.errorModule __ [ message ]


parse as fn Env, [ Token ], [ FA.Statement ]: Res [ FA.Statement ] =
    fn env, allTokens, acc:
#    log "PARSE ===================================================" env.errorModule.fsPath
#    List.each allTokens fn t: log "*" t

    initState as ReadState =
        {
        , commentsReversed = []
        , tokens = allTokens
        }

    (failureStates as [ ReadState ]) & (outcome as Parser.Outcome ReadState [ FA.Statement ]) =
        Parser.runParser (rootStatement env acc) initState

    try outcome as

        Parser.'accepted readState output:
            'ok output

        Parser.'aborted readState message:
            makeError env readState.tokens message

        Parser.'rejected:
            findMin =
                fn { with  tokens }, best:
                if List.length tokens < List.length best then tokens else best

            farthestParsed as [ Token ] =
                List.for allTokens failureStates findMin

            try farthestParsed & allTokens as

                [ 'token start end _, rest... ] & _:
                    Error.res env.errorModule (Pos.'p start end) [ "I got stuck parsing here. =(" ]

                [] & [ 'token start end0 _, rest... ]:
                    end =
                        List.for end0 rest (fn 'token _ endX _, _: endX)

                    Error.res env.errorModule (Pos.'p start end) [ "I got to the end of the statement and I can't make sense of it. =(" ]

                [] & []:
                    'ok []


textToFormattableModule as fn Env: Res FA.Module =
    fn env:
    tokensResult as Res [ [ Token ] ] =
        Compiler/Lexer.lexer env.keepComments env.errorModule

    tokensResult
    >> onOk fn tokenChunks:
    #Debug.benchStart None

    errors & reversedStatements =
        List.for ([] & []) tokenChunks fn tokens, es & revStats:
            try parse env tokens revStats as
                'ok newReversedStatements: es & newReversedStatements
                'err e: [ e, es... ] & revStats

    #Debug.benchStop "parseTrail"

    if errors /= [] then
        errors
        >> Error.'nested
        >> 'err
    else
        reversedStatements
        >> List.reverse
        >> 'ok
