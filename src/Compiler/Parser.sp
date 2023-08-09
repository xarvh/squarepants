
alias Env = {
    , errorModule as Error.Module
    , stripLocations as Bool
    }


alias ReadState = {
    # All comments are forced into statements
    # If we wanted the formatter to support also inline comments, I imagine we'd add a `commentInlines as [FA.Comment]`?
    , commentsReversed as [FA.Comment]
    , tokens as [Token]
    }


alias Parser a =
    Parser.Parser ReadState a


eatComments as fn ReadState: ReadState =
    fn readState:

    try readState.tokens as
        , [Token start end (Token.Comment { indent, isBlock, isFollowedByBlank }) , ...tail]:
            {
            , tokens = tail
            , commentsReversed = [
                , { start, end, indent, isBlock, isFollowedByBlank }
                , ...readState.commentsReversed
                ]
            }
            >> eatComments

        , _:
            readState



oneToken as Parser Token =
    fn rejections, rs:

    readState = eatComments rs

    try readState.tokens as
        , []:
            [readState, ...rejections] & Parser.Rejected

        , [token, ...tail]:
            rejections & Parser.Accepted { readState with tokens = tail } token



pullCommentsReversed as Parser [FA.Comment] =
    fn rejections, rs:

    readState =
        eatComments rs

    rejections & Parser.Accepted { readState with commentsReversed = [] } readState.commentsReversed


#
# Helpers
#
on as fn (fn a: Parser b): fn Parser a: Parser b =
    Parser.andThen


ok as fn a: Parser a =
    Parser.accept


maybe as fn Parser a: Parser (Maybe a) =
    Parser.maybe


here as Parser Int =
    Parser.here >> on fn readState:

    try readState.tokens as
        , [Token start end _, ...rest]: start
        , []: 0
    >> ok


# TODO rename to mkpos?
pos as fn Env, Int, Int: Pos =
    fn env, start, end:

    if env.stripLocations then
        Pos.T
    else
        Pos.P start end


mkLine as fn Env, Int: Int =
    fn env, line:

    if env.stripLocations then
        -1
    else
        line


#
# Utility
#
forZeroOrMore as fn o, (fn o: Parser o): Parser o =
    fn init, getParser:

    getParser init
    >> Parser.thenWithDefault (ok init) (forZeroOrMore __ getParser)


# TODO rename to "exact"?
kind as fn Token.Kind: Parser Token =
    fn targetKind:
    oneToken >> on fn token:
    (Token _ _ k) = token
    if targetKind == k then
        ok token

    else
        Parser.reject


discardFirst as fn Parser a, Parser b: Parser b =
    fn a, b:
    a >> on fn _: b


discardSecond as fn Parser a, Parser b: Parser a =
    fn a, b:
    a >> on fn aa:
    b >> on fn _:
    ok aa


maybeWithDefault as fn a, Parser a: Parser a =
    fn a, p:
    Parser.oneOf [ p, ok a ]


surroundStrict as fn Token.Kind, Token.Kind, Parser a: Parser a =
    fn left, right, p:
    Parser.surroundWith (kind left) (kind right) p


surroundMultiline as fn Token.Kind, Token.Kind, Parser a: Parser a =
    fn left, right, content:
    discardFirst
        (kind left)
        (inlineOrBelowOrIndented
            (discardSecond
                content
                (inlineOrBelowOrIndented (kind right))
            )
        )


oomSeparatedBy as fn Parser a, Parser b: Parser [b] =
    fn sep, pa:
    pa >> on fn head:
    Parser.zeroOrMore (discardFirst sep pa) >> on fn tail:
    ok << head :: tail


sepListAtSep as fn Parser sep, Parser item: Parser [sep & item] =
    fn sep, item:
    sep >> on fn sep0:
    theParserStillSucks =
        Parser.oneOf
            [ block (sepListAtItem sep item)
            , sib (sepListAtItem sep item)
            , sepListAtItem sep item
            ]
    theParserStillSucks >> on fn ( item0 & tail ):
    ok << sep0 & item0 :: tail



sepListAtItem as fn Parser sep, Parser item: Parser (item & [sep & item]) =
    fn sep, item:
    item >> on fn item0:
    theParserStillSucks =
        Parser.oneOf
            [ block (sepListAtSep sep item)
            , sib (sepListAtSep sep item)
            , sepListAtSep sep item
            , ok []
            ]
    theParserStillSucks >> on fn sepsAndItems:
    ok ( item0 & sepsAndItems )


sepList as fn Parser sep, Parser item: Parser (item & [sep & item]) =
    sepListAtItem


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
rawList as fn Parser a: Parser [a] =
    fn item:

    sibsep =
        # TODO was:
        #             (Parser.maybe << kind Token.NewSiblingLine) >> on _:
        #             kind Token.Comma
        # but I didn't test it properly
        inlineOrBelowOrIndented << kind Token.Comma

    discardFirst (Parser.maybe sibsep) (oomSeparatedBy sibsep item)


word as fn Env: Parser FA.Word =
    fn env:

    oneToken >> on fn (Token start end k):
    try k as
        , Token.Word w: ok (FA.Word (pos env start end) w)
        , _: Parser.reject


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
    surroundStrict Token.BlockStart Token.BlockEnd __


sib as fn Parser a: Parser a =
    discardFirst (kind Token.NewSiblingLine) __


maybeNewLine as fn Parser a: Parser a =
    discardFirst (Parser.maybe (kind Token.NewSiblingLine)) __


inlineOrBelowOrIndented as fn Parser a: Parser a =
    fn p:
    Parser.oneOf
        [
        , block p
        , sib p
        , p
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

  forZeroOrMore acc (fn a: discardFirst (kind Token.NewSiblingLine) (statementParser env a))
  >> on fn reversedStatements:

  here
  >> on fn end:

  try reversedStatements as
      , [ FA.Evaluation e ]:
          ok e

      , many:
          many
          >> List.reverse
          >> FA.Statements
          >> FA.Expression [] (pos env start end) __
          >> ok


indentedOrInlineStatements as fn Env: Parser FA.Expression =
    fn env:

    [
    , block (siblingStatements env)
    , expr env
    ]
    >> Parser.oneOf


alignedOrInlineStatements as fn Env: Parser (Bool & FA.Expression) =
    fn env:

    Parser.oneOf
        [
        , block (siblingStatements env) >> on fn e: ok (True & e)
        , sib (siblingStatements env) >> on fn e: ok (True & e)
        , expr env >> on fn e: ok (False & e)
        ]


#
# Statements
#
aliasDef as fn Env: Parser FA.Statement =
    fn env:

    aliasWord as Token.Word = {
        , modifier = Token.NameNoModifier
        , isUpper = False
        , maybeModule = Nothing
        , name = "alias"
        , attrPath = []
        }

    kind (Token.Word aliasWord) >> on fn _:
    word env >> on fn name:
    Parser.zeroOrMore (word env) >> on fn args:
    kind Token.Defop >> on fn _:
    inlineOrBelowOrIndented (expr env) >> on fn type:
    {
    , name
    , args
    , type
    }
    # TODO use ty end instead
    >> FA.AliasDef
    >> ok


unionDef as fn Env: Parser FA.Statement =
    fn env:

    unionWord as Token.Word =
        {
        , modifier = Token.NameNoModifier
        , isUpper = False
        , maybeModule = Nothing
        , name = "union"
        , attrPath = []
        }

    kind (Token.Word unionWord) >> on fn _:
    word env >> on fn name:
    Parser.zeroOrMore (word env) >> on fn args:
    kind Token.Defop >> on fn _:
    inlineOrBelowOrIndented (rawList (expr env)) >> on fn constructors:
    {
    , name
    , args
    , constructors
    }
    >> FA.UnionDef
    >> ok


#
# Expression
#

union Ee = E_full FA.Expression, E_under FA.Expr_




expressionWithUnambiguousStart as fn Env: Parser FA.Expression =
    fn env:

    pullCommentsReversed
    >> on fn commentsReversed:

    oneToken
    >> on fn Token start end kk:

    expressionOk as fn FA.Expr_: Parser Ee =
      fn e: e >> E_under >> ok

    b =
      # We try all tokens that unambiguously mark the start of an expression
      try kk as
        , Token.Word tokenWord:

            variableOk =
                fn maybeType: {
                    , maybeType
                    , tokenWord
                    }
                    >> FA.Variable
                    >> expressionOk

            Parser.oneOf [
              , discardFirst (kind Token.As) (expr env)
                >> on fn t:
                variableOk (Just t)
              , variableOk Nothing
              ]

        , Token.ArgumentPlaceholder:
            FA.ArgumentPlaceholder
            >> expressionOk

        , Token.NumberLiteral isPercent s:
            Parser.oneOf [

              , discardFirst (kind Token.UniquenessPolymorphismBinop) (expr env)
                >> on fn e:
                FA.Poly s e
                >> expressionOk

              , FA.LiteralNumber isPercent s
                >> expressionOk
              ]

        , Token.TextLiteral s:
            FA.LiteralText s
            >> expressionOk

        , Token.RoundParen Token.Open:
            discardSecond
                (inlineOrBelowOrIndented (expr env))
                (inlineOrBelowOrIndented (kind (Token.RoundParen Token.Closed)))
            >> on fn e:
            ok (E_full e)


        , Token.SquareBracket openRow Token.Open:
            item as Parser (Bool & FA.Expression) =
                maybe (kind Token.ThreeDots) >> on fn maybeDots:
                expr env >> on fn exp:
                ok (maybeDots /= Nothing & exp)

            closeBracket as Parser Int =
                oneToken >> on fn Token _ _ k:
                try k as
                    , Token.SquareBracket line Token.Closed: ok line
                    , _: Parser.reject

            inlineOrBelowOrIndented (maybe << rawList item) >> on fn exps:
            inlineOrBelowOrIndented (closeBracket) >> on fn closeRow:
            FA.List (closeRow > openRow) (Maybe.withDefault [] exps)
            >> expressionOk

        , Token.CurlyBrace openRow Token.Open:
            extension as Parser (Maybe FA.Expression) =
                discardSecond
                    (maybe (expr env))
                    (kind Token.With)

            attribute as Parser { name as FA.Expression, maybeExpr as Maybe FA.Expression } =
                maybe (kind Token.NewSiblingLine) >> on fn _:
                expr env >> on fn name:
                maybe (discardFirst (kind Token.Defop) (inlineOrBelowOrIndented (expr env))) >> on fn maybeExpr:
                ok { name, maybeExpr }

            closeBrace as Parser Int =
                oneToken >> on fn Token _ _ k:
                try k as
                    , Token.CurlyBrace line Token.Closed: ok line
                    , _: Parser.reject

            inlineOrBelowOrIndented (maybe extension) >> on fn maybeExtension:
            inlineOrBelowOrIndented (maybe (rawList attribute)) >> on fn attrs:
            inlineOrBelowOrIndented (closeBrace) >> on fn closeRow:
            {
            , maybeExtension
            , attrs = Maybe.withDefault [] attrs
            , isMultiline = closeRow > openRow
            }
            >> FA.Record
            >> expressionOk

        , Token.Fn:
            rawList (expr env) >> on fn args:
            kind Token.Colon >> on fn _:
            alignedOrInlineStatements env >> on fn isMultiline & body:
            FA.Fn isMultiline args body
            >> expressionOk

        , Token.If ifLine:

            elseParser as Parser Int =
                oneToken >> on fn Token _ _ k:
                try k as
                  , Token.Else line: ok line
                  , _: Parser.reject

            expr env >> on fn condition:
            inlineOrBelowOrIndented (kind Token.Then) >> on fn _:
            alignedOrInlineStatements env >> on fn _ & true:
            inlineOrBelowOrIndented elseParser >> on fn elseLine:
            alignedOrInlineStatements env >> on fn _ & false:
            FA.If { isMultiline = elseLine > ifLine, condition, true, false }
            >> expressionOk

        , Token.Try:
            maybeNewLineKind as fn Token.Kind: Parser Token =
                fn k:
                maybeNewLine (kind k)

            patternAndValue as Parser (FA.Expression & FA.Expression) =
                expr env >> on fn p:
                kind Token.Colon >> on fn _:
                indentedOrInlineStatements env >> on fn value:
                ok ( p & value )

            inlineOrBelowOrIndented (expr env) >> on fn value:
            inlineOrBelowOrIndented (kind Token.As) >> on fn _:
            inlineOrBelowOrIndented (rawList patternAndValue) >> on fn patterns:
            {
            , value
            , patterns
            }
            >> FA.Try
            >> expressionOk

        , Token.Unop op:
            expressionWithUnambiguousStart env
            >> on fn e:

            FA.UnopCall op e
            >> expressionOk

        , _:
            Parser.reject


    b >> on fn ee:

    try ee as
        , E_under expr_:

          FA.Expression (List.reverse commentsReversed) (pos env start end) expr_
          >> ok

        , E_full expression:
          if commentsReversed == [] then
            ok expression
          else
                [FA.Evaluation expression] #:: List.map FA.CommentStatement commentsReversed
                #>> List.reverse
                >> FA.Statements
                >> FA.Expression (List.reverse commentsReversed) (pos env start end) __
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

    e =
        expressionWithUnambiguousStart env

    # First we need one "term".
    e
    >> on fn ref:

    # Then we need another, indented or inline.
    # Directly below is not allowed.
    [
    , block e
    , e
    ]
    >> Parser.oneOf
    >> maybe
    >> on fn maybeFirstArgument:

    try maybeFirstArgument as
        , Nothing:
            # No application
            ok ref

        , Just argsHead:
            # There might be more arguments.
            # These are allowed to be directly below

            e
            >> inlineOrBelowOrIndented
            >> Parser.zeroOrMore
            >> on fn argsTail:

            p =
                posRange [ref, argsHead, ...argsTail]

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
                , FA.Expression comments p1 (FA.UnopCall op unoped):
                    FA.Call unoped (argsHead :: argsTail)
                    >> FA.Expression [] p __
                    >> FA.UnopCall op __
                    >> FA.Expression comments p1 __
                    >> ok

                , _:
                    FA.Call ref (argsHead :: argsTail)
                    >> FA.Expression [] p __
                    >> ok



binop as fn Env: Parser FA.Binop =
    fn env:

    oneToken
    >> on fn (Token start end k):

    try k as
        , Token.Binop line { with usr, symbol, precedence }:

            pullCommentsReversed
            >> on fn commentsReversed:

            {
            , comments = List.reverse commentsReversed
            , usr
            , symbol
            , precedence
            , pos = pos env start end
            , line = mkLine env line
            }
            >> ok

        , _:
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

    reorderAccordingToBinopPrecedence x
    >> ok



#recInlineOrIndentedOrBelow as fn Parser FA.Expression, [FA.Expression]: Parser [FA.Expression] =
#    fn higher, accum:
#    higher >> on fn h:
#
#    r =
#        h :: accum
#
#    maybeWithDefault r __ << inlineOrBelowOrIndented (recInlineOrIndentedOrBelow higher r)




#
# Reorder binop chain according to binop precedence
#


findLowestPrecedence as fn FA.BinopChain: Int =

    rec as fn Int, [FA.Binop & expression]: Int =
        fn lowest, exprs:
        try exprs as
            , []: lowest
            , [ b & _ , ...tail ]:
                rec (min lowest b.precedence) tail

    fn chain:
    rec 1000 chain.second


chain_append as fn FA.Binop & FA.Expression, FA.BinopChain: FA.BinopChain =
    fn binopAndExpr, left & rights:

    left & List.reverse (binopAndExpr :: (List.reverse rights))


blah as fn Int, FA.BinopChain, FA.BinopChain, FA.Binop: FA.Expression =
    fn lowestPrecedence, remainingChain, accChain, accOp:

    abovePrecedence & rest =
        List.partitionWhile (fn op & exp: op.precedence > lowestPrecedence) remainingChain.second

    ee =
        reorderAccordingToBinopPrecedence (remainingChain.first & abovePrecedence)

    updatedChain =
        chain_append (accOp & ee) accChain

    try rest as
        , []:
            p =
                updatedChain.first :: List.map (fn x: x.second) updatedChain.second
                >> posRange

            updatedChain
            >> FA.BinopChain lowestPrecedence __
            >> FA.Expression [] p __

        # We are guaranteed that this op is at lowestPrecedence
        , [op & e, ...rem]:
              blah lowestPrecedence (e & rem) updatedChain op


# TODO this needs to be cleaned up?
reorderAccordingToBinopPrecedence as fn FA.BinopChain: FA.Expression =
    fn chain:

    try findLowestPrecedence chain as
        , 1000:
            chain.first

        , lowestPrecedence:

              abovePrecedence & rest =
                  List.partitionWhile (fn op & exp: op.precedence > lowestPrecedence) chain.second

              # This is going to be the "left" part of our final chain
              left =
                  reorderAccordingToBinopPrecedence (chain.first & abovePrecedence)

              try rest as
                  , []:
                      left

                  , [op & ee, ...tail]:
                      blah lowestPrecedence (ee & tail) (left & []) op


posRange as fn [FA.Expression]: Pos =
    fn exprs:

    try exprs as
        , []: Pos.G
        , [ FA.Expression _ start _, ...tail]:
            try List.last tail as
                , Just (FA.Expression _ end _): Pos.range start end
                , Nothing: start



#
# Statements
#


stackCommentsReversedAsStatements as fn [FA.Comment], [FA.Statement]: [FA.Statement] =
    fn comments, acc:
    List.forReversed acc comments fn comment, accN:
        FA.CommentStatement comment :: accN




statementParser as fn Env, [FA.Statement]: Parser [FA.Statement] =
    fn env, acc0:

    Parser.breakCircularDefinition fn _:

    Parser.zeroOrMore (kind Token.NewSiblingLine)
    >> on fn _:

    pullCommentsReversed
    >> on fn commentsReversed:

    [
    , aliasDef env
    , unionDef env
    , definition env
    , expr env >> on fn e: e >> FA.Evaluation >> ok
    ]
    >> Parser.oneOf
    >> on fn statement:

    [ statement, ...stackCommentsReversedAsStatements commentsReversed acc0]
    >> ok


definition as fn Env: Parser FA.Statement =
    fn env:

    expr env
    >> on fn p:

    Parser.maybe (inlineOrBelowOrIndented (nonFunction env))
    >> on fn nf:

    inlineOrBelowOrIndented (kind Token.Defop)
    >> on fn _:

    indentedOrInlineStatements env
    >> on fn body:

    {
    , pattern = p
    , body
    , nonFn = Maybe.withDefault [] nf
    }
    >> FA.ValueDef
    >> ok


#
# Types
#


nonFunction as fn Env: Parser [FA.Word] =
    fn env:
    kind Token.With >> on fn _:
    rawList (word env) >> on fn names:
    word env >> on fn (FA.Word _ literal):
    if literal.name /= "NonFunction" then
        Parser.abort "Only NonFunction is supported for now"
    else
        ok names


typeAnnotation as fn Env: Parser FA.Expression =
    fn env:

    discardFirst
        (kind Token.As)
        (inlineOrBelowOrIndented (expr env))






#
# Module
#
rootStatement as fn Env, [FA.Statement]: Parser [FA.Statement] =
    fn env, acc0:

    forZeroOrMore acc0 (statementParser env __)
    >> on fn acc1:

    # TODO I don't remember why this happens, but sometimes we are missing the BlockStart tokens!?
    Parser.zeroOrMore (kind Token.BlockEnd)
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
makeError as fn Env, [Token], Text: Res a =
    fn env, farthestParsed, message:

    try farthestParsed as
        , []: Pos.P 0 1
        , [Token start end k, ...rest]: Pos.P start end
    >> Error.res env.errorModule __ [ message ]


parse as fn Env, [Token], [FA.Statement]: Res [FA.Statement] =
    fn env, allTokens, acc:

    log "PARSE ===================================================" env.errorModule.fsPath
#    List.each allTokens fn t: log "*" t

    initState as ReadState = {
        , commentsReversed = []
        , tokens = allTokens
        }

    (failureStates as [ReadState]) & (outcome as Parser.Outcome ReadState [FA.Statement]) =
        Parser.runParser (rootStatement env acc) initState

    try outcome as
        , Parser.Accepted readState output:
            Ok output

        , Parser.Aborted readState message:
            makeError env readState.tokens message

        , Parser.Rejected:
            findMin =
                fn { with tokens }, best:
                if List.length tokens < List.length best then tokens else best

            farthestParsed as [Token] =
                List.for allTokens failureStates findMin

            try farthestParsed & allTokens as
                , [Token start end _, ...rest] & _:
                    Error.res env.errorModule (Pos.P start end) [ "I got stuck parsing here. =(" ]

                , [] & [Token start end0 _, ...rest]:
                    end = List.for end0 rest fn Token _ endX _, _: endX
                    Error.res env.errorModule (Pos.P start end) [ "I got to the end of the statement and I can't make sense of it. =(" ]

                , [] & []:
                    Ok []


textToFormattableModule as fn Env: Res FA.Module =
    fn env:

    tokensResult as Res [[Token]] =
        Compiler/Lexer.lexer env.errorModule

    tokensResult
    >> onOk fn tokenChunks:

    Debug.benchStart None

    errors & reversedStatements =
        List.for ([] & []) tokenChunks fn tokens, es & revStats:
            try parse env tokens revStats as
                , Ok newReversedStatements: es & newReversedStatements
                , Err e: [e, ...es] & revStats

    Debug.benchStop "parseTrail"

    if errors /= [] then
        errors
        >> Error.Nested
        >> Err

    else
      reversedStatements
      >> List.reverse
      >> Ok

