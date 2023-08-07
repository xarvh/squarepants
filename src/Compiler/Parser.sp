
alias Env =
    {
    , errorModule as Error.Module
    , stripLocations as Bool
    }


#
#
#

alias Parser a =
    Parser.Parser Token a


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
    Parser.here >> on fn tokens:
    ok
        (try tokens as
            , Token c mod start end :: rest:
                start

            , []:
                0
        )


pos as fn Env, Int, Int: Pos =
    fn env, start, end:

    if env.stripLocations then
        Pos.T
    else
        Pos.P start end


#
# Utility
#
oneToken as Parser Token =
    Parser.consumeOne


# TODO rename to "exact"?
kind as fn Token.Kind: Parser Token =
    fn targetKind:
    oneToken >> on fn token:
    (Token _ _ _ k) = token
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



sepListAtItem as fn Parser sep, Parser item: Parser (FA.SepList sep item) =
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


sepList as fn Parser sep, Parser item: Parser (FA.SepList sep item) =
    sepListAtItem


[#| TODO make it more flexible

also, note whether it is multiline or not, so that formatting can preserve it

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


word as fn Env: Parser (At Token.Word) =
    fn env:

    oneToken >> on fn (Token comment start end k):
    try k as
        , Token.Word w: ok (At (pos env start end) w)
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

  oomSeparatedBy (kind Token.NewSiblingLine) (statement env)
  >> on fn stats:

  here
  >> on fn end:

  # Is this optimization really necessary?
  try stats as
      , [ FA.Evaluation expr ]:
          ok expr

      , many:
          FA.Expression (pos env start end) (FA.Statements stats) >> ok


indentedOrInlineStatements as fn Env: Parser FA.Expression =
    fn env:

    Parser.oneOf
        [
        , block (siblingStatements env)
        , expr env
        ]


alignedOrInlineStatements as fn Env: Parser FA.Expression =
    fn env:

    Parser.oneOf
        [
        , block (siblingStatements env)
        , sib (siblingStatements env)
        , expr env
        ]


#
# Statements
#
aliasDef as fn Env: Parser FA.Statement =
    fn env:

    aliasWord as Token.Word =
        {
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
exprWithLeftDelimiter as fn Env, Token.Kind: Parser FA.Expr_ =
    fn env, tokenKind:

    try tokenKind as
        , Token.Word w:
            Parser.oneOf
              [
              , discardFirst (kind Token.As) (expr env) >> on fn t:
                 FA.Variable { maybeType = Just t, word = w } >> ok
              , FA.Variable { maybeType = Nothing, word = w } >> ok
              ]

        , Token.ArgumentPlaceholder:
            FA.ArgumentPlaceholder >> ok

        , Token.NumberLiteral isPercent s:
            Parser.oneOf
              [
              , discardFirst (kind Token.UniquenessPolymorphismBinop) (expr env) >> on fn e:
                 FA.Poly s e >> ok
              , FA.LiteralNumber isPercent s >> ok
              ]

        , Token.TextLiteral s:
            FA.LiteralText s >> ok

        , Token.RoundParen Token.Open:
            inlineOrBelowOrIndented (expr env) >> on fn (FA.Expression p expr_):
            inlineOrBelowOrIndented (kind (Token.RoundParen Token.Closed)) >> on fn _:
            ok expr_

        , Token.SquareBracket Token.Open:
            item as Parser (Bool & FA.Expression) =
                maybe (kind Token.ThreeDots) >> on fn maybeDots:
                expr env >> on fn exp:
                ok (maybeDots /= Nothing & exp)

            inlineOrBelowOrIndented (maybe << rawList item) >> on fn exps:
            inlineOrBelowOrIndented (kind (Token.SquareBracket Token.Closed)) >> on fn _:
            FA.List (Maybe.withDefault [] exps) >> ok

        , Token.CurlyBrace Token.Open:
            extension as Parser (Maybe FA.Expression) =
                discardSecond
                    (maybe (expr env))
                    (kind Token.With)

            attribute as Parser { name as FA.Expression, maybeExpr as Maybe FA.Expression } =
                maybe (kind Token.NewSiblingLine) >> on fn _:
                expr env >> on fn name:
                maybe (discardFirst (kind Token.Defop) (inlineOrBelowOrIndented (expr env))) >> on fn maybeExpr:
                ok { name, maybeExpr }

            inlineOrBelowOrIndented (maybe extension) >> on fn maybeExtension:
            inlineOrBelowOrIndented (maybe (rawList attribute)) >> on fn attrs:
            inlineOrBelowOrIndented (kind (Token.CurlyBrace Token.Closed)) >> on fn _:
            FA.Record { maybeExtension, attrs = Maybe.withDefault [] attrs } >> ok

        , Token.Fn:
            rawList (expr env) >> on fn args:
            kind Token.Colon >> on fn _:
            alignedOrInlineStatements env >> on fn body:
            FA.Fn args body >> ok

        , Token.If:
            expr env >> on fn condition:
            inlineOrBelowOrIndented (kind Token.Then) >> on fn _:
            alignedOrInlineStatements env >> on fn true:
            inlineOrBelowOrIndented (kind Token.Else) >> on fn _:
            alignedOrInlineStatements env >> on fn false:
            FA.If { condition, true, false } >> ok

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
            here >> on fn end:
            {
            , value = value
            , patterns = patterns
            }
            >> FA.Try
            >> ok

        , Token.Unop op:
            FA.Unop op
            >> ok

        , Token.Binop op:
            FA.Binop op
            >> ok

        [# TODO this is a horrid workaround necessary to support the edge case of an op being used inside an attr def:

            {
            , attr =
                a
                >> b
            }

          Would be nice to reorganize the parser so that it is not needed.
        #]
        , Token.NewSiblingLine:
            oneToken >> on fn (Token c  s e k):
            try k as
                , Token.Binop op:
                  maybe (kind Token.NewSiblingLine) >> on fn _:
                  FA.Binop op
                  >> ok

                , _:
                  Parser.reject

        , _:
            Parser.reject



expr as fn Env: Parser FA.Expression =
    fn env:

    higher as Parser FA.Expression =
        oneToken >> on fn (Token comment start end k):
        exprWithLeftDelimiter env k >> on fn expr_:
        FA.Expression (pos env start end) expr_ >> ok


    recInlineOrIndented as fn [FA.Expression]: Parser [FA.Expression] =
        fn accum:

        higher >> on fn h:

        r =
            h :: accum

        Parser.oneOf
            # after at least one indented block, allow arguments to appear also as siblings (ie, right below)
            [ block (recInlineOrIndentedOrBelow higher r)
            , recInlineOrIndented r
            , ok r
            ]

    here >> on fn start:
    recInlineOrIndented [] >> on fn reversedArgs:
    here >> on fn end:

    # TODO Why is functionApplicationOr being called twice?
    #log "===========================================" { start, end }

    reversedArgs
    >> List.reverse
    >> breakByPrecedence
    >> ok


recInlineOrIndentedOrBelow as fn Parser FA.Expression, [FA.Expression]: Parser [FA.Expression] =
    fn higher, accum:
    higher >> on fn h:

    r =
        h :: accum

    maybeWithDefault r __ << inlineOrBelowOrIndented (recInlineOrIndentedOrBelow higher r)





findLowestPrecedence as fn [FA.Expression]: Int =

    rec as fn Int, [FA.Expression]: Int =
        fn lowest, exprs:
        try exprs as
            , []: lowest
            , [ FA.Expression _ head, ...tail ]:
                try head as
                    , FA.Binop op: rec (min lowest op.precedence) tail
                    , _: rec lowest tail

    rec 1000 __


# TODO Properly manage errors
breakByPrecedence as fn [FA.Expression]: FA.Expression =
    fn rawExpressions:

    try findLowestPrecedence rawExpressions as
        , 1000:
            headUnops & exprs1 =
                eatAllUnops rawExpressions

            applyHeadUnops =
                List.for __ headUnops fn p & op, exp: FA.Expression p (FA.UnopCall op exp)

            try breakByUnops exprs1 as
                , []: todo "bug: breakByPrecedence empty?"
                , [ one ]: applyHeadUnops one
                , [ head, ...tail ]: applyHeadUnops (FA.Expression (posRange rawExpressions) (FA.Call head tail))

        , lowestPrecedence:
            rawExpressions
            >> splitOnOpWithPrecedence lowestPrecedence __
            >> FA.BinopChain lowestPrecedence __
            >> FA.Expression (posRange rawExpressions) __


posRange as fn [FA.Expression]: Pos =
    fn exprs:

    try exprs as
        , []: Pos.G
        , [ FA.Expression start _, ...tail]:
            try List.last tail as
                , Just (FA.Expression end _): Pos.range start end
                , Nothing: start



# TODO use an ad-hoc variant type instead of Result
exprOrTargetOp as fn Int, FA.Expression: Result Op.Binop FA.Expression =
    fn precedence, head:

    try head as
        , FA.Expression _ (FA.Binop op):
            if op.precedence == precedence then
                Err op
            else
                Ok head

        , _:
            Ok head


readExprs as fn Int, [FA.Expression]: [FA.Expression] & [FA.Expression] =
    fn targetOpPrecedence, rawExpressions:

    rec =
        fn acc, remainder:
        try remainder as
            , [ head, ...tail ]:
                try exprOrTargetOp targetOpPrecedence head as
                    , Ok e:
                        rec [e, ...acc] tail

                    , Err op:
                        List.reverse acc & remainder

            , []:
                List.reverse acc & remainder

    rec [] rawExpressions


readOpsAndExprs as fn Int, [Op.Binop & FA.Expression], [FA.Expression]: [Op.Binop & FA.Expression] =
    fn targetOpPrecedence, acc, remainder:

    try remainder as
        , []:
            List.reverse acc

        , [ head, ...tail ]:
            try exprOrTargetOp targetOpPrecedence head as

                , Err op:
                    exprs & newRemainder =
                        readExprs targetOpPrecedence tail

                    readOpsAndExprs targetOpPrecedence [ op & breakByPrecedence exprs, ...acc ] newRemainder

                , Ok exprs:
                    todo "bug: readOpsAndExprs spurious exp"


splitOnOpWithPrecedence as fn Int, [FA.Expression]: FA.SepList Op.Binop FA.Expression =
    fn targetOpPrecedence, rawExpressions:

    first & remainder =
        readExprs targetOpPrecedence rawExpressions

    opsAndExpressions as [ Op.Binop & FA.Expression ]=
        readOpsAndExprs targetOpPrecedence [] remainder

    breakByPrecedence first & opsAndExpressions



getOneUnopExpr as fn [FA.Expression]: FA.Expression & [FA.Expression] =

    rec as fn (fn FA.Expression: FA.Expression), [FA.Expression]: FA.Expression & [FA.Expression] =
        fn accF, remainder:

        try remainder as
            , FA.Expression p (FA.Unop op) :: tail:
                rec (fn e: FA.Expression p (FA.UnopCall op e) >> accF) tail

            , head :: tail:
                accF head & tail

            , []:
                todo "bug: FA.Error & []"

    rec identity __





eatAllUnops as fn [FA.Expression]: [Pos & Op.UnopId] & [FA.Expression] =
    rec as fn [Pos & Op.UnopId], [FA.Expression]: [Pos & Op.UnopId] & [FA.Expression] =
        fn unops, exprs:
        try exprs as
            , FA.Expression p (FA.Unop op) :: tail:
                rec [p & op, ...unops] tail

            , _:
                unops & exprs

    rec [] __


breakByUnops as fn [FA.Expression]: [FA.Expression] =
    rec as fn [FA.Expression], [FA.Expression]: [FA.Expression] =
        fn acc, exprs:

        try exprs as
            , []: List.reverse acc

            , _:
              e & remainder = getOneUnopExpr exprs
              rec (e :: acc) remainder

    rec [] __










#
# Statements
#


statement as fn Env: Parser FA.Statement =
    fn env:
    Parser.breakCircularDefinition fn _:
    # This is here because inline comments might be followed by NewSiblingLine
    # and I am not sure it's a responsibility of the lexer to deal with it.
    Parser.maybe (kind Token.NewSiblingLine) >> on fn _:
    Parser.oneOf
        [ aliasDef env
        , unionDef env
        , definition env
        , expr env >> on fn e:
          FA.Evaluation e >> ok
        ]


definition as fn Env: Parser FA.Statement =
    fn env:
    here >> on fn start:
    expr env >> on fn p:
    Parser.maybe (inlineOrBelowOrIndented (nonFunction env)) >> on fn nf:
    inlineOrBelowOrIndented (kind Token.Defop) >> on fn defModifier:
    indentedOrInlineStatements env >> on fn body:

#    end =
#        body
#            >> List.reverse
#            >> List.head
#            >> Maybe.map getpos
#            >> Maybe.withDefault Pos.T
#            >> Pos.end

    here >> on fn end:
    { pattern = p
    , body = body
    , nonFn = Maybe.withDefault [] nf
    }
    >> FA.ValueDef
    >> ok


#
# Types
#


nonFunction as fn Env: Parser [At Token.Word] =
    fn env:
    kind Token.With >> on fn _:
    rawList (word env) >> on fn names:
    word env >> on fn (At _ literal):
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
rootStatement as fn Env: Parser (Maybe FA.Statement) =
    fn env:

    fillers =
        Parser.oneOf
            [ kind Token.BlockEnd
            , kind Token.NewSiblingLine
            ]

    Parser.zeroOrMore (kind Token.NewSiblingLine) >> on fn _:
    Parser.maybe (statement env) >> on fn maybeStatement:
    Parser.zeroOrMore fillers >> on fn _:
    Parser.end >> on fn _:

    Parser.accept maybeStatement


#
# Main
#
makeError as fn Env, [Token], Text: Res a =
    fn env, readState, message:

    p =
        try readState as
            , []: Pos.P 0 1
            , Token comment start end k :: rest: Pos.P start end

    Error.res env.errorModule p [ message ]


parse as fn Env, [Token]: Res (Maybe FA.Statement) =
    fn env, tokens:

    (failureStates as [[Token]]) & (outcome as Parser.Outcome Token (Maybe FA.Statement)) =
        Parser.runParser (rootStatement env) tokens

    try outcome as
        , Parser.Accepted readState output:
            Ok output

        , Parser.Aborted readState message:
            makeError env readState message

        , Parser.Rejected:
            findMin = fn readState, best:
                if List.length readState < List.length best then readState else best

            readState as [Token] =
                List.for tokens failureStates findMin

            message =
                try readState as
                    , []: "I got to the end of the statement and I can't make sense of it. =("
                    , _: "I got stuck parsing here. =("

            makeError env readState message


textToFormattableModule as fn Env: Res FA.Module =
    fn env:

    tokensResult as Res [[Token]] =
        Compiler/Lexer.lexer env.errorModule

    tokensResult
    >> onOk fn rootStatements:

    Debug.benchStart None
    result =
        rootStatements
        >> List.mapRes (parse env __) __
        >> Result.map (List.filterMap identity __) __

    Debug.benchStop "parseTrail"

    result

