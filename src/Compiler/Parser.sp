
alias Env =
    { moduleName as Text
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
on =
    Parser.andThen


ok as a: Parser a =
    Parser.accept


maybe as Parser a: Parser (Maybe a) =
    Parser.maybe


here as Parser Int =
    Parser.here >> on tokens:
    ok
        (try tokens as
            Token mod start end :: rest:
                start

            []:
                0
        )


pos as Env: Int: Int: Pos =
    env: start: end:

    if env.stripLocations then
        Pos.T
    else
        Pos.P env.moduleName start end


#
# Utility
#
oneToken as Parser Token =
    Parser.consumeOne


kind as Token.Kind: Parser Token =
    targetKind:
    oneToken >> on token:
    (Token _ _ k) = token
    if targetKind == k then
        ok token

    else
        Parser.reject


discardFirst as Parser a: Parser b: Parser b =
    a: b:
    a >> on _: b


discardSecond as Parser a: Parser b: Parser a =
    a: b:
    a >> on aa:
    b >> on _:
    ok aa


inlineOrIndented as Parser a: Parser a =
    p:
    Parser.oneOf
        [ block p
        , p
        ]


inlineOrBelowOrIndented as Parser a: Parser a =
    p:
    Parser.oneOf
        [ block p
        , sib p
        , p
        ]


maybeWithDefault as a: Parser a: Parser a =
    a: p:
    Parser.oneOf [ p, ok a ]


surroundStrict as Token.Kind: Token.Kind: Parser a: Parser a =
    left: right:
    Parser.surroundWith (kind left) (kind right)


surroundMultiline as Token.Kind: Token.Kind: Parser a: Parser a =
    left: right: content:
    discardFirst
        (kind left)
        (inlineOrBelowOrIndented
            (discardSecond
                content
                (inlineOrBelowOrIndented (kind right))
            )
        )


oomSeparatedBy as Parser a: Parser b: Parser [b] =
    sep: pa:
    pa >> on head:
    Parser.zeroOrMore (discardFirst sep pa) >> on tail:
    ok <<  head :: tail


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
block as Parser a: Parser a =
    surroundStrict Token.BlockStart Token.BlockEnd


sib as Parser a: Parser a =
    discardFirst (kind Token.NewSiblingLine)


sepListAtSep as Parser sep: Parser item: Parser [sep & item] =
    sep: item:
    sep >> on sep0:
    theParserStillSucks =
        Parser.oneOf
            [ block (sepListAtItem sep item)
            , sib (sepListAtItem sep item)
            , sepListAtItem sep item
            ]
    theParserStillSucks >> on ( item0 & tail ):
    ok << sep0 & item0 :: tail



sepListAtItem as Parser sep: Parser item: Parser (FA.SepList sep item) =
    sep: item:
    item >> on item0:
    theParserStillSucks =
        Parser.oneOf
            [ block (sepListAtSep sep item)
            , sib (sepListAtSep sep item)
            , sepListAtSep sep item
            , ok []
            ]
    theParserStillSucks >> on sepsAndItems:
    ok ( item0 & sepsAndItems )


sepList as Parser sep: Parser item: Parser (FA.SepList sep item) =
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
rawList as Parser a: Parser [a] =
    item:

    sibsep =
        # TODO was:
        #             (Parser.maybe << kind Token.NewSiblingLine) >> on _:
        #             kind Token.Comma
        # but I didn't test it properly
        inlineOrBelowOrIndented << kind Token.Comma

    discardFirst (Parser.maybe sibsep) (oomSeparatedBy sibsep item)



#
# Statements
#
aliasDef as Env: Parser FA.Statement =
    env:

    kind (Token.LowerName Token.NameNoModifier Nothing "alias" []) >> on _:
    upperNameBare env >> on name:
    Parser.zeroOrMore (lowerNameBare env) >> on args:
    kind Token.Defop >> on None:
    inlineOrBelowOrIndented (typeExpr env) >> on ty:
    { name = name
    , args = args
    , ty = ty
    }
    # TODO use ty end instead
    >> FA.AliasDef
    >> ok


unionConstructor as Env: Parser (At Name & [Type]) =
    env:

    typeExpr env >> on type:
    try type as
        FA.TypeConstant p Nothing name args:
            (At p name) & args >> ok

        _:
            Parser.reject


unionDef as Env: Parser FA.Statement =
    env:

    kind (Token.LowerName Token.NameNoModifier Nothing "union" []) >> on _:
    upperNameBare env >> on (At p name):
    Parser.zeroOrMore (lowerNameBare env) >> on args:
    kind Token.Defop >> on None:
    inlineOrBelowOrIndented (rawList (unionConstructor env)) >> on cons:
    { name = name
    , args = List.map Pos.drop args
    , constructors = cons
    }
    >> FA.UnionDef p
    >> ok


#
# Expression
#
exprWithLeftDelimiter as Env: Token.Kind: Parser FA.Expr_ =
    env: k:

    try k as
        Token.Name name:
            FA.Name name >> ok

        Token.NumberLiteral s:
            FA.LiteralNumber s >> ok

        Token.TextLiteral s:
            FA.LiteralText s >> ok

        Token.RoundParen Token.Open:
            exprParser as Parser Expression =
                discardSecond
                    (expr env)
                    (kind (Token.RoundParen Token.Closed))

            inlineOrBelowOrIndented exprParser
            >> on (Expression pos expr_): ok expr_

        Token.SquareBracket Token.Open:
            item as Parser (Bool & Expression) =
                maybe (kind Token.ThreeDots) >> on maybeDots:
                exp env >> on exp:
                ok (maybeDots /= Nothing & exp)

            rawList item >> on exps:
            kind (Token.SquareBracket Token.Closed) >> on _:
            FA.List p exps >> ok

        Token.CurlyBrace Token.Open:
            extension as Parser (Maybe Expression) =
                discardSecond
                    (maybe (expr env))
                    (kind Token.With)

            separator as Parser None =
                oneOf [ kind Token.Defop, kind Token.As ]

            attribute as Parser { name as Name, maybeAnnotation as Maybe Type, maybeExpr as Maybe Expression } =
                word >> on name:
                maybe asAnnotation >> on maybeAnnotation:
                maybe (discardFirst separator (inlineOrBelowOrIndented (expr env))) >> on maybeExpr:
                ok { name, maybeAnnotation, maybeExpr }

            inlineOrBelowOrIndented (maybe extension) >> on maybeExtension:
            rawList attribute >> on attrs:
            FA.Record p { maybeExtension, attrs } >> ok

        Token.Unop unop:
            expr env >> on e:
            FA.Unop p unop e >> ok

        Token.Fn:
            rawList expression >> on args:
            kind Token.Colon >> on _:
            expr env >> on body:
            FA.Fn p args body >> ok

        Token.If:
            expr env >> on condition:
            kind Token.Then >> on _:
            inlineOrBelowOrIndented (expr env) >> on true:
            kind Token.Else >> on _:
            inlineOrBelowOrIndented (expr env) >> on false:
            FA.If p { condition, true, false } >> ok

        Token.Try:
            maybeNewLine as Parser a: Parser a =
                discardFirst (Parser.maybe (kind Token.NewSiblingLine))

            maybeNewLineKind as Token.Kind: Parser Token =
                k:
                maybeNewLine (kind k)

            patternAndValue as Parser (FA.Expression & FA.Expression) =
                pattern env >> on p:
                maybeNewLineKind Token.Colon >> on _:
                inlineStatementOrBlock env >> on value:
                ok ( p & value )

            expr env >> on value:
            kind Token.As >> on _:
            block (Parser.zeroOrMore (maybeNewLine patternAndValue)) >> on patterns:
            here >> on end:
            {
            , value = value
            , patterns = patterns
            }
            >> FA.Try p
            >> ok

        _:
            Parser.reject



expr as Env: Parser FA.Expression =
    env:

    expressionWithLeftDelimiter as Parser FA.Expression =
        oneToken >> on (Token start end k):
        exprWithLeftDelimiter env k >> on expr_:
        Expression (pos env start end) expr_ >> ok

    Parser.expression
        expressionWithLeftDelimiter
        # the `Or` stands for `Or higher priority parser`
        [
        , functionApplicationOr env
        , binopsOr env Op.Exponential
        , binopsOr env Op.Multiplicative
        , binopsOr env Op.Addittive

        # Compops can collapse (ie, `1 < x < 10` => `1 < x && x < 10`)
        , binopsOr env Op.Comparison
        , binopsOr env Op.Logical

        # Tuples are chained (ie, `a & b & c` makes a tuple3)
        , binopsOr env Op.Tuple

        #
        , binopsOr env Op.Cons

        # TODO pipes can't actually be mixed
        , binopsOr env Op.Pipe
        , binopsOr env Op.Mutop
        ]



functionApplicationOr as Env: Parser FA.Expression: Parser FA.Expression =
    env: higher:

    recInlineOrIndented as [FA.Expression]: Parser [FA.Expression] =
        accum:

        higher >> on h:

        r =
            h :: accum

        Parser.oneOf
            # after at least one indented block, allow arguments to appear also as siblings (ie, right below)
            [ block (recInlineOrIndentedOrBelow higher r)
            , recInlineOrIndented r
            , ok r
            ]

    here >> on start:
    recInlineOrIndented [] >> on reversedArgs:
    here >> on end:
    try List.reverse reversedArgs as
        []:
            Parser.reject

        [ fnExpression ]:
            ok fnExpression

        fnExpression :: args:
            FA.call (pos env start end) fnExpression args >> ok



#
# Statements
#


statement as Env: Parser FA.Statement =
    env:
    Parser.breakCircularDefinition _:
    # This is here because inline comments might be followed by NewSiblingLine
    # and I am not sure it's a responsibility of the lexer to deal with it.
    Parser.maybe (kind Token.NewSiblingLine) >> on _:
    Parser.oneOf
        [ aliasDef env
        , unionDef env
        , definition env
        , expr env >> on e:
          e >> FA.Evaluation (FA.expressionPos e) >> ok
        ]


definition as Env: Parser FA.Statement =
    env:
    here >> on start:
    pattern env >> on p:
    Parser.maybe (inlineOrBelowOrIndented (nonFunction env)) >> on nf:
    inlineOrBelowOrIndented (kind Token.Defop) >> on defModifier:
    inlineStatementOrBlock env >> on body:

#    end =
#        body
#            >> List.reverse
#            >> List.head
#            >> Maybe.map getpos
#            >> Maybe.withDefault Pos.T
#            >> Pos.end

    here >> on end:
    { pattern = p
    , body = body
    , nonFn = Maybe.withDefault [] nf
    }
        >> FA.Definition (pos env start end)
        >> ok


inlineStatementOrBlock as Env: Parser [FA.Statement] =
    env:
    Parser.oneOf
        [ Parser.breakCircularDefinition (_: expr env) >> on e: ok [FA.Evaluation (FA.expressionPos e) e]
        , block (oomSeparatedBy (kind Token.NewSiblingLine) (statement env))
        ]


#
# Types
#


nonFunction as Env: Parser [Text] =
    env:
    kind Token.With >> on _:
    rawList (lowerNameBare env) >> on nf:
    upperNameBare env >> on (At _ n):
    if n == "NonFunction" then
        ok << List.map Pos.drop nf
    else
        Parser.abort << "Only NonFunction is supported for now"


typeAnnotation as Env: Parser FA.Type =
    env:

    discardFirst
        (kind Token.As)
        (inlineOrBelowOrIndented (typeExpr env))


#
# Binops
#


binopsOr as Env: Op.Precedence: Parser FA.Expression: Parser FA.Expression =
    env: group: higher:
    here >> on start:
    sepList (binaryOperators group) higher >> on ( head & sepTail ):
    here >> on end:
    if sepTail == [] then
        ok head

    else
        FA.Binop (pos env start end) group ( head & sepTail )
            >> ok


binaryOperators as Op.Precedence: Parser Op.Binop =
    group:
    oneToken >> on (Token s e k):
    try k as
        Token.Binop op:
            if op.precedence == group then
                ok op

            else
                Parser.reject

        _:
            Parser.reject



#
# Module
#
module_ as Env: Parser FA.Module =
    env:

    start =
        Parser.maybe (kind Token.NewSiblingLine)


    e =
        Parser.oneOf
            [ kind Token.BlockEnd
            , kind Token.NewSiblingLine
            ]

    # This is called `zzz` rather than `end` because apparently there is some really
    # bad problems with sorting that result in this (?) being declared before Parser.end?
    zzz =
        Parser.zeroOrMore e >> on _:
        Parser.end

    statements =
        oomSeparatedBy (kind Token.NewSiblingLine) (statement env)

    Parser.oneOf
        [ Parser.map (_: []) Parser.end
        , Parser.surroundWith start zzz statements
        ]


#
# Main
#
makeError as Text: [Token]: Text: Res a =
    moduleName: readState: message:

    p =
        try readState as
            []: Pos.P moduleName 0 1
            Token start end k :: rest: Pos.P moduleName start end

    Error.res p (eenv: [ message ])


parse as Env: [Token]: Res FA.Module =
    env: tokens:

    (failureStates as [[Token]]) & (outcome as Parser.Outcome Token output) =
        tokens
            >> List.filter ((Token s e k): k /= Token.Comment)
            >> Parser.runParser (module_ env)

    try outcome as
        Parser.Accepted readState output:
            Ok output

        Parser.Aborted readState message:
            makeError env.moduleName readState message

        Parser.Rejected:
            findMin = readState: best:
                if List.length readState < List.length best then readState else best

            readState as [Token] =
                List.for failureStates findMin tokens

            message =
                try readState as
                    []: "I got to the end of file and I can't make sense of it. =("
                    _: "I got stuck parsing here. =("

            makeError env.moduleName readState message


textToFormattableModule as Env: Text: Res FA.Module =
    env: code:

    tokensResult as Res [Token] =
        Compiler/Lexer.lexer env.moduleName code

    tokensToStatsResult as [Token]: Res [FA.Statement] =
        tokens:

#        List.each tokens t:
#              log "*" t

        Debug.benchStart None
        parse env tokens
        >> btw Debug.benchStop "parse"

    Result.onOk tokensToStatsResult tokensResult

