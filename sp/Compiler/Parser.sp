
alias Params = {
    , stripLocations as Bool
    , name as Name
    }


textToFormattableModule as Params: Text: Res [FA.Statement] =
    pars: code:

    tokensResult as Res [Token] =
        Compiler/Lexer.lexer pars.name code

    tokensToStatsResult as [Token]: Res [FA.Statement] =
        tokens:
        SPCore.benchStart None
        parse pars.stripLocations pars.name tokens
            >> btw SPCore.benchStop "parse"

    Result.onOk tokensToStatsResult tokensResult


#
#
#

alias Parser a =
    Parser.Parser Token a


alias Env =
    { moduleName as Text
    , stripLocations as Bool
    }


#
# Helpers
#


cb as Parser a: (a: Parser b): Parser b =
    a: b: Parser.andThen b a


here as Parser Int =

    tokens ?
        Parser.here >> cb

    Parser.accept
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


makeError as Text: [Token]: Text: Res a =
    moduleName: readState: message:

    p =
        try readState as
            []: Pos.P moduleName 0 1
            Token start end k :: rest: Pos.P moduleName start end

    Error.res p (eenv: [ message ])


# This is just a pr attempt at getting some sort of parser debugging
#palog as Text: Parser Text =
#    m:
#    Parser.accept None >> andThen _:
#    Parser.accept (Debug.log "->" m)


#
# Main
#
parse as Bool: Text: [Token]: Res [FA.Statement] =
    stripLocations: moduleName: tokens:

    parser =
        module_ { moduleName, stripLocations }

    runParser moduleName parser tokens


runParser as Text: Parser output: [Token]: Res output =
    moduleName: parser: tokens:

    (failureStates as [[Token]]) & (outcome as Parser.Outcome Token output) =
        tokens
            >> List.filter ((Token s e k): k /= Token.Comment)
            >> Parser.runParser parser

    try outcome as
        Parser.Accepted readState output:
            Ok output

        Parser.Aborted readState message:
            makeError moduleName readState message

        Parser.Rejected:
            findMin = readState: best:
                if List.length readState < List.length best then readState else best

            readState as [Token] =
                List.for failureStates findMin tokens

            message =
                try readState as
                    []: "I got to the end of file and I can't make sense of it. =("
                    _: "I got stuck parsing here. =("

            makeError moduleName readState message


module_ as Env: Parser [FA.Statement] =
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
        _ ? Parser.zeroOrMore e >> cb
        Parser.end

    statements =
        oomSeparatedBy (kind Token.NewSiblingLine) (statement env)

    Parser.oneOf
        [ Parser.map (_: []) Parser.end
        , Parser.surroundWith start zzz statements
        ]


#
# Terms
#
oneToken as Parser Token =
    Parser.consumeOne


kind as Token.Kind: Parser Token =
    targetKind:
    token ? oneToken >> cb
    (Token _ _ k) = token
    if targetKind == k then
        Parser.accept token

    else
        Parser.reject


upperNameBare as Env: Parser (At Text) =
    env:
    token ? oneToken >> cb
    try token as
        Token start end (Token.UpperName Nothing name):
            Parser.accept << At (pos env start end) name
        _:
            Parser.reject


lowerNameBare as Env: Parser (At Text) =
    env:
    token ? oneToken >> cb
    try token as
        Token start end (Token.LowerName Token.NameNoModifier Nothing name []):
            Parser.accept << At (pos env start end) name
        _:
            Parser.reject


defop as Parser Token.DefModifier =

    token ? oneToken >> cb
    try token as
        Token _ _ (Token.Defop mod):
            Parser.accept mod

        _:
            Parser.reject



#
# Combinators
#
discardFirst as Parser a: Parser b: Parser b =
    a: b:
    cb a (_: b)


discardSecond as Parser a: Parser b: Parser a =
    a: b:
    aa ? a >> cb
    _ ? b >> cb
    Parser.accept aa


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
    Parser.oneOf [ p, Parser.accept a ]


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
    head ? pa >> cb
    tail ? Parser.zeroOrMore (discardFirst sep pa) >> cb
    Parser.accept <<  head :: tail


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
    sep0 ? sep >> cb
    theParserStillSucks =
        Parser.oneOf
            [ block (sepListAtItem sep item)
            , sib (sepListAtItem sep item)
            , sepListAtItem sep item
            ]
    item0 & tail ? theParserStillSucks >> cb
    Parser.accept << sep0 & item0 :: tail



sepListAtItem as Parser sep: Parser item: Parser (FA.SepList sep item) =
    sep: item:
    item0 ? item >> cb
    theParserStillSucks =
        Parser.oneOf
            [ block (sepListAtSep sep item)
            , sib (sepListAtSep sep item)
            , sepListAtSep sep item
            , Parser.accept []
            ]
    sepsAndItems ? theParserStillSucks >> cb
    Parser.accept ( item0 & sepsAndItems )


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
        #             (Parser.maybe << kind Token.NewSiblingLine) >> andThen _:
        #             kind Token.Comma
        # but I didn't test it properly
        inlineOrBelowOrIndented << kind Token.Comma

    discardFirst (Parser.maybe sibsep) (oomSeparatedBy sibsep item)



#
# Statements
#
errorShouldUseDefNormalHere as Text =
    "You should use a normal `=` here."


typeAlias as Env: Parser FA.Statement =
    env:

    _ ?
        kind (Token.LowerName Token.NameNoModifier Nothing "alias" []) >> cb

    name ?
        upperNameBare env >> cb

    args ?
        Parser.zeroOrMore (lowerNameBare env) >> cb

    defModifier ?
        defop >> cb

    ty ?
        inlineOrBelowOrIndented (typeExpr env) >> cb


    if defModifier /= Token.DefNormal then
        Parser.abort errorShouldUseDefNormalHere

    else
        { name = name
        , args = args
        , ty = ty
        }
            # TODO use ty end instead
            >> FA.TypeAlias
            >> Parser.accept


unionDef as Env: Parser FA.Statement =
    env:

    _ ?
        kind (Token.LowerName Token.NameNoModifier Nothing "union" []) >> cb

    At p name ?
        upperNameBare env >> cb

    args ?
        Parser.zeroOrMore (lowerNameBare env) >> cb

    defModifier ?
        defop >> cb

    cons ?
        inlineOrBelowOrIndented (rawList (unionConstructor env)) >> cb

    if defModifier /= Token.DefNormal then
        Parser.abort errorShouldUseDefNormalHere

    else
        { name = name
        , args = List.map Pos.drop args
        , constructors = cons
        }
            >> FA.UnionDef p
            >> Parser.accept


unionConstructor as Env: Parser FA.Constructor =
    env:

    type ?
        typeExpr env >> cb

    try type as
        FA.TypeConstant p Nothing name args:
            Parser.accept << (At p name) & args

        _:
            Parser.reject


#
# Term
#

#term env =
#    as Env: Parser FA.Expression
#
#    oneToken >> andThen (Token start end k):
#
#    p =
#        pos env start end
#
#    try k as
#        Token.NumberLiteral s:
#            Parser.accept << FA.LiteralNumber p s
#
#        Token.TextLiteral s:
#            Parser.accept << FA.LiteralText p s
#
#        Token.UpperName maybeModule name:
#            Parser.accept << FA.Constructor p maybeModule name
#
#        Token.LowerName modifier maybeModule name attrs:
#            try modifier as
#                Token.NameNoModifier:
#                    # This is a HACK and probably the world would be a better place if I cleaned this up
#                    # TODO clean this up once I have a better way to debug the parser
#                    Parser.maybe lambdaColon >> andThen maybeColon:
#                        try maybeColon as
#                            Nothing:
#                                Parser.accept << FA.Variable p maybeModule name attrs
#
#                            Just mutable:
#                                lambdaBody env >> andThen b:
#                                Parser.accept << FA.Lambda p (FA.PatternAny p False name Nothing) mutable b
#
#                Token.NameMutable:
#                    Parser.accept << FA.Mutable p name attrs
#
#                Token.NameStartsWithDot:
#                    Parser.accept << FA.RecordShorthand p (name :: attrs)
#
#        _:
#            Parser.reject



exprWithLeftDelimiter as Env: Parser FA.Expression =
    env:

    colon =
        Parser.oneOf
            [ kind Token.Colon >> Parser.map _: False
            , kind Token.MutableColon >> Parser.map _: True
            ]

    maybeColon =
        Parser.maybe colon

    Token start end k ?
        oneToken >> cb

    p =
        pos env start end

    try k as
        Token.NumberLiteral s:
            mc ?
                maybeColon >> cb

            try mc as
                Nothing: Parser.accept << FA.LiteralNumber p s
                Just mutable: lambdaParser env mutable (FA.PatternLiteralNumber p s)


        Token.TextLiteral s:
            mc ?
                maybeColon >> cb

            try mc as
                Nothing: Parser.accept << FA.LiteralText p s
                Just mutable: lambdaParser env mutable (FA.PatternLiteralText p s)

        Token.LowerName modifier maybeModule name attrs:
            try modifier as
                Token.NameMutable:
                    Parser.accept << FA.Mutable p name attrs

                Token.NameStartsWithDot:
                    Parser.accept << FA.RecordShorthand p (name :: attrs)

                Token.NameNoModifier:
                    mc ?
                        maybeColon >> cb

                    try mc as
                        Nothing: Parser.accept << FA.Variable p maybeModule name attrs
                        # TODO also test that maybeModule == Nothing and attrs == []
                        Just mutable: lambdaParser env mutable (FA.PatternAny p False name Nothing)

        Token.UpperName maybeModule name:
            mc ?
                maybeColon >> cb

            try mc as
                Nothing: Parser.accept << FA.Constructor p maybeModule name
                Just mutable: lambdaParser env mutable (FA.PatternConstructor p maybeModule name [])

        Token.RoundParen Token.Open:
            paParser =
                pa ?
                    pattern env >> cb

                _ ?
                    kind (Token.RoundParen Token.Closed) >> cb

                mutable ?
                    colon >> cb

                lambdaParser env mutable pa

            exprParser =
                discardSecond
                    (expr env)
                    (kind (Token.RoundParen Token.Closed))

            inlineOrBelowOrIndented << Parser.oneOf [ paParser, exprParser ]

#            Token.SquareBracket Token.Open:
#                paParser =
#                    rawList (pattern env) >> andThen pas:
#                    kind (Token.SquareBracket Token.Closed) >> andThen _:
#                    colon >> andThen mutable:
#                    lambdaParser env mutable (FA.PatternList p pas)
#
#                exprParser =
#                    rawList (expr env) >> andThen exps:
#                    kind (Token.SquareBracket Token.Closed) >> andThen _:
#                    Parser.accept << FA.List p exps
#
#                Parser.oneOf [ paParser, exprParser ]

#            Token.CurlyBrace Token.Open
#                attrs, close brace, lambdaOrAppl

        _:
            Parser.reject

#            Token.If
#                expr, then, body, else, body
#            Token.Try
#                expr, as, pasAndBodies


lambdaParser as Env: Bool: FA.Pattern: Parser FA.Expression =
    env: mutable: pa:

    body ?
        lambdaBody env >> cb

    Parser.accept << FA.Lambda (FA.patternPos pa) pa mutable body



#
# Expr (with precedence rules)
#


expr as Env: Parser FA.Expression =
    env:

    higherOr =
        Parser.higherOr

    nest =
        Parser.breakCircularDefinition _: expr env

    Parser.expression
        (exprWithLeftDelimiter env)
        # the `Or` stands for `Or higher priority parser`
        [
#        , higherOr << parens (Parser.oneOf [ binopInsideParens env, nest ])
        , higherOr << list env FA.List nest
        , higherOr << record env (Token.Defop Token.DefNormal) FA.Record nest
#        , higherOr << lambda env
        , unopsOr env
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
        , higherOr << if_ env
        , higherOr << try_ env
        ]




#
# Parens
#


parens as Parser a: Parser a =
    Parser.surroundWith
        (kind << Token.RoundParen Token.Open)
        (inlineOrBelowOrIndented << kind << Token.RoundParen Token.Closed)



#
# List
#


list as Env: (Pos: List a: a): Parser a: Parser a =
    env: constructor: main:
    start ?
        here >> cb

    maybeLs ?
        surroundMultiline (Token.SquareBracket Token.Open) (Token.SquareBracket Token.Closed) (Parser.maybe (rawList main)) >> cb

    end ?
        here >> cb

    theParserStillSucks =
        try maybeLs as
            Just ls:
                ls

            Nothing:
                []

    theParserStillSucks
        >> constructor (pos env start end)
        >> Parser.accept



#
# Record
#


record as Env: Token.Kind: (Pos: FA.RecordArgs a: a): Parser a: Parser a =
    env: assign: constructor: main:

    attrAssignment =
        discardFirst (kind assign) (inlineOrBelowOrIndented main)

    attr =
        name ?
            lowerNameBare env >> cb

        maybeAssignment ?
            Parser.maybe attrAssignment >> cb

        Parser.accept ( name & maybeAssignment )

    updateTarget =
        h ?
            main >> cb

        _ ?
            kind Token.With >> cb

        Parser.accept h

    content =
        start:
        maybeUpdateTarget ?
            Parser.maybe updateTarget >> cb

        attrs ?
            attr
                >> rawList
                >> inlineOrBelowOrIndented
                >> cb

        end ?
            here >> cb

        { extends = maybeUpdateTarget
        , attrs = attrs
        }
            >> constructor (pos env start end)
            >> Parser.accept

    s ?
        here >> cb

    maybeRecord ?
        surroundMultiline (Token.CurlyBrace Token.Open) (Token.CurlyBrace Token.Closed) (Parser.maybe << content s) >> cb

    e ?
        here >> cb

    try maybeRecord as
        Just re:
            Parser.accept re

        Nothing:
            { extends = Nothing
            , attrs = []
            }
                >> constructor (pos env s e)
                >> Parser.accept



#
# if..else
#


if_ as Env: Parser FA.Expression =
    env:

    maybeNewLine = k:
        discardFirst
            (Parser.maybe (kind Token.NewSiblingLine))
            (kind k)

    Token start _ _ ?
        kind Token.If >> cb

    condition ?
        expr env >> cb

    maybeThen ?
        Parser.maybe (maybeNewLine Token.Then) >> cb

    if maybeThen == Nothing then
        Parser.abort "`if` should be followed by a `then` but I can't find it"

    else
        true ?
            inlineStatementOrBlock env >> cb

        _ ?
            maybeNewLine Token.Else >> cb

        _ ?
            Parser.maybe (kind Token.Colon) >> cb

        false ?
            inlineStatementOrBlock env >> cb

        end ?
            here >> cb

        { isCompact = False
        , condition = condition
        , true = true
        , false = false
        }
            >> FA.If (pos env start end)
            >> Parser.accept



#
# try..as
#


try_ as Env: Parser FA.Expression =
    env:

    maybeNewLine as Parser a: Parser a =
        discardFirst (Parser.maybe (kind Token.NewSiblingLine))

    maybeNewLineKind as Token.Kind: Parser Token =
        k:
        maybeNewLine (kind k)

    patternAndAccept =
        p ?
            pattern env >> cb

        _ ?
            maybeNewLineKind Token.Colon >> cb

        accept ?
            inlineStatementOrBlock env >> cb

        Parser.accept ( p & accept )

    Token start _ _ ?
        kind Token.Try >> cb

    value ?
        expr env >> cb

    _ ?
        maybeNewLineKind Token.As >> cb

    patterns ?
        block (Parser.zeroOrMore (maybeNewLine patternAndAccept)) >> cb

    end ?
        here >> cb

    { isCompact = False
    , value = value
    , patterns = patterns
    }
        >> FA.Try (pos env start end)
        >> Parser.accept



#
# Statements
#


statement as Env: Parser FA.Statement =
    env:
    Parser.breakCircularDefinition _:
    # This is here because inline comments might be followed by NewSiblingLine
    # and I am not sure it's a responsibility of the lexer to deal with it.
    _ ?
        Parser.maybe (kind Token.NewSiblingLine) >> cb

    eval =
       e ?
           expr env >> cb
       e
           >> FA.Evaluation (FA.expressionPos e)
           >> Parser.accept

    Parser.oneOf
        [ typeAlias env
        , unionDef env
        , definition env
        , eval
        ]


definition as Env: Parser FA.Statement =
    env:

    start ?
        here >> cb

    p ?
        pattern env >> cb

    nf ?
        Parser.maybe (inlineOrBelowOrIndented (nonFunction env)) >> cb

    defModifier ?
        inlineOrBelowOrIndented defop >> cb

    body ?
        inlineStatementOrBlock env >> cb

#    end =
#        body
#            >> List.reverse
#            >> List.head
#            >> Maybe.map getpos
#            >> Maybe.withDefault Pos.T
#            >> Pos.end

    end ?
        here >> cb

    { pattern = p
    , modifier = defModifier
    , body = body
    , nonFn = Maybe.withDefault [] nf
    }
        >> FA.Definition (pos env start end)
        >> Parser.accept


inlineStatementOrBlock as Env: Parser [FA.Statement] =
    env:

    blah =
        e ?
            Parser.breakCircularDefinition (_: expr env) >> cb

        Parser.accept [FA.Evaluation (FA.expressionPos e) e]

    Parser.oneOf
        [ blah
        , block (oomSeparatedBy (kind Token.NewSiblingLine) (statement env))
        ]


#
# Types
#


nonFunction as Env: Parser [Text] =
    env:
    _ ?
        kind Token.With >> cb

    nf ?
        rawList (lowerNameBare env) >> cb

    At _ n ?
        upperNameBare env >> cb

    if n == "NonFunction" then
        Parser.accept << List.map Pos.drop nf
    else
        Parser.abort << "Only NonFunction is supported for now"


typeTerm as Env: Parser FA.Type =
    env:

    Token start end k ?
        oneToken >> cb

    try k as
        Token.UpperName maybeModule name:
            Parser.accept << FA.TypeConstant (pos env start end) maybeModule name []

        Token.LowerName Token.NameNoModifier Nothing name []:
            Parser.accept << FA.TypeVariable (pos env start end) name

        _:
            Parser.reject


typeExpr as Env: Parser FA.Type =
    env:

    nest =
        Parser.breakCircularDefinition _: typeExpr env

    higherOr =
        # "higher priority parser or"
        Parser.higherOr

    Parser.expression
        (typeTerm env)
        [ higherOr << typeParens nest
        , higherOr << typeList env nest
        , higherOr << record env Token.As FA.TypeRecord nest
        , typeConstructorAppOr env
        , typeTupleOr env
        , typeFunctionOr env
        ]


typeTupleOr as Env: Parser FA.Type: Parser FA.Type =
    env: higher:

    binopAndPrev as Parser FA.Type =
        discardFirst (binaryOperators Op.Tuple) higher

    start ?
        here >> cb

    head ?
        higher >> cb

    tail ?
        Parser.zeroOrMore binopAndPrev >> cb

    end ?
        here >> cb

    if tail == [] then
        Parser.accept head

    else
        (head :: tail)
            >> FA.TypeTuple (pos env start end)
            >> Parser.accept


typeParens as Parser FA.Type: Parser FA.Type =
    main:
    surroundStrict
        (Token.RoundParen Token.Open)
        (Token.RoundParen Token.Closed)
        main


typeList as Env: Parser FA.Type: Parser FA.Type =
    env: main:
    start ?
        here >> cb

    t ?
        surroundStrict (Token.SquareBracket Token.Open) (Token.SquareBracket Token.Closed) main >> cb

    end ?
        here >> cb

    Parser.accept << FA.TypeList (pos env start end) t


typeFunctionOr as Env: Parser FA.Type: Parser FA.Type =
    env: higher:

    arrowAndHigher as Parser ( Bool & Pos & FA.Type ) =
        mutable & p ?
            arrow env >> cb

        h ?
            higher >> cb

        Parser.accept ( mutable & p & h )

    fold as ( Bool & Pos & FA.Type ): ( Bool & FA.Type ): ( Bool & FA.Type ) =

        ( nextIsMutable & p & ty ):
        ( thisIsMutable & accum ):

        ( nextIsMutable & FA.TypeFunction p ty thisIsMutable accum)

    fs ?
        here >> cb

    e ?
        higher >> cb

    fe ?
        here >> cb

    es ?
        Parser.zeroOrMore arrowAndHigher >> cb

    firstPos =
        pos env fs fe

    # This used to be OneOrMore.reverse, maybe there is a better way to rewrite this?
    reverseRec as a: [a]: [a]: a & [a] =
        a: ls: accum:
        try ls as
            []:
                a & accum

            head :: tail:
                reverseRec head tail (a :: accum)

    ( ( thisIsMutable & p & return ) & reversedArgs ) =
        reverseRec ( False & firstPos & e ) es []

    thisIsMutable & return
        >> List.for reversedArgs fold
        >> x: x.second
        >> Parser.accept


# TODO this is not an "arrow" any more
arrow as Env: Parser ( Bool & Pos ) =
    env:

    Token start end k ?
        oneToken >> cb

    try k as
        Token.Colon:
            Parser.accept ( False & (pos env start end) )

        Token.MutableColon:
            Parser.accept ( True & (pos env start end) )

        _:
            Parser.reject


typeConstructorAppOr as Env: Parser FA.Type: Parser FA.Type =
    env: higher:

    ty ?
        higher >> cb

    try ty as
        FA.TypeConstant p1 maybeModule name []:
            args ?
                Parser.zeroOrMore higher >> cb

            end2 ?
                here >> cb

            if args == [] then
                Parser.accept ty

            else
                Parser.accept << FA.TypeConstant p1 maybeModule name args

        _:
            Parser.accept ty


#
# Lambda
#

#lambdaColon =
#    as Parser Bool
#
#    Parser.oneOf
#        [ kind Token.Colon >> andThen _: Parser.accept False
#        , kind Token.MutableColon >> andThen _: Parser.accept True
#        ]




lambdaBody as Env: Parser [FA.Statement] =
    env:
    Parser.oneOf
        [ [#
             x:
             a
             b
             c
          #]
        , Parser.oneOrMore (sib (statement env)) >> Parser.andThen (h & t):
          Parser.accept << h :: t
          [#
             x: a

             x:
               a

          #]
        , inlineStatementOrBlock env
        ]


#lambda env =
#    as Env: Parser FA.Expression
#
#    pattern env >> andThen param:
#    lambdaColon >> andThen mutable:
#    lambdaBody env >> andThen b:
#    Parser.accept << FA.Lambda (FA.patternPos param) param mutable b


#
# Pattern
#
pattern as Env: Parser FA.Pattern =
    env:

    nest =
        Parser.breakCircularDefinition _: pattern env

    higherOr =
        Parser.higherOr

    Parser.expression
        (patternApplication env << functionParameter env nest)
        # the `Or` stands for `Or higher priority parser`
        [ higherOr << parens nest
        , higherOr << list env FA.PatternList nest
        , higherOr << record env (Token.Defop Token.DefNormal) FA.PatternRecord nest
        , patternBinopOr env Op.Cons FA.PatternListCons
        , patternBinopOr env Op.Tuple FA.PatternTuple
        ]


# TODO maybe the whole pattern -> functionParameter -> patternApplication mess can be cleaned up?
functionParameter as Env: Parser FA.Pattern: Parser FA.Pattern =
    env: nest:
    Parser.oneOf
        [ patternApplication env Parser.reject
        , parens nest
        , list env FA.PatternList nest
        , record env (Token.Defop Token.DefNormal) FA.PatternRecord nest
        ]


patternApplication as Env: Parser FA.Pattern: Parser FA.Pattern =
    env: param:

    Token start end k ?
        oneToken >> cb

    p =
        pos env start end

    try k as
        Token.NumberLiteral s:
            s
                >> FA.PatternLiteralNumber p
                >> Parser.accept

        Token.TextLiteral s:
            s
                >> FA.PatternLiteralText p
                >> Parser.accept

        Token.LowerName modifier Nothing name []:
            thingy =
                mutable:

                maybeTy ?
                    Parser.maybe (inlineOrBelowOrIndented << typeAnnotation env) >> cb

                Parser.accept << FA.PatternAny p mutable name maybeTy

            try modifier as
                Token.NameNoModifier: thingy False
                Token.NameMutable: thingy True
                Token.NameStartsWithDot: Parser.reject

        Token.UpperName maybeModule name:
            params ?
                Parser.zeroOrMore param >> cb

            end1 ?
                here >> cb

            Parser.accept << FA.PatternConstructor (pos env start end1) maybeModule name params

        _:
            Parser.reject


typeAnnotation as Env: Parser FA.Type =
    env:

    discardFirst
        (kind Token.As)
        (inlineOrBelowOrIndented (typeExpr env))


patternBinopOr as Env: Op.Precedence: (Pos: [FA.Pattern]: FA.Pattern): Parser FA.Pattern: Parser FA.Pattern =
    env: precedenceGroup: constructor: higher:

    start ?
        here >> cb

    head & sepTail ?
        sepList (binaryOperators precedenceGroup) higher >> cb

    end ? here >> cb

    if sepTail == [] then
        Parser.accept head

    else
        (head :: List.map (x: x.second) sepTail)
            >> constructor (pos env start end)
            >> Parser.accept


#
# Function application
#

recInlineOrIndentedOrBelow as Parser FA.Expression: [FA.Expression]: Parser [FA.Expression] =
    higher: accum:
    h ?
        higher >> cb

    r =
        h :: accum

    maybeWithDefault r << inlineOrBelowOrIndented (recInlineOrIndentedOrBelow higher r)


functionApplicationOr as Env: Parser FA.Expression: Parser FA.Expression =
    env: higher:

    recInlineOrIndented as [FA.Expression]: Parser [FA.Expression] =
        accum:

        h ?
            higher >> cb

        r =
            h :: accum

        Parser.oneOf
            # after at least one indented block, allow arguments to appear also as siblings (ie, right below)
            [ block (recInlineOrIndentedOrBelow higher r)
            , recInlineOrIndented r
            , Parser.accept r
            ]

    start ?
        here >> cb

    reversedArgs ?
        recInlineOrIndented [] >> cb

    end ?
        here >> cb

    try List.reverse reversedArgs as
        []:
            Parser.reject

        [ fnExpression ]:
            Parser.accept fnExpression

        fnExpression :: args:
            Parser.accept << FA.FunctionCall (pos env start end) fnExpression args



#
# Unops
#


unopsOr as Env: Parser FA.Expression: Parser FA.Expression =
    env: higher:

    maybeUnary ?
        Parser.maybe unaryOperator >> cb

    right ?
        higher >> cb

    end ?
        here >> cb

    try maybeUnary as
        Just ( op & Token start _ _ ):
            Parser.accept << FA.Unop (pos env start end) op right

        Nothing:
            Parser.accept right


unaryOperator as Parser ( Op.Unop & Token ) =

    token ?
        oneToken >> cb

    try token as
        Token s e (Token.Unop op):
            Parser.accept ( op & token )

        _:
            Parser.reject



#
# Binops
#


binopInsideParens as Env: Parser FA.Expression =
    env:

    Token start end k ?
        oneToken >> cb

    try k as
        Token.Binop binop:
            Parser.accept << FA.PrefixBinop (pos env start end) binop.symbol

        _:
            Parser.reject


binopsOr as Env: Op.Precedence: Parser FA.Expression: Parser FA.Expression =
    env: group: higher:

    start ?
        here >> cb

    head & sepTail ?
        sepList (binaryOperators group) higher >> cb

    end ?
        here >> cb

    if sepTail == [] then
        Parser.accept head

    else
        FA.Binop (pos env start end) group ( head & sepTail )
            >> Parser.accept


binaryOperators as Op.Precedence: Parser Op.Binop =
    group:

    Token s e k ?
        oneToken >> cb

    try k as
        Token.Binop op:
            if op.precedence == group then
                Parser.accept op

            else
                Parser.reject

        _:
            Parser.reject

