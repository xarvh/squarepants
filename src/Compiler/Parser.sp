
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
        Debug.benchStart None
        parse pars.stripLocations pars.name tokens
            >> btw Debug.benchStop "parse"

    Result.onOk tokensToStatsResult tokensResult


#
#
#

alias Parser a =
    Parser.Parser Token a


alias Env =
    { moduleName as Text
    , stripLocations as Bool
    , parentIndent as Int
    }


#
# Helpers
#
andThen =
    Parser.andThen


here as Parser Int =
    Parser.here >> andThen tokens:
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
        module_ {
          , moduleName
          , stripLocations
          , parentIndent = 0
          }

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

    Parser.zeroOrMore (statement env)


#
# Terms
#
oneToken as Parser Token =
    Parser.consumeOne


kind as Token.Kind: Parser Token =
    targetKind:
    oneToken >> andThen token:
    (Token _ _ k) = token
    if targetKind == k then
        Parser.accept token

    else
        Parser.reject


upperNameBare as Env: Parser (At Text) =
    env:
    oneToken >> andThen token:
    try token as
        Token start end (Token.UpperName Nothing name):
            Parser.accept << At (pos env start end) name
        _:
            Parser.reject


lowerNameBare as Env: Parser (At Text) =
    env:
    oneToken >> andThen token:
    try token as
        Token start end (Token.LowerName Token.NameNoModifier Nothing name []):
            Parser.accept << At (pos env start end) name
        _:
            Parser.reject


defop as Parser None =

    oneToken >> andThen token:
    try token as
        Token _ _ Token.Defop:
            Parser.accept None

        _:
            Parser.reject


ind as Parser Int =

    oneToken >> andThen token:
    try token as
        Token _ _ (Token.Indent indent):
            Parser.accept indent

        _:
            Parser.reject



maybeIndented as Parser a: Parser a =
    p:

    Parser.oneOf [
        , discardFirst ind p
        , p
        ]


#
# Combinators
#
discardFirst as Parser a: Parser b: Parser b =
    a: b:
    a >> andThen _: b


discardSecond as Parser a: Parser b: Parser a =
    a: b:
    a >> andThen aa:
    b >> andThen _:
    Parser.accept aa


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
        (maybeIndented
            (discardSecond
                content
                (maybeIndented (kind right))
            )
        )


oomSeparatedBy as Parser a: Parser b: Parser [b] =
    sep: pa:
    pa >> andThen head:
    Parser.zeroOrMore (discardFirst sep pa) >> andThen tail:
    Parser.accept <<  head :: tail



sepListAtSep as Parser sep: Parser item: Parser [sep & item] =
    sep: item:
    sep >> andThen sep0:

    maybeIndented (sepListAtItem sep item) >> andThen ( item0 & tail ):
    Parser.accept << sep0 & item0 :: tail



sepListAtItem as Parser sep: Parser item: Parser (FA.SepList sep item) =
    sep: item:
    item >> andThen item0:
    theParserStillSucks =
        Parser.oneOf
            [ maybeIndented (sepListAtSep sep item)
            , Parser.accept []
            ]
    theParserStillSucks >> andThen sepsAndItems:
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
        maybeIndented << kind Token.Comma

    discardFirst (Parser.maybe sibsep) (oomSeparatedBy sibsep item)



#
# Statements
#


afterEqualOrColon as Env: Parser [FA.Statement] =
    env:

    Parser.oneOf [
        , evaluation env >> Parser.map e: [e]
        , Parser.oneOrMore (statement env) >> Parser.map (h & t): (h :: t)
        ]


indentGreaterThanParentIndent as Env: Parser Int =
    env:

    ind
    >> andThen indent:

    if indent > env.parentIndent then
        Parser.accept indent
    else
        Parser.reject


statement as Env: Parser FA.Statement =
    env:

    indentGreaterThanParentIndent env
    >> andThen indent:

    newEnv =
        { env with parentIndent = indent }

    Parser.oneOf [
      , typeAlias env
      , unionDef env
      , evaluation env
      , definition env
      ]


evaluation as Env: Parser FA.Statement =
    env:

    expr env
    >> andThen e:

    e
    >> FA.Evaluation (FA.expressionPos e)
    >> Parser.accept



typeAlias as Env: Parser FA.Statement =
    env:

    kind (Token.LowerName Token.NameNoModifier Nothing "alias" []) >> andThen _:
    upperNameBare env >> andThen name:
    Parser.zeroOrMore (lowerNameBare env) >> andThen args:
    defop >> andThen None:
    maybeIndented (typeExpr env) >> andThen ty:
        { name = name
        , args = args
        , ty = ty
        }
            # TODO use ty end instead
            >> FA.TypeAlias
            >> Parser.accept


unionDef as Env: Parser FA.Statement =
    env:

    kind (Token.LowerName Token.NameNoModifier Nothing "union" []) >> andThen _:
    upperNameBare env >> andThen (At p name):
    Parser.zeroOrMore (lowerNameBare env) >> andThen args:
    defop >> andThen None:
    maybeIndented (rawList (unionConstructor env)) >> andThen cons:
        { name = name
        , args = List.map Pos.drop args
        , constructors = cons
        }
            >> FA.UnionDef p
            >> Parser.accept


unionConstructor as Env: Parser FA.Constructor =
    env:

    typeExpr env >> andThen type:
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

    maybeColon as Parser (Maybe LambdaModifier) =
        Parser.maybe (colon env >> Parser.map Tuple.first)

    oneToken >> andThen (Token start end k):

        p =
            pos env start end

        try k as
            Token.NumberLiteral s:
                maybeColon >> andThen mc:
                    try mc as
                        Nothing: Parser.accept << FA.LiteralNumber p s
                        Just modifier: lambdaParser env modifier (FA.PatternLiteralNumber p s)

            Token.TextLiteral s:
                maybeColon >> andThen mc:
                    try mc as
                        Nothing: Parser.accept << FA.LiteralText p s
                        Just modifier: lambdaParser env modifier (FA.PatternLiteralText p s)

            Token.LowerName modifier maybeModule name attrs:
                log "-------" { modifier, name }
                try modifier as
#                    Token.NameMutable:
#                        maybeColon
#                        >> andThen mc:
#                            try mc as
#                                Nothing: Parser.accept << FA.Mutable p name attrs
#                                # TODO also test that maybeModule == Nothing and attrs == []
#                                Just modifier: lambdaParser env modifier (FA.PatternAny p True name Nothing)

                    Token.NameStartsWithDot:
                        Parser.accept << FA.RecordShorthand p (name :: attrs)

                    Token.NameNoModifier:
                        maybeColon >> andThen mc:
                            try mc as
                                Nothing:
                                    Parser.accept << FA.Variable p maybeModule name attrs

                                # TODO also test that maybeModule == Nothing and attrs == []
                                Just modifier:
                                    lambdaParser env modifier (FA.PatternAny p False name Nothing)

            Token.UpperName maybeModule name:
                maybeColon >> andThen mc:
                    try mc as
                        Nothing: Parser.accept << FA.Constructor p maybeModule name
                        Just modifier: lambdaParser env modifier (FA.PatternConstructor p maybeModule name [])

            Token.RoundParen Token.Open:
                paParser as Parser FA.Expression =
                    pattern env >> andThen pa:
                    kind (Token.RoundParen Token.Closed) >> andThen _:
                    colon env >> andThen (lambdaModifier & pos):
                    lambdaParser env lambdaModifier pa

                exprParser as Parser FA.Expression =
                    discardSecond
                        (expr env)
                        (kind (Token.RoundParen Token.Closed))

                maybeIndented << Parser.oneOf [ paParser, exprParser ]

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


lambdaParser as Env: LambdaModifier: FA.Pattern: Parser FA.Expression =
    env: modifier: pa:

    afterEqualOrColon env >> andThen body:
    Parser.accept << FA.Lambda (FA.patternPos pa) pa modifier body



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
        , higherOr << record env Token.Defop FA.Record nest
#        , higherOr << lambda env
        , unopsOr env
#        , mutableOr env
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



mutableOr as Env: Parser FA.Expression: Parser FA.Expression =
    env: higher:

    kind Token.Mutop >> andThen (Token start end k):
    higher >> andThen n:
    Parser.accept << FA.Mutable (pos env start end) n





#
# Parens
#


parens as Parser a: Parser a =
    Parser.surroundWith
        (kind << Token.RoundParen Token.Open)
        (maybeIndented << kind << Token.RoundParen Token.Closed)



#
# List
#


list as Env: (Pos: List a: a): Parser a: Parser a =
    env: constructor: main:
    here >> andThen start:
    surroundMultiline (Token.SquareBracket Token.Open) (Token.SquareBracket Token.Closed) (Parser.maybe (rawList main)) >> andThen maybeLs:
    here >> andThen end:
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
        discardFirst (kind assign) (maybeIndented main)

    attr =
        lowerNameBare env >> andThen name:
        Parser.maybe attrAssignment >> andThen maybeAssignment:
        Parser.accept ( name & maybeAssignment )

    updateTarget =
        main >> andThen h:
        kind Token.With >> andThen _:
        Parser.accept h

    content =
        start:
        Parser.maybe updateTarget >> andThen maybeUpdateTarget:
        (maybeIndented << rawList attr) >> andThen attrs:
        here >> andThen end:
        { extends = maybeUpdateTarget
        , attrs = attrs
        }
            >> constructor (pos env start end)
            >> Parser.accept

    here >> andThen s:
    surroundMultiline (Token.CurlyBrace Token.Open) (Token.CurlyBrace Token.Closed) (Parser.maybe << content s) >> andThen maybeRecord:
    here >> andThen e:
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

    kind Token.If >> andThen (Token start _ _):
    expr env >> andThen condition:
    Parser.maybe (maybeIndented (kind Token.Then)) >> andThen maybeThen:
    if maybeThen == Nothing then
        Parser.abort "`if` should be followed by a `then` but I can't find it"

    else
        afterEqualOrColon env >> andThen true:
        maybeIndented (kind Token.Else) >> andThen _:
        afterEqualOrColon env >> andThen false:
        here >> andThen end:
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

    patternAndAccept =
        pattern env >> andThen p:
        colon env >> andThen _:
        afterEqualOrColon env >> andThen accept:
        Parser.accept ( p & accept )

    kind Token.Try >> andThen (Token start _ _):
    expr env >> andThen value:
    maybeIndented (kind Token.As) >> andThen _:
    maybeIndented (Parser.zeroOrMore (maybeIndented patternAndAccept)) >> andThen patterns:
    here >> andThen end:
    { isCompact = False
    , value = value
    , patterns = patterns
    }
        >> FA.Try (pos env start end)
        >> Parser.accept



#
# Statements
#


definition as Env: Parser FA.Statement =
    env:
    here >> andThen start:
    pattern env >> andThen p:
    Parser.maybe (maybeIndented (nonFunction env)) >> andThen nf:
    maybeIndented defop >> andThen None:
    afterEqualOrColon env >> andThen body:

#    end =
#        body
#            >> List.reverse
#            >> List.head
#            >> Maybe.map getpos
#            >> Maybe.withDefault Pos.T
#            >> Pos.end

    here >> andThen end:
    { pattern = p
    , body = body
    , nonFn = Maybe.withDefault [] nf
    }
        >> FA.Definition (pos env start end)
        >> Parser.accept


#inlineStatementOrBlock as Env: Parser [FA.Statement] =
#    env:
#    Parser.oneOf
#        [ Parser.breakCircularDefinition (_: expr env) >> andThen e: Parser.accept [FA.Evaluation (FA.expressionPos e) e]
#        , block (oomSeparatedBy (kind Token.NewSiblingLine) (statement env))
#        ]


#
# Types
#


nonFunction as Env: Parser [Text] =
    env:
    kind Token.With >> andThen _:
    rawList (lowerNameBare env) >> andThen nf:
    upperNameBare env >> andThen (At _ n):
    if n == "NonFunction" then
        Parser.accept << List.map Pos.drop nf
    else
        Parser.abort << "Only NonFunction is supported for now"


typeTerm as Env: Parser FA.Type =
    env:

    oneToken >> andThen (Token start end k):
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

    mutable =
        kind Token.Mutop >> andThen (Token start end k):
        nest >> andThen n:
        Parser.accept << FA.TypeMutable (pos env start end) n

    Parser.expression
        (typeTerm env)
        [ higherOr << typeParens nest
        , higherOr << typeList env nest
        , higherOr << record env Token.As FA.TypeRecord nest
        , higherOr << mutable
        , typeConstructorAppOr env
        , typeTupleOr env
        , typeFunctionOr env
        ]


typeTupleOr as Env: Parser FA.Type: Parser FA.Type =
    env: higher:

    binopAndPrev as Parser FA.Type =
        discardFirst (binaryOperators Op.Tuple) higher

    here >> andThen start:
    higher >> andThen head:
    Parser.zeroOrMore binopAndPrev >> andThen tail:
    here >> andThen end:
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
    here >> andThen start:
    surroundStrict (Token.SquareBracket Token.Open) (Token.SquareBracket Token.Closed) main >> andThen t:
    here >> andThen end:
    Parser.accept << FA.TypeList (pos env start end) t


typeFunctionOr as Env: Parser FA.Type: Parser FA.Type =
    env: higher:

    arrowAndHigher as Parser ( LambdaModifier & Pos & FA.Type ) =
        colon env >> andThen ( consuming & p ):
        higher >> andThen h:
        Parser.accept ( consuming & p & h )

    fold as ( LambdaModifier & Pos & FA.Type ): ( LambdaModifier & FA.Type ): ( LambdaModifier & FA.Type ) =

        ( nextModifier & p & ty ):
        ( thisModifier & accum ):

        nextModifier & FA.TypeFunction p ty thisModifier accum

    here >> andThen fs:
    higher >> andThen e:
    here >> andThen fe:
    Parser.zeroOrMore arrowAndHigher >> andThen es:

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

    ( ( lambdaModifier & p & return ) & reversedArgs ) =
        reverseRec ( LambdaNormal & firstPos & e ) es []

    lambdaModifier & return
        >> List.for reversedArgs fold
        >> x: x.second
        >> Parser.accept


colon as Env: Parser ( LambdaModifier & Pos ) =
    env:

    oneToken >> andThen (Token start end k):
    try k as
        Token.Colon lambdaModifier:
            Parser.accept ( lambdaModifier & pos env start end )

        _:
            Parser.reject


typeConstructorAppOr as Env: Parser FA.Type: Parser FA.Type =
    env: higher:

    higher >> andThen ty:
    try ty as
        FA.TypeConstant p1 maybeModule name []:
            (Parser.zeroOrMore higher) >> andThen args:
            here >> andThen end2:
            if args == [] then
                Parser.accept ty

            else
                Parser.accept << FA.TypeConstant p1 maybeModule name args

        _:
            Parser.accept ty



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
        , higherOr << record env Token.Defop FA.PatternRecord nest
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
        , record env Token.Defop FA.PatternRecord nest
        ]


patternApplication as Env: Parser FA.Pattern: Parser FA.Pattern =
    env: param:

    oneToken >> andThen (Token start end k):

    p = pos env start end

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
                Parser.maybe (maybeIndented << typeAnnotation env) >> andThen maybeTy:
                Parser.accept << FA.PatternAny p mutable name maybeTy

            try modifier as
                Token.NameNoModifier: thingy False
#                Token.NameMutable: thingy True
                Token.NameStartsWithDot: Parser.reject

        Token.UpperName maybeModule name:
            Parser.zeroOrMore param >> andThen params:
            here >> andThen end1:
            Parser.accept << FA.PatternConstructor (pos env start end1) maybeModule name params

        _:
            Parser.reject


typeAnnotation as Env: Parser FA.Type =
    env:

    discardFirst
        (kind Token.As)
        (maybeIndented (typeExpr env))


patternBinopOr as Env: Op.Precedence: (Pos: [FA.Pattern]: FA.Pattern): Parser FA.Pattern: Parser FA.Pattern =
    env: precedenceGroup: constructor: higher:

    here >> andThen start:
    sepList (binaryOperators precedenceGroup) higher >> andThen ( head & sepTail ):
    here >> andThen end:
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
    higher >> andThen h:

    r =
        h :: accum

    maybeWithDefault r << maybeIndented (recInlineOrIndentedOrBelow higher r)


functionApplicationOr as Env: Parser FA.Expression: Parser FA.Expression =
    env: higher:

    recInlineOrIndented as [FA.Expression]: Parser [FA.Expression] =
        accum:

        higher >> andThen h:

        r =
            h :: accum

        Parser.oneOf
            # after at least one indented block, allow arguments to appear also as siblings (ie, right below)
            [ maybeIndented (recInlineOrIndentedOrBelow higher r)
            , recInlineOrIndented r
            , Parser.accept r
            ]

    here >> andThen start:
    recInlineOrIndented [] >> andThen reversedArgs:
    here >> andThen end:
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

    Parser.maybe unaryOperator >> andThen maybeUnary:
    higher >> andThen right:
    here >> andThen end:
    try maybeUnary as
        Just ( op & Token start _ _ ):
            Parser.accept << FA.Unop (pos env start end) op right

        Nothing:
            Parser.accept right


unaryOperator as Parser ( Op.Unop & Token ) =

    oneToken >> andThen token:
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

    oneToken >> andThen (Token start end k):
    try k as
        Token.Binop binop:
            Parser.accept << FA.PrefixBinop (pos env start end) binop.symbol

        _:
            Parser.reject


binopsOr as Env: Op.Precedence: Parser FA.Expression: Parser FA.Expression =
    env: group: higher:
    here >> andThen start:
    sepList (binaryOperators group) higher >> andThen ( head & sepTail ):
    here >> andThen end:
    if sepTail == [] then
        Parser.accept head

    else
        FA.Binop (pos env start end) group ( head & sepTail )
            >> Parser.accept


binaryOperators as Op.Precedence: Parser Op.Binop =
    group:
    oneToken >> andThen (Token s e k):
    try k as
        Token.Binop op:
            if op.precedence == group then
                Parser.accept op

            else
                Parser.reject

        _:
            Parser.reject

