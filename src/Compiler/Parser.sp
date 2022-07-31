

alias Parser a =
    Parser.Parser Token a


alias Env =
    { moduleName as Text
    , stripLocations as Bool
    , parentIndent as Int
    }


et as (a: Parser b): Parser a: Parser b =
    Parser.andThen


here as Parser Int =
    Parser.here >> et tokens:
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


#
# Terms
#
oneToken as Parser Token =
    Parser.consumeOne


kind as Token.Kind: Parser Token =
    targetKind:
    oneToken >> et token:
    (Token _ _ k) = token
    if targetKind == k then
        Parser.accept token

    else
        Parser.reject


upperNameBare as Env: Parser (At Text) =
    env:
    oneToken >> et token:
    try token as
        Token start end (Token.UpperName Nothing name):
            Parser.accept << At (pos env start end) name
        _:
            Parser.reject


lowerNameBare as Env: Parser (At Text) =
    env:
    oneToken >> et token:
    try token as
        Token start end (Token.LowerName Token.NameNoModifier Nothing name []):
            Parser.accept << At (pos env start end) name
        _:
            Parser.reject


defop as Parser None =

    oneToken >> et token:
    try token as
        Token _ _ Token.Defop:
            Parser.accept None

        _:
            Parser.reject


ind as Parser Int =

    oneToken >> et token:
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
    a >> et _: b


discardSecond as Parser a: Parser b: Parser a =
    a: b:
    a >> et aa:
    b >> et _:
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
    pa >> et head:
    Parser.zeroOrMore (discardFirst sep pa) >> et tail:
    Parser.accept <<  head :: tail



sepListAtSep as Parser sep: Parser item: Parser [sep & item] =
    sep: item:
    sep >> et sep0:

    maybeIndented (sepListAtItem sep item) >> et ( item0 & tail ):
    Parser.accept << sep0 & item0 :: tail



sepListAtItem as Parser sep: Parser item: Parser (FA.SepList sep item) =
    sep: item:
    item >> et item0:
    theParserStillSucks =
        Parser.oneOf
            [ maybeIndented (sepListAtSep sep item)
            , Parser.accept []
            ]
    theParserStillSucks >> et sepsAndItems:
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
        #             (Parser.maybe << kind Token.NewSiblingLine) >> et _:
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
    >> et indent:

    if indent > env.parentIndent then
        Parser.accept indent
    else
        Parser.reject


evaluation as Env: Parser FA.Statement =
    env:

    expr env
    >> Parser.map e: FA.Evaluation (FA.expressionPos e) e



typeAlias as Env: Parser FA.Statement =
    env:

    kind (Token.LowerName Token.NameNoModifier Nothing "alias" []) >> et _:
    upperNameBare env >> et name:
    Parser.zeroOrMore (lowerNameBare env) >> et args:
    defop >> et None:
    maybeIndented (typeExpr env) >> et ty:
        { name = name
        , args = args
        , ty = ty
        }
            # TODO use ty end instead
            >> FA.TypeAlias
            >> Parser.accept


unionDef as Env: Parser FA.Statement =
    env:

    kind (Token.LowerName Token.NameNoModifier Nothing "union" []) >> et _:
    upperNameBare env >> et (At p name):
    Parser.zeroOrMore (lowerNameBare env) >> et args:
    defop >> et None:
    maybeIndented (rawList (unionConstructor env)) >> et cons:
        { name = name
        , args = List.map Pos.drop args
        , constructors = cons
        }
            >> FA.UnionDef p
            >> Parser.accept


unionConstructor as Env: Parser FA.Constructor =
    env:

    typeExpr env >> et type:
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
#    oneToken >> et (Token start end k):
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
#                    Parser.maybe lambdaColon >> et maybeColon:
#                        try maybeColon as
#                            Nothing:
#                                Parser.accept << FA.Variable p maybeModule name attrs
#
#                            Just mutable:
#                                lambdaBody env >> et b:
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

    oneToken >> et (Token start end k):

        p =
            pos env start end

        try k as
            Token.NumberLiteral s:
                maybeColon >> et mc:
                    try mc as
                        Nothing: Parser.accept << FA.LiteralNumber p s
                        Just modifier: lambdaParser env modifier (FA.PatternLiteralNumber p s)

            Token.TextLiteral s:
                maybeColon >> et mc:
                    try mc as
                        Nothing: Parser.accept << FA.LiteralText p s
                        Just modifier: lambdaParser env modifier (FA.PatternLiteralText p s)

            Token.LowerName modifier maybeModule name attrs:
                log "-------" { modifier, name }
                try modifier as
#                    Token.NameMutable:
#                        maybeColon
#                        >> et mc:
#                            try mc as
#                                Nothing: Parser.accept << FA.Mutable p name attrs
#                                # TODO also test that maybeModule == Nothing and attrs == []
#                                Just modifier: lambdaParser env modifier (FA.PatternAny p True name Nothing)

                    Token.NameStartsWithDot:
                        Parser.accept << FA.RecordShorthand p (name :: attrs)

                    Token.NameNoModifier:
                        maybeColon >> et mc:
                            log "--->" mc
                            try mc as
                                Nothing:
                                    Parser.accept << FA.Variable p maybeModule name attrs

                                # TODO also test that maybeModule == Nothing and attrs == []
                                Just modifier:
                                    lambdaParser env modifier (FA.PatternAny p False name Nothing)

            Token.UpperName maybeModule name:
                maybeColon >> et mc:
                    try mc as
                        Nothing: Parser.accept << FA.Constructor p maybeModule name
                        Just modifier: lambdaParser env modifier (FA.PatternConstructor p maybeModule name [])

            Token.RoundParen Token.Open:
                paParser as Parser FA.Expression =
                    pattern env >> et pa:
                    kind (Token.RoundParen Token.Closed) >> et _:
                    colon env >> et (lambdaModifier & pos):
                    lambdaParser env lambdaModifier pa

                exprParser as Parser FA.Expression =
                    discardSecond
                        (expr env)
                        (kind (Token.RoundParen Token.Closed))

                maybeIndented << Parser.oneOf [ paParser, exprParser ]

#            Token.SquareBracket Token.Open:
#                paParser =
#                    rawList (pattern env) >> et pas:
#                    kind (Token.SquareBracket Token.Closed) >> et _:
#                    colon >> et mutable:
#                    lambdaParser env mutable (FA.PatternList p pas)
#
#                exprParser =
#                    rawList (expr env) >> et exps:
#                    kind (Token.SquareBracket Token.Closed) >> et _:
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

    afterEqualOrColon env >> et body:
    Parser.accept << FA.Lambda (FA.patternPos pa) pa modifier body



#
# Expr (with precedence rules)
#


expr as Env: Parser FA.Expression =
    env:

    log "expr" env.parentIndent

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

    kind Token.Mutop >> et (Token start end k):
    higher >> et n:
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
    here >> et start:
    surroundMultiline (Token.SquareBracket Token.Open) (Token.SquareBracket Token.Closed) (Parser.maybe (rawList main)) >> et maybeLs:
    here >> et end:
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
        lowerNameBare env >> et name:
        Parser.maybe attrAssignment >> et maybeAssignment:
        Parser.accept ( name & maybeAssignment )

    updateTarget =
        main >> et h:
        kind Token.With >> et _:
        Parser.accept h

    content =
        start:
        Parser.maybe updateTarget >> et maybeUpdateTarget:
        (maybeIndented << rawList attr) >> et attrs:
        here >> et end:
        { extends = maybeUpdateTarget
        , attrs = attrs
        }
            >> constructor (pos env start end)
            >> Parser.accept

    here >> et s:
    surroundMultiline (Token.CurlyBrace Token.Open) (Token.CurlyBrace Token.Closed) (Parser.maybe << content s) >> et maybeRecord:
    here >> et e:
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

    kind Token.If >> et (Token start _ _):
    expr env >> et condition:
    Parser.maybe (maybeIndented (kind Token.Then)) >> et maybeThen:
    if maybeThen == Nothing then
        Parser.abort "`if` should be followed by a `then` but I can't find it"

    else
        afterEqualOrColon env >> et true:
        maybeIndented (kind Token.Else) >> et _:
        afterEqualOrColon env >> et false:
        here >> et end:
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

    patternAndBlock as Parser (FA.Pattern & [FA.Statement]) =
        pattern env >> et p:
        colon env >> et _:
        afterEqualOrColon env >> et block:
        Parser.accept ( p & block )

    kind Token.Try >> et (Token start _ _):
    expr env >> et value:
    maybeIndented (kind Token.As) >> et _:
    maybeIndented (Parser.zeroOrMore (maybeIndented patternAndBlock)) >> et patterns:
    here >> et end:
    { isCompact = False
    , value = value
    , patterns = patterns
    }
        >> FA.Try (pos env start end)
        >> Parser.accept



#
# Statements
#


#inlineStatementOrBlock as Env: Parser [FA.Statement] =
#    env:
#    Parser.oneOf
#        [ Parser.breakCircularDefinition (_: expr env) >> et e: Parser.accept [FA.Evaluation (FA.expressionPos e) e]
#        , block (oomSeparatedBy (kind Token.NewSiblingLine) (statement env))
#        ]


#
# Types
#


nonFunction as Env: Parser [Text] =
    env:
    kind Token.With >> et _:
    rawList (lowerNameBare env) >> et nf:
    upperNameBare env >> et (At _ n):
    if n == "NonFunction" then
        Parser.accept << List.map Pos.drop nf
    else
        Parser.abort << "Only NonFunction is supported for now"


typeTerm as Env: Parser FA.Type =
    env:

    oneToken >> et (Token start end k):
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
        kind Token.Mutop >> et (Token start end k):
        nest >> et n:
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

    here >> et start:
    higher >> et head:
    Parser.zeroOrMore binopAndPrev >> et tail:
    here >> et end:
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
    here >> et start:
    surroundStrict (Token.SquareBracket Token.Open) (Token.SquareBracket Token.Closed) main >> et t:
    here >> et end:
    Parser.accept << FA.TypeList (pos env start end) t


typeFunctionOr as Env: Parser FA.Type: Parser FA.Type =
    env: higher:

    arrowAndHigher as Parser ( LambdaModifier & Pos & FA.Type ) =
        colon env >> et ( consuming & p ):
        higher >> et h:
        Parser.accept ( consuming & p & h )

    fold as ( LambdaModifier & Pos & FA.Type ): ( LambdaModifier & FA.Type ): ( LambdaModifier & FA.Type ) =

        ( nextModifier & p & ty ):
        ( thisModifier & accum ):

        nextModifier & FA.TypeFunction p ty thisModifier accum

    here >> et fs:
    higher >> et e:
    here >> et fe:
    Parser.zeroOrMore arrowAndHigher >> et es:

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

    oneToken >> et (Token start end k):
    try k as
        Token.Colon lambdaModifier:
            Parser.accept ( lambdaModifier & pos env start end )

        _:
            Parser.reject


typeConstructorAppOr as Env: Parser FA.Type: Parser FA.Type =
    env: higher:

    higher >> et ty:
    try ty as
        FA.TypeConstant p1 maybeModule name []:
            (Parser.zeroOrMore higher) >> et args:
            here >> et end2:
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

    oneToken >> et (Token start end k):

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
                Parser.maybe (maybeIndented << typeAnnotation env) >> et maybeTy:
                Parser.accept << FA.PatternAny p mutable name maybeTy

            try modifier as
                Token.NameNoModifier: thingy False
#                Token.NameMutable: thingy True
                Token.NameStartsWithDot: Parser.reject

        Token.UpperName maybeModule name:
            Parser.zeroOrMore param >> et params:
            here >> et end1:
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

    here >> et start:
    sepList (binaryOperators precedenceGroup) higher >> et ( head & sepTail ):
    here >> et end:
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
    higher >> et h:

    r =
        h :: accum

    maybeWithDefault r << maybeIndented (recInlineOrIndentedOrBelow higher r)


functionApplicationOr as Env: Parser FA.Expression: Parser FA.Expression =
    env: higher:

    recInlineOrIndented as [FA.Expression]: Parser [FA.Expression] =
        accum:

        higher >> et h:

        r =
            h :: accum

        Parser.oneOf
            # after at least one indented block, allow arguments to appear also as siblings (ie, right below)
            [ maybeIndented (recInlineOrIndentedOrBelow higher r)
            , recInlineOrIndented r
            , Parser.accept r
            ]

    here >> et start:
    recInlineOrIndented [] >> et reversedArgs:
    here >> et end:
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

    Parser.maybe unaryOperator >> et maybeUnary:
    higher >> et right:
    here >> et end:
    try maybeUnary as
        Just ( op & Token start _ _ ):
            Parser.accept << FA.Unop (pos env start end) op right

        Nothing:
            Parser.accept right


unaryOperator as Parser ( Op.Unop & Token ) =

    oneToken >> et token:
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

    oneToken >> et (Token start end k):
    try k as
        Token.Binop binop:
            Parser.accept << FA.PrefixBinop (pos env start end) binop.symbol

        _:
            Parser.reject


binopsOr as Env: Op.Precedence: Parser FA.Expression: Parser FA.Expression =
    env: group: higher:
    here >> et start:
    sepList (binaryOperators group) higher >> et ( head & sepTail ):
    here >> et end:
    if sepTail == [] then
        Parser.accept head

    else
        FA.Binop (pos env start end) group ( head & sepTail )
            >> Parser.accept


binaryOperators as Op.Precedence: Parser Op.Binop =
    group:
    oneToken >> et (Token s e k):
    try k as
        Token.Binop op:
            if op.precedence == group then
                Parser.accept op

            else
                Parser.reject

        _:
            Parser.reject




#
#
#

definition as Env: Parser FA.Statement =
    env:

    here >> et start:
    pattern env >> et p:

    Parser.maybe (maybeIndented (nonFunction env)) >> et nf:

    maybeIndented defop >> et None:

    afterEqualOrColon env >> et body:

    here >> et end:

    { pattern = p
    , body = body
    , nonFn = Maybe.withDefault [] nf
    }
    >> FA.Definition (pos env start end)
    >> Parser.accept







statement as Env: Parser FA.Statement =
    oldEnv:

    indentGreaterThanParentIndent oldEnv
    >> et indent:

    newEnv =
        { oldEnv with parentIndent = indent }

#    definition newEnv
    Parser.oneOf [
#      , typeAlias newEnv
#      , unionDef newEnv
      , evaluation newEnv
      , definition newEnv
      ]





#
# Main
#

module_ as Env: Parser [FA.Statement] =
    env:

    discardSecond
        (Parser.zeroOrMore (statement env))
        Parser.end


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
            findMin =
                readState: best:
                if List.length readState < List.length best then
                    readState
                else
                    best

            readState as [Token] =
                List.for failureStates findMin tokens

            message =
                try readState as
                    []: "I got to the end of file and I can't make sense of it. =("
                    _: "I got stuck parsing here. =("

            makeError moduleName readState message


parse as Bool: Text: [Token]: Res [FA.Statement] =
    stripLocations: moduleName: tokens:

    List.each tokens t:
      log "*" t

    parser =
        module_ {
          , moduleName
          , stripLocations
          , parentIndent = -1
          }

    runParser moduleName parser tokens



alias Pars = {
    , stripLocations as Bool
    , name as Name
    }


textToFormattableModule as Pars: Text: Res [FA.Statement] =
    pars: code:

    tokensResult as Res [Token] =
        Compiler/Lexer.lexer pars.name code

    tokensToStatsResult as [Token]: Res [FA.Statement] =
        tokens:
        Debug.benchStart None
        parse pars.stripLocations pars.name tokens
            >> btw Debug.benchStop "parse"

    Result.onOk tokensToStatsResult tokensResult

