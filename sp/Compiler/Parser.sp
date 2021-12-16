

alias Parser a =
    Parser.Parser Token a


alias Env =
    { moduleName as Text
    , stripLocations as Bool
    }


#
# Helpers
#
then =
    Parser.then


here =
    as Parser Int

    Parser.here >> then fn tokens:
    Parser.accept
        (try tokens as
            Token mod start end :: rest:
                start

            []:
                0
        )


pos env start end =
    as Env: Int: Int: Pos

    if env.stripLocations:
        Pos.T
    else
        Pos.P env.moduleName start end


makeError moduleName readState message =
    as Text: [Token]: Text: Res a

    p =
        try readState as
            []: Pos.P moduleName 0 1
            Token start end k :: rest: Pos.P moduleName start end

    Error.res p (fn eenv: [ message ])


# This is just a pr attempt at getting some sort of parser debugging
palog m =
    as Text: Parser Text
    Parser.accept None >> then fn _:
    Parser.accept (Debug.log "->" m)


#
# Main
#
parse stripLocations moduleName tokens =
    as Bool: Text: [Token]: Res [FA.Statement]

    parser =
        module_ { moduleName, stripLocations }

    runParser moduleName parser tokens


runParser moduleName parser tokens =
    as Text: Parser output: [Token]: Res output

    failureStates & outcome =
        as [[Token]] & Parser.Outcome Token output
        tokens
            >> List.filter (fn (Token s e k): k /= Token.Comment)
            >> Parser.runParser parser

    try outcome as
        Parser.Accepted readState output:
            Ok output

        Parser.Aborted readState message:
            makeError moduleName readState message

        Parser.Rejected:
            findMin readState best =
                if List.length readState < List.length best: readState else: best

            readState =
                as [Token]
                List.foldl findMin failureStates tokens

            message =
                try readState as
                    []: "I got to the end of file and I can't make sense of it. =("
                    _: "I got stuck parsing here. =("

            makeError moduleName readState message


module_ env =
    as Env: Parser [FA.Statement]

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
        Parser.zeroOrMore e >> then fn _:
        Parser.end

    statements =
        oomSeparatedBy (kind Token.NewSiblingLine) (statement env)

    Parser.oneOf
        [ Parser.map (fn _: []) Parser.end
        , Parser.surroundWith start zzz statements
        ]


#
# Terms
#
oneToken =
    as Parser Token
    Parser.consumeOne


kind targetKind =
    as Token.Kind: Parser Token
    oneToken >> then fn token:
    (Token _ _ k) = token
    if targetKind == k:
        Parser.accept token

    else
        Parser.reject


upperNameBare env =
    as Env: Parser (At Text)
    oneToken >> then fn token:
    try token as
        Token start end (Token.UpperName Nothing name):
            Parser.accept << At (pos env start end) name
        _:
            Parser.reject


lowerNameBare env =
    as Env: Parser (At Text)
    oneToken >> then fn token:
    try token as
        Token start end (Token.LowerName Token.NameNoModifier Nothing name []):
            Parser.accept << At (pos env start end) name
        _:
            Parser.reject


defop =
    as Parser { mutable as Bool }

    oneToken >> then fn token:
    try token as
        Token _ _ (Token.Defop arg):
            Parser.accept arg

        _:
            Parser.reject



#
# Combinators
#
discardFirst a b =
    as Parser a: Parser b: Parser b
    a >> then fn _: b


discardSecond a b =
    as Parser a: Parser b: Parser a
    a >> then fn aa:
    b >> then fn _:
    Parser.accept aa


inlineOrIndented p =
    as Parser a: Parser a
    Parser.oneOf
        [ block p
        , p
        ]


inlineOrBelowOrIndented p =
    as Parser a: Parser a
    Parser.oneOf
        [ block p
        , sib p
        , p
        ]


maybeWithDefault a p =
    as a: Parser a: Parser a
    Parser.oneOf [ p, Parser.accept a ]


surroundStrict left right =
    as Token.Kind: Token.Kind: Parser a: Parser a
    Parser.surroundWith (kind left) (kind right)


surroundMultiline left right content =
    as Token.Kind: Token.Kind: Parser a: Parser a
    discardFirst
        (kind left)
        (inlineOrBelowOrIndented
            (discardSecond
                content
                (inlineOrBelowOrIndented (kind right))
            )
        )


oomSeparatedBy sep pa =
    as Parser a: Parser b: Parser [b]

    pa >> then fn head:
    Parser.zeroOrMore (discardFirst sep pa) >> then fn tail:
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
block =
    as Parser a: Parser a
    surroundStrict Token.BlockStart Token.BlockEnd


sib =
    as Parser a: Parser a
    discardFirst (kind Token.NewSiblingLine)


sepListAtSep sep item =
    as Parser sep: Parser item: Parser [sep & item]
    sep >> then fn sep0:
    (Parser.oneOf
        [ block (sepListAtItem sep item)
        , sib (sepListAtItem sep item)
        , sepListAtItem sep item
        ]
    ) >> then fn ( item0 & tail ):
    Parser.accept << sep0 & item0 :: tail



sepListAtItem sep item =
    as Parser sep: Parser item: Parser (FA.SepList sep item)
    item >> then fn item0:
    (Parser.oneOf
        [ block (sepListAtSep sep item)
        , sib (sepListAtSep sep item)
        , sepListAtSep sep item
        , Parser.accept []
        ]
    ) >> then fn sepsAndItems:
    Parser.accept ( item0 & sepsAndItems )


sepList =
    as Parser sep: Parser item: Parser (FA.SepList sep item)
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
rawList item =
    as Parser a: Parser [a]

    sibsep =
        # TODO was:
        #             (Parser.maybe << kind Token.NewSiblingLine) >> then fn _:
        #             kind Token.Comma
        # but I didn't test it properly
        inlineOrBelowOrIndented << kind Token.Comma

    discardFirst (Parser.maybe sibsep) (oomSeparatedBy sibsep item)



#
# Statements
#
errorCantUseMutableAssignmentHere =
    as Text
    "Can't use mutable assignment here"


typeAlias env =
    as Env: Parser FA.Statement

    kind (Token.LowerName Token.NameNoModifier Nothing "alias" []) >> then fn _:
    upperNameBare env >> then fn name:
    Parser.zeroOrMore (lowerNameBare env) >> then fn args:
    defop >> then fn { mutable }:
    (inlineOrBelowOrIndented (typeExpr env)) >> then fn ty:
    if mutable:
        Parser.abort errorCantUseMutableAssignmentHere

    else
        { name = name
        , args = args
        , ty = ty
        }
            # TODO use ty end instead
            >> FA.TypeAlias
            >> Parser.accept


unionDef env =
    as Env: Parser FA.Statement

    kind (Token.LowerName Token.NameNoModifier Nothing "union" []) >> then fn _:
    upperNameBare env >> then fn (At p name):
    Parser.zeroOrMore (lowerNameBare env) >> then fn args:
    defop >> then fn { mutable }:
    inlineOrBelowOrIndented (rawList (unionConstructor env)) >> then fn cons:
    if mutable:
        Parser.abort errorCantUseMutableAssignmentHere

    else
        { name = name
        , args = List.map Pos.drop args
        , constructors = cons
        }
            >> FA.UnionDef p
            >> Parser.accept


unionConstructor env =
    as Env: Parser FA.Constructor

    typeExpr env >> then fn type:
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
#    oneToken >> then fn (Token start end k):
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
#                    Parser.maybe lambdaColon >> then fn maybeColon:
#                        try maybeColon as
#                            Nothing:
#                                Parser.accept << FA.Variable p maybeModule name attrs
#
#                            Just mutable:
#                                lambdaBody env >> then fn b:
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



exprWithLeftDelimiter env =
    as Env: Parser FA.Expression

    colon =
        Parser.oneOf
            [ kind Token.Colon >> Parser.map fn _: False
            , kind Token.MutableColon >> Parser.map fn _: True
            ]

    maybeColon =
        Parser.maybe colon

    oneToken >> then fn (Token start end k):

        p =
            pos env start end

        try k as
            Token.NumberLiteral s:
                maybeColon >> then fn mc:
                    try mc as
                        Nothing: Parser.accept << FA.LiteralNumber p s
                        Just mutable: lambdaParser env mutable (FA.PatternLiteralNumber p s)


            Token.TextLiteral s:
                maybeColon >> then fn mc:
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
                        maybeColon >> then fn mc:
                            try mc as
                                Nothing: Parser.accept << FA.Variable p maybeModule name attrs
                                # TODO also test that maybeModule == Nothing and attrs == []
                                Just mutable: lambdaParser env mutable (FA.PatternAny p False name Nothing)

            Token.UpperName maybeModule name:
                maybeColon >> then fn mc:
                    try mc as
                        Nothing: Parser.accept << FA.Constructor p maybeModule name
                        Just mutable: lambdaParser env mutable (FA.PatternConstructor p maybeModule name [])

            Token.RoundParen Token.Open:
                paParser =
                    pattern env >> then fn pa:
                    kind (Token.RoundParen Token.Closed) >> then fn _:
                    colon >> then fn mutable:
                    lambdaParser env mutable pa

                exprParser =
                    discardSecond
                        (expr env)
                        (kind (Token.RoundParen Token.Closed))

                inlineOrBelowOrIndented << Parser.oneOf [ paParser, exprParser ]

#            Token.SquareBracket Token.Open:
#                paParser =
#                    rawList (pattern env) >> then fn pas:
#                    kind (Token.SquareBracket Token.Closed) >> then fn _:
#                    colon >> then fn mutable:
#                    lambdaParser env mutable (FA.PatternList p pas)
#
#                exprParser =
#                    rawList (expr env) >> then fn exps:
#                    kind (Token.SquareBracket Token.Closed) >> then fn _:
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


lambdaParser env mutable pa =
    as Env: Bool: FA.Pattern: Parser FA.Expression

    lambdaBody env >> then fn body:
    Parser.accept << FA.Lambda (FA.patternPos pa) pa mutable body






#
# Expr (with precedence rules)
#


expr env =
    as Env: Parser FA.Expression

    higherOr =
        Parser.higherOr

    nest =
        Parser.breakCircularDefinition fn _: expr env

    Parser.expression
        (exprWithLeftDelimiter env)
        # the `Or` stands for `Or higher priority parser`
        [
#        , higherOr << parens (Parser.oneOf [ binopInsideParens env, nest ])
        , higherOr << list env FA.List nest
        , higherOr << record env (Token.Defop { mutable = False }) FA.Record nest
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


parens =
    as Parser a: Parser a
    Parser.surroundWith
        (kind << Token.RoundParen Token.Open)
        (inlineOrBelowOrIndented << kind << Token.RoundParen Token.Closed)



#
# List
#


list env constructor main =
    as Env: (Pos: List a: a): Parser a: Parser a
    here >> then fn start:
    surroundMultiline (Token.SquareBracket Token.Open) (Token.SquareBracket Token.Closed) (Parser.maybe (rawList main)) >> then fn maybeLs:
    here >> then fn end:
    (try maybeLs as
        Just ls:
            ls

        Nothing:
            []
    )
        >> constructor (pos env start end)
        >> Parser.accept



#
# Record
#


record env assign constructor main =
    as Env: Token.Kind: (Pos: FA.RecordArgs a: a): Parser a: Parser a

    attrAssignment =
        discardFirst (kind assign) (inlineOrBelowOrIndented main)

    attr =
        lowerNameBare env >> then fn name:
        Parser.maybe attrAssignment >> then fn maybeAssignment:
        Parser.accept ( name & maybeAssignment )

    updateTarget =
        main >> then fn h:
        kind Token.With >> then fn _:
        Parser.accept h

    content start =
        Parser.maybe updateTarget >> then fn maybeUpdateTarget:
        (inlineOrBelowOrIndented << rawList attr) >> then fn attrs:
        here >> then fn end:
        { extends = maybeUpdateTarget
        , attrs = attrs
        }
            >> constructor (pos env start end)
            >> Parser.accept

    here >> then fn s:
    surroundMultiline (Token.CurlyBrace Token.Open) (Token.CurlyBrace Token.Closed) (Parser.maybe << content s) >> then fn maybeRecord:
    here >> then fn e:
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


if_ env =
    as Env: Parser FA.Expression

    maybeNewLine k =
        discardFirst
            (Parser.maybe (kind Token.NewSiblingLine))
            (kind k)

    kind Token.If >> then fn (Token start _ _):
    expr env >> then fn condition:
    Parser.maybe (maybeNewLine Token.Then) >> then fn maybeThen:
    if maybeThen == Nothing:
        Parser.abort "`if` should be followed by a `then` but I can't find it"

    else
        inlineStatementOrBlock env >> then fn true:
        maybeNewLine Token.Else >> then fn _:
        Parser.maybe (kind Token.Colon) >> then fn _:
        inlineStatementOrBlock env >> then fn false:
        here >> then fn end:
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


try_ env =
    as Env: Parser FA.Expression

    maybeNewLine =
        as Parser a: Parser a
        discardFirst (Parser.maybe (kind Token.NewSiblingLine))

    maybeNewLineKind k =
        as Token.Kind: Parser Token
        maybeNewLine (kind k)

    patternAndAccept =
        pattern env >> then fn p:
        maybeNewLineKind Token.Colon >> then fn _:
        inlineStatementOrBlock env >> then fn accept:
        Parser.accept ( p & accept )

    kind Token.Try >> then fn (Token start _ _):
    expr env >> then fn value:
    maybeNewLineKind Token.As >> then fn _:
    block (Parser.zeroOrMore (maybeNewLine patternAndAccept)) >> then fn patterns:
    here >> then fn end:
    { isCompact = False
    , value = value
    , patterns = patterns
    }
        >> FA.Try (pos env start end)
        >> Parser.accept



#
# Statements
#


statement env =
    as Env: Parser FA.Statement
    Parser.breakCircularDefinition fn _:
    # This is here because inline comments might be followed by NewSiblingLine
    # and I am not sure it's a responsibility of the lexer to deal with it.
    Parser.maybe (kind Token.NewSiblingLine) >> then fn _:
    Parser.oneOf
        [ typeAlias env
        , unionDef env
        , definition env
        , expr env >> then fn e:
          e >> FA.Evaluation (FA.expressionPos e) >> Parser.accept
        ]


definition env =
    as Env: Parser FA.Statement
    here >> then fn start:
    pattern env >> then fn p:
    Parser.maybe (inlineOrBelowOrIndented (nonFunction env)) >> then fn nf:
    inlineOrBelowOrIndented defop >> then fn { mutable }:
    inlineStatementOrBlock env >> then fn body:

#    end =
#        body
#            >> List.reverse
#            >> List.head
#            >> Maybe.map getpos
#            >> Maybe.withDefault Pos.T
#            >> Pos.end

    here >> then fn end:
    { pattern = p
    , mutable = mutable
    , body = body
    , nonFn = Maybe.withDefault [] nf
    }
        >> FA.Definition (pos env start end)
        >> Parser.accept


inlineStatementOrBlock env =
    as Env: Parser [FA.Statement]
    Parser.oneOf
        [ Parser.breakCircularDefinition (fn _: expr env) >> then fn e: Parser.accept [FA.Evaluation (FA.expressionPos e) e]
        , block (oomSeparatedBy (kind Token.NewSiblingLine) (statement env))
        ]


#
# Types
#


nonFunction env =
    as Env: Parser [Text]
    kind Token.With >> then fn _:
    rawList (lowerNameBare env) >> then fn nf:
    upperNameBare env >> then fn (At _ n):
    if n == "NonFunction":
        Parser.accept << List.map Pos.drop nf
    else
        Parser.abort << "Only NonFunction is supported for now"


typeTerm env =
    as Env: Parser FA.Type

    oneToken >> then fn (Token start end k):
    try k as
        Token.UpperName maybeModule name:
            Parser.accept << FA.TypeConstant (pos env start end) maybeModule name []

        Token.LowerName Token.NameNoModifier Nothing name []:
            Parser.accept << FA.TypeVariable (pos env start end) name

        _:
            Parser.reject


typeExpr env =
    as Env: Parser FA.Type

    nest =
        Parser.breakCircularDefinition fn _: typeExpr env

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


typeTupleOr env higher =
    as Env: Parser FA.Type: Parser FA.Type

    binopAndPrev =
        as Parser FA.Type
        discardFirst (binaryOperators Op.Tuple) higher

    here >> then fn start:
    higher >> then fn head:
    Parser.zeroOrMore binopAndPrev >> then fn tail:
    here >> then fn end:
    if tail == []:
        Parser.accept head

    else
        (head :: tail)
            >> FA.TypeTuple (pos env start end)
            >> Parser.accept


typeParens main =
    as Parser FA.Type: Parser FA.Type
    surroundStrict
        (Token.RoundParen Token.Open)
        (Token.RoundParen Token.Closed)
        main


typeList env main =
    as Env: Parser FA.Type: Parser FA.Type
    here >> then fn start:
    surroundStrict (Token.SquareBracket Token.Open) (Token.SquareBracket Token.Closed) main >> then fn t:
    here >> then fn end:
    Parser.accept << FA.TypeList (pos env start end) t


typeFunctionOr env higher =
    as Env: Parser FA.Type: Parser FA.Type

    arrowAndHigher =
        as Parser ( Bool & Pos & FA.Type )
        arrow env >> then fn ( mutable & p ):
        higher >> then fn h:
        Parser.accept ( mutable & p & h )

    fold ( nextIsMutable & p & ty ) ( thisIsMutable & accum ) =
        as ( Bool & Pos & FA.Type ): ( Bool & FA.Type ): ( Bool & FA.Type )
        ( nextIsMutable
         & FA.TypeFunction p ty thisIsMutable accum
        )

    here >> then fn fs:
    higher >> then fn e:
    here >> then fn fe:
    Parser.zeroOrMore arrowAndHigher >> then fn es:

    firstPos =
        pos env fs fe

    # This used to be OneOrMore.reverse, maybe there is a better way to rewrite this?
    reverseRec a ls accum =
        as a: [a]: [a]: a & [a]
        try ls as
            []:
                a & accum

            head :: tail:
                reverseRec head tail (a :: accum)

    ( ( thisIsMutable & p & return ) & reversedArgs ) =
        reverseRec ( False & firstPos & e ) es []

    thisIsMutable & return
        >> List.foldl fold reversedArgs
        >> fn x: x.second
        >> Parser.accept


arrow env =
    as Env: Parser ( Bool & Pos )

    oneToken >> then fn (Token start end k):
    try k as
        Token.Colon:
            Parser.accept ( False & (pos env start end) )

        Token.MutableColon:
            Parser.accept ( True & (pos env start end) )

        _:
            Parser.reject


typeConstructorAppOr env higher =
    as Env: Parser FA.Type: Parser FA.Type

    higher >> then fn ty:
    try ty as
        FA.TypeConstant p1 maybeModule name []:
            (Parser.zeroOrMore higher) >> then fn args:
            here >> then fn end2:
            if args == []:
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
#        [ kind Token.Colon >> then fn _: Parser.accept False
#        , kind Token.MutableColon >> then fn _: Parser.accept True
#        ]




lambdaBody env =
    as Env: Parser [FA.Statement]
    Parser.oneOf
        [ [#
             x:
             a
             b
             c
          #]
        , Parser.oneOrMore (sib (statement env)) >> then fn (h & t):
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
#    pattern env >> then fn param:
#    lambdaColon >> then fn mutable:
#    lambdaBody env >> then fn b:
#    Parser.accept << FA.Lambda (FA.patternPos param) param mutable b


#
# Pattern
#
pattern env =
    as Env: Parser FA.Pattern

    nest =
        Parser.breakCircularDefinition fn _: pattern env

    higherOr =
        Parser.higherOr

    Parser.expression
        (patternApplication env << functionParameter env nest)
        # the `Or` stands for `Or higher priority parser`
        [ higherOr << parens nest
        , higherOr << list env FA.PatternList nest
        , higherOr << record env (Token.Defop { mutable = False }) FA.PatternRecord nest
        , patternBinopOr env Op.Cons FA.PatternListCons
        , patternBinopOr env Op.Tuple FA.PatternTuple
        ]


# TODO maybe the whole pattern -> functionParameter -> patternApplication mess can be cleaned up?
functionParameter env nest =
    as Env: Parser FA.Pattern: Parser FA.Pattern
    Parser.oneOf
        [ patternApplication env Parser.reject
        , parens nest
        , list env FA.PatternList nest
        , record env (Token.Defop { mutable = False }) FA.PatternRecord nest
        ]


patternApplication env param =
    as Env: Parser FA.Pattern: Parser FA.Pattern

    oneToken >> then fn (Token start end k):

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
            thingy mutable =
                Parser.maybe (inlineOrBelowOrIndented << typeAnnotation env) >> then fn maybeTy:
                Parser.accept << FA.PatternAny p mutable name maybeTy

            try modifier as
                Token.NameNoModifier: thingy False
                Token.NameMutable: thingy True
                Token.NameStartsWithDot: Parser.reject

        Token.UpperName maybeModule name:
            Parser.zeroOrMore param >> then fn params:
            here >> then fn end1:
            Parser.accept << FA.PatternConstructor (pos env start end1) maybeModule name params

        _:
            Parser.reject


typeAnnotation env =
    as Env: Parser FA.Type

    discardFirst
        (kind Token.As)
        (inlineOrBelowOrIndented (typeExpr env))


patternBinopOr env precedenceGroup constructor higher =
    as Env: Op.Precedence: (Pos: [FA.Pattern]: FA.Pattern): Parser FA.Pattern: Parser FA.Pattern

    here >> then fn start:
    sepList (binaryOperators precedenceGroup) higher >> then fn ( head & sepTail ):
    here >> then fn end:
    if sepTail == []:
        Parser.accept head

    else
        (head :: List.map (fn x: x.second) sepTail)
            >> constructor (pos env start end)
            >> Parser.accept


#
# Function application
#

recInlineOrIndentedOrBelow higher accum =
    as Parser FA.Expression: [FA.Expression]: Parser [FA.Expression]
    higher >> then fn h:

    r =
        h :: accum

    maybeWithDefault r << inlineOrBelowOrIndented (recInlineOrIndentedOrBelow higher r)


functionApplicationOr env higher =
    as Env: Parser FA.Expression: Parser FA.Expression

    recInlineOrIndented accum =
        as [FA.Expression]: Parser [FA.Expression]
        higher >> then fn h:

        r =
            h :: accum

        Parser.oneOf
            # after at least one indented block, allow arguments to appear also as siblings (ie, right below)
            [ block (recInlineOrIndentedOrBelow higher r)
            , recInlineOrIndented r
            , Parser.accept r
            ]

    here >> then fn start:
    recInlineOrIndented [] >> then fn reversedArgs:
    here >> then fn end:
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


unopsOr env higher =
    as Env: Parser FA.Expression: Parser FA.Expression

    Parser.maybe unaryOperator >> then fn maybeUnary:
    higher >> then fn right:
    here >> then fn end:
    try maybeUnary as
        Just ( op & Token start _ _ ):
            Parser.accept << FA.Unop (pos env start end) op right

        Nothing:
            Parser.accept right


unaryOperator =
    as Parser ( Op.Unop & Token )

    oneToken >> then fn token:
    try token as
        Token s e (Token.Unop op):
            Parser.accept ( op & token )

        _:
            Parser.reject



#
# Binops
#


binopInsideParens env =
    as Env: Parser FA.Expression

    oneToken >> then fn (Token start end k):
    try k as
        Token.Binop binop:
            Parser.accept << FA.PrefixBinop (pos env start end) binop.symbol

        _:
            Parser.reject


binopsOr env group higher =
    as Env: Op.Precedence: Parser FA.Expression: Parser FA.Expression
    here >> then fn start:
    sepList (binaryOperators group) higher >> then fn ( head & sepTail ):
    here >> then fn end:
    if sepTail == []:
        Parser.accept head

    else
        FA.Binop (pos env start end) group ( head & sepTail )
            >> Parser.accept


binaryOperators group =
    as Op.Precedence: Parser Op.Binop
    oneToken >> then fn (Token s e k):
    try k as
        Token.Binop op:
            if op.precedence == group:
                Parser.accept op

            else
                Parser.reject

        _:
            Parser.reject

