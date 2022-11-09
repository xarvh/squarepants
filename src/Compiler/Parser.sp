
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
    oneToken >> on token:
    (Token _ _ k) = token
    if targetKind == k then
        ok token

    else
        Parser.reject


defop as Parser None =

    oneToken >> on token:
    try token as
        Token _ _ Token.Defop:
            ok None

        _:
            Parser.reject



#
# Combinators
#
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
#errorShouldUseDefNormalHere as Text =
#    "You should use a normal `=` here."


aliasDef as Env: Parser FA.Statement =
    env:

    kind (Token.LowerName Token.NameNoModifier Nothing "alias" []) >> on _:
    upperNameBare env >> on name:
    Parser.zeroOrMore (lowerNameBare env) >> on args:
    defop >> on None:
    inlineOrBelowOrIndented (typeExpr env) >> on ty:
#    if defModifier /= Token.DefNormal then
#        Parser.abort errorShouldUseDefNormalHere
#
#    else
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
            ok << (At p name) & args

        _:
            Parser.reject


unionDef as Env: Parser FA.Statement =
    env:

    kind (Token.LowerName Token.NameNoModifier Nothing "union" []) >> on _:
    upperNameBare env >> on (At p name):
    Parser.zeroOrMore (lowerNameBare env) >> on args:
    defop >> on None:
    inlineOrBelowOrIndented (rawList (unionConstructor env)) >> on cons:
#    if defModifier /= Token.DefNormal then
#        Parser.abort errorShouldUseDefNormalHere
#
#    else
        { name = name
        , args = List.map Pos.drop args
        , constructors = cons
        }
            >> FA.UnionDef p
            >> ok


#
# Term
#

#term env =
#    as Env: Parser FA.Expression
#
#    oneToken >> on (Token start end k):
#
#    p =
#        pos env start end
#
#    try k as
#        Token.NumberLiteral s:
#            ok << FA.LiteralNumber p s
#
#        Token.TextLiteral s:
#            ok << FA.LiteralText p s
#
#        Token.UpperName maybeModule name:
#            ok << FA.Constructor p maybeModule name
#
#        Token.LowerName modifier maybeModule name attrs:
#            try modifier as
#                Token.NameNoModifier:
#                    # This is a HACK and probably the world would be a better place if I cleaned this up
#                    # TODO clean this up once I have a better way to debug the parser
#                    Parser.maybe lambdaColon >> on maybeColon:
#                        try maybeColon as
#                            Nothing:
#                                ok << FA.Variable p maybeModule name attrs
#
#                            Just mutable:
#                                lambdaBody env >> on b:
#                                ok << FA.Lambda p (FA.PatternAny p False name Nothing) mutable b
#
#                Token.NameMutable:
#                    ok << FA.Mutable p name attrs
#
#                Token.NameStartsWithDot:
#                    ok << FA.RecordShorthand p (name :: attrs)
#
#        _:
#            Parser.reject



exprWithLeftDelimiter as Env: Parser FA.Expression =
    env:

    oneToken >> on (Token start end k):

        p =
            pos env start end

        try k as
            Token.Name name:
                FA.Name p name >> ok

            Token.NumberLiteral s:
                FA.LiteralNumber p s >> ok

            Token.TextLiteral s:
                FA.LiteralText p s >> ok

            Token.RoundParen Token.Open:
                exprParser as Parser Expression =
                    discardSecond
                        (expr env)
                        (kind (Token.RoundParen Token.Closed))

                inlineOrBelowOrIndented exprParser

            Token.SquareBracket Token.Open:
                item as Parser (Bool & Expression) =
                    maybe (kind Token.ThreeDots) >> on maybeDots:
                    exp env >> on exp:
                    ok (maybeDots /= Nothing & exp)

                rawList item >> on exps:
                kind (Token.SquareBracket Token.Closed) >> on _:
                ok << FA.List p exps


            Token.CurlyBrace Token.Open:
                extension as Parser (Maybe Expression) =
                    discardSecond
                        (maybe (expr env))
                        (kind Token.With)

                attribute as Parser { name as Name, maybeAnnotation as Maybe Type, maybeExpr as Maybe Expression } =
                    word >> on name:
                    maybe asAnnotation >> on maybeAnnotation:
                    maybe (discardFirst defop (inlineOrBelowOrIndented (expr env))) >> on maybeExpr:
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
                >> FA.Try (pos env start end)
                >> ok

            _:
                Parser.reject


#
# Expr (with precedence rules)
#


expr as Env: Parser FA.Expression =
    env:

    ho =
        Parser.higherOr

    nest =
        Parser.breakCircularDefinition _: expr env

    Parser.expression
        (exprWithLeftDelimiter env)
        # the `Or` stands for `Or higher priority parser`
        [
#        , ho << parens (Parser.oneOf [ binopInsideParens env, nest ])
#        , ho << list env FA.List nest
#        , ho << record env Token.Defop FA.Record nest
#        , ho << lambda env
#        , unopsOr env
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
#        , ho << if_ env
#        , ho << try_ env
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
    here >> on start:
    surroundMultiline (Token.SquareBracket Token.Open) (Token.SquareBracket Token.Closed) (Parser.maybe (rawList main)) >> on maybeLs:
    here >> on end:
    theParserStillSucks =
        try maybeLs as
            Just ls:
                ls

            Nothing:
                []
    theParserStillSucks
        >> constructor (pos env start end)
        >> ok



#
# Record
#


record as Env: Token.Kind: (Pos: FA.RecordArgs a: a): Parser a: Parser a =
    env: assign: constructor: main:

    attrAssignment =
        discardFirst (kind assign) (inlineOrBelowOrIndented main)

    attr =
        lowerNameBare env >> on name:
        Parser.maybe attrAssignment >> on maybeAssignment:
        ok ( name & maybeAssignment )

    updateTarget =
        main >> on h:
        kind Token.With >> on _:
        ok h

    content =
        start:
        Parser.maybe updateTarget >> on maybeUpdateTarget:
        (inlineOrBelowOrIndented << rawList attr) >> on attrs:
        here >> on end:
        { extends = maybeUpdateTarget
        , attrs = attrs
        }
            >> constructor (pos env start end)
            >> ok

    here >> on s:
    surroundMultiline (Token.CurlyBrace Token.Open) (Token.CurlyBrace Token.Closed) (Parser.maybe << content s) >> on maybeRecord:
    here >> on e:
    try maybeRecord as
        Just re:
            ok re

        Nothing:
            { extends = Nothing
            , attrs = []
            }
                >> constructor (pos env s e)
                >> ok



#
# if..else
#


if_ as Env: Parser FA.Expression =
    env:

    maybeNewLine = k:
        discardFirst
            (Parser.maybe (kind Token.NewSiblingLine))
            (kind k)

    kind Token.If >> on (Token start _ _):
    expr env >> on condition:
    Parser.maybe (maybeNewLine Token.Then) >> on maybeThen:
    if maybeThen == Nothing then
        Parser.abort "`if` should be followed by a `then` but I can't find it"

    else
        inlineStatementOrBlock env >> on true:
        maybeNewLine Token.Else >> on _:
        Parser.maybe (kind Token.Colon) >> on _:
        inlineStatementOrBlock env >> on false:
        here >> on end:
        { isCompact = False
        , condition = condition
        , true = true
        , false = false
        }
            >> FA.If (pos env start end)
            >> ok



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
        pattern env >> on p:
        maybeNewLineKind Token.Colon >> on _:
        inlineStatementOrBlock env >> on accept:
        ok ( p & accept )

    kind Token.Try >> on (Token start _ _):
    expr env >> on value:
    maybeNewLineKind Token.As >> on _:
    block (Parser.zeroOrMore (maybeNewLine patternAndAccept)) >> on patterns:
    here >> on end:
    { isCompact = False
    , value = value
    , patterns = patterns
    }
        >> FA.Try (pos env start end)
        >> ok



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
    inlineOrBelowOrIndented defop >> on defModifier:
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


typeTerm as Env: Parser FA.Type =
    env:

    oneToken >> on (Token start end k):
    try k as
        Token.UpperName maybeModule name:
            ok << FA.TypeConstant (pos env start end) maybeModule name []

        Token.LowerName Token.NameNoModifier Nothing name []:
            ok << FA.TypeVariable (pos env start end) name

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
        , mutopOr env
        , typeConstructorAppOr env
        , typeTupleOr env
        , typeFunctionOr env
        ]



mutopOr as Env: Parser FA.Type: Parser FA.Type =
    env: higher:

    Parser.maybe (kind Token.Mutop) >> on maybeUnary:
    higher >> on right:
    here >> on end:
    try maybeUnary as
        Just (Token start _ _ ):
            ok << FA.TypeMutable (pos env start end) right

        Nothing:
            ok right





typeTupleOr as Env: Parser FA.Type: Parser FA.Type =
    env: higher:

    binopAndPrev as Parser FA.Type =
        discardFirst (binaryOperators Op.Tuple) higher

    here >> on start:
    higher >> on head:
    Parser.zeroOrMore binopAndPrev >> on tail:
    here >> on end:
    if tail == [] then
        ok head

    else
        (head :: tail)
            >> FA.TypeTuple (pos env start end)
            >> ok


typeParens as Parser FA.Type: Parser FA.Type =
    main:
    surroundStrict
        (Token.RoundParen Token.Open)
        (Token.RoundParen Token.Closed)
        main


typeList as Env: Parser FA.Type: Parser FA.Type =
    env: main:
    here >> on start:
    surroundStrict (Token.SquareBracket Token.Open) (Token.SquareBracket Token.Closed) main >> on t:
    here >> on end:
    ok << FA.TypeList (pos env start end) t


typeFunctionOr as Env: Parser FA.Type: Parser FA.Type =
    env: higher:

    arrowAndHigher as Parser ( LambdaModifier & Pos & FA.Type ) =
        arrow env >> on ( lambdaModifier & p ):
        higher >> on h:
        ok ( lambdaModifier & p & h )

    fold as ( LambdaModifier & Pos & FA.Type ): ( LambdaModifier & FA.Type ): ( LambdaModifier & FA.Type ) =

        ( nextIsMutable & p & ty ):
        ( lambdaModifier & accum ):

        ( nextIsMutable & FA.TypeFunction p ty lambdaModifier accum)

    here >> on fs:
    higher >> on e:
    here >> on fe:
    Parser.zeroOrMore arrowAndHigher >> on es:

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
        reverseRec ( LambdaNormal & firstPos & e ) es []

    thisIsMutable & return
        >> List.for reversedArgs fold
        >> x: x.second
        >> ok


# TODO this is not an "arrow" any more
arrow as Env: Parser ( LambdaModifier & Pos ) =
    env:

    oneToken >> on (Token start end k):
    try k as
        Token.Colon:
            ok ( LambdaNormal & (pos env start end) )

        Token.ConsumingColon:
            ok ( LambdaConsuming & (pos env start end) )

        _:
            Parser.reject


typeConstructorAppOr as Env: Parser FA.Type: Parser FA.Type =
    env: higher:

    higher >> on ty:
    try ty as
        FA.TypeConstant p1 maybeModule name []:
            (Parser.zeroOrMore higher) >> on args:
            here >> on end2:
            if args == [] then
                ok ty

            else
                ok << FA.TypeConstant p1 maybeModule name args

        _:
            ok ty


#
# Lambda
#

#lambdaColon =
#    as Parser Bool
#
#    Parser.oneOf
#        [ kind Token.Colon >> on _: ok False
#        , kind Token.MutableColon >> on _: ok True
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
        , Parser.oneOrMore (sib (statement env)) >> on (h & t):
          ok << h :: t
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
#    pattern env >> on param:
#    lambdaColon >> on mutable:
#    lambdaBody env >> on b:
#    ok << FA.Lambda (FA.patternPos param) param mutable b


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

    oneToken >> on (Token start end k):

    p = pos env start end

    try k as
        Token.NumberLiteral s:
            s
                >> FA.PatternLiteralNumber p
                >> ok

        Token.TextLiteral s:
            s
                >> FA.PatternLiteralText p
                >> ok

        Token.Mutop:
            lowerNameBare env >> on (At pp name):
            Parser.maybe (inlineOrBelowOrIndented << typeAnnotation env) >> on maybeTy:
            ok << FA.PatternAny pp True name maybeTy

        Token.LowerName Token.NameNoModifier Nothing name []:
            Parser.maybe (inlineOrBelowOrIndented << typeAnnotation env) >> on maybeTy:
            ok << FA.PatternAny p False name maybeTy


        Token.UpperName maybeModule name:
            Parser.zeroOrMore param >> on params:
            here >> on end1:
            ok << FA.PatternConstructor (pos env start end1) maybeModule name params

        _:
            Parser.reject


typeAnnotation as Env: Parser FA.Type =
    env:

    discardFirst
        (kind Token.As)
        (inlineOrBelowOrIndented (typeExpr env))


patternBinopOr as Env: Op.Precedence: (Pos: [FA.Pattern]: FA.Pattern): Parser FA.Pattern: Parser FA.Pattern =
    env: precedenceGroup: constructor: higher:

    here >> on start:
    sepList (binaryOperators precedenceGroup) higher >> on ( head & sepTail ):
    here >> on end:
    if sepTail == [] then
        ok head

    else
        (head :: List.map (x: x.second) sepTail)
            >> constructor (pos env start end)
            >> ok


#
# Function application
#

recInlineOrIndentedOrBelow as Parser FA.Expression: [FA.Expression]: Parser [FA.Expression] =
    higher: accum:
    higher >> on h:

    r =
        h :: accum

    maybeWithDefault r << inlineOrBelowOrIndented (recInlineOrIndentedOrBelow higher r)


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
            ok << FA.FunctionCall (pos env start end) fnExpression args



#
# Unops
#


unopsOr as Env: Parser FA.Expression: Parser FA.Expression =
    env: higher:

    Parser.maybe unaryOperator >> on maybeUnary:
    higher >> on right:
    here >> on end:
    try maybeUnary as
        Just ( op & Token start _ _ ):
            ok << FA.Unop (pos env start end) op right

        Nothing:
            ok right


unaryOperator as Parser ( Op.Unop & Token ) =

    oneToken >> on token:
    try token as
        Token s e (Token.Unop op):
            ok ( op & token )

        _:
            Parser.reject



#
# Binops
#


binopInsideParens as Env: Parser FA.Expression =
    env:

    oneToken >> on (Token start end k):
    try k as
        Token.Binop binop:
            ok << FA.PrefixBinop (pos env start end) binop.symbol

        _:
            Parser.reject


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

