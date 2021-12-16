

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


here as Parser Int =
    Parser.here >> then tokens:
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
            Token start end k :: rest: Pos.End moduleName

    Error.res p (eenv: [ message ])


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

    failureStates & outcome =
        as [[Token]] & Parser.Outcome Token output

        notComment = t:
            Token s e k = t
            k /= Token.Comment

        tokens
            >> List.filter notComment
            >> Parser.runParser parser

    try outcome as
        Parser.Accepted readState output:
            Ok output

        Parser.Aborted readState message:
            makeError moduleName readState message

        Parser.Rejected:
            findMin readState best =
                if List.length readState < List.length best then readState else best

            readState as [Token] =
                List.foldl findMin failureStates tokens

            message =
                try readState as
                    []: "I got to the end of file and I can't make sense of it. =("
                    _: "I got stuck parsing here. =("

            makeError moduleName readState message


module_ as Env: Parser [FA.Statement] =
    env:

    start =
        Parser.oneOf
            [ kind Token.BlockStart
            , kind Token.NewSiblingLine
            ]

    # This is called `zzz` rather than `end` because apparently there is some really
    # bad problems with sorting that result in this (?) being declared before Parser.end?
    zzz =
        Parser.zeroOrMore (kind Token.NewSiblingLine) >> then _: Parser.end

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
    oneToken >> then token:
    (Token _ _ k) = token
    if targetKind == k then
        Parser.accept token

    else
        Parser.reject


upperNameBare as Env: Parser (At Text) =
    env:
    oneToken >> then token:
    try token as
        Token start end (Token.UpperName Nothing name):
            Parser.accept << At (pos env start end) name
        _:
            Parser.reject


lowerNameBare as Env: Parser (At Text) =
    env:
    oneToken >> then token:
    try token as
        Token start end (Token.LowerName Token.NameNoModifier Nothing name []):
            Parser.accept << At (pos env start end) name
        _:
            Parser.reject


defop as Parser { mutable as Bool } =
    oneToken >> then token:
    try token as
        Token _ _ (Token.Defop arg):
            Parser.accept arg

        _:
            Parser.reject



#
# Combinators
#
discardFirst as Parser a: Parser b: Parser b =
    a: b:
    a >> then _: b


discardSecond as Parser a: Parser b: Parser a =
    a: b:
    a >> then aa:
    b >> then _:
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
    pa >> then head:
    Parser.zeroOrMore (discardFirst sep pa) >> then tail:
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
    sep >> then sep0:
    (Parser.oneOf
        [ block (sepListAtItem sep item)
        , sib (sepListAtItem sep item)
        , sepListAtItem sep item
        ]
    ) >> then x:
        item0 & tail = x
        Parser.accept << sep0 & item0 :: tail



sepListAtItem as Parser sep: Parser item: Parser (FA.SepList sep item) =
    sep: item:
    item >> then item0:
    (Parser.oneOf
        [ block (sepListAtSep sep item)
        , sib (sepListAtSep sep item)
        , sepListAtSep sep item
        , Parser.accept []
        ]
    ) >> then sepsAndItems:
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
        #             (Parser.maybe << kind Token.NewSiblingLine) >> then _:
        #             kind Token.Comma
        # but I didn't test it properly
        inlineOrBelowOrIndented << kind Token.Comma

    discardFirst (Parser.maybe sibsep) (oomSeparatedBy sibsep item)



#
# Statements
#
errorCantUseMutableAssignmentHere as Text =
    "Can't use mutable assignment here"


typeAlias as Env: Parser FA.Statement =
    env:
    kind (Token.LowerName Token.NameNoModifier Nothing "alias" []) >> then _:
    upperNameBare env >> then name:
    Parser.zeroOrMore (lowerNameBare env) >> then args:
    defop >> then a:
    (inlineOrBelowOrIndented (typeExpr env)) >> then ty:
    if a.mutable then
        Parser.abort errorCantUseMutableAssignmentHere

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

    kind (Token.LowerName Token.NameNoModifier Nothing "union" []) >> then _:
    upperNameBare env >> then pp:
    At p name = pp
    Parser.zeroOrMore (lowerNameBare env) >> then args:
    defop >> then a:
    inlineOrBelowOrIndented (rawList (unionConstructor env)) >> then cons:
    if a.mutable then
        Parser.abort errorCantUseMutableAssignmentHere

    else
        { name = name
        , args = List.map Pos.drop args
        , constructors = cons
        }
            >> FA.UnionDef p
            >> Parser.accept


unionConstructor as Env: Parser FA.Constructor =
    env:

    typeExpr env >> then type:
    try type as
        FA.TypeConstant p Nothing name args:
            Parser.accept << (At p name) & args

        _:
            Parser.reject


#
# Term
#

term as Env: Parser FA.Expression =
    env:

    oneToken >> then tt:
    Token start end k = tt

    p =
        pos env start end

    try k as
        Token.NumberLiteral s:
            Parser.accept << FA.LiteralNumber p s

        Token.TextLiteral s:
            Parser.accept << FA.LiteralText p s

        Token.UpperName maybeModule name:
            Parser.accept << FA.Constructor p maybeModule name

        Token.LowerName modifier maybeModule name attrs:
            try modifier as
                Token.NameNoModifier:
                    # This is a HACK and probably the world would be a better place if I cleaned this up
                    # TODO clean this up once I have a better way to debug the parser
                    Parser.maybe lambdaColon >> then maybeColon:
                        try maybeColon as
                            Nothing:
                                Parser.accept << FA.Variable p maybeModule name attrs

                            Just mutable:
                                lambdaBody env >> then b:
                                Parser.accept << FA.Lambda p (FA.PatternAny p False name Nothing) mutable b

                Token.NameMutable:
                    Parser.accept << FA.Mutable p name attrs

                Token.NameStartsWithDot:
                    Parser.accept << FA.RecordShorthand p (name :: attrs)

        _:
            Parser.reject



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
        (term env)
        # the `Or` stands for `Or higher priority parser`
        [
        , higherOr << parens (Parser.oneOf [ binopInsideParens env, nest ])
        , higherOr << list env FA.List nest
        , higherOr << record env (Token.Defop { mutable = False }) FA.Record nest
        , higherOr << lambda env
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
    here >> then start:
    surroundMultiline (Token.SquareBracket Token.Open) (Token.SquareBracket Token.Closed) (Parser.maybe (rawList main)) >> then maybeLs:
    here >> then end:
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


record as Env: Token.Kind: (Pos: FA.RecordArgs a: a): Parser a: Parser a =
    env: assign: constructor: main:

    attrAssignment =
        discardFirst (kind assign) (inlineOrBelowOrIndented main)

    attr =
        lowerNameBare env >> then name:
        Parser.maybe attrAssignment >> then maybeAssignment:
        Parser.accept ( name & maybeAssignment )

    updateTarget =
        main >> then h:
        kind Token.With >> then _:
        Parser.accept h

    content start =
        Parser.maybe updateTarget >> then maybeUpdateTarget:
        (inlineOrBelowOrIndented << rawList attr) >> then attrs:
        here >> then end:
        { extends = maybeUpdateTarget
        , attrs = attrs
        }
            >> constructor (pos env start end)
            >> Parser.accept

    here >> then s:
    surroundMultiline (Token.CurlyBrace Token.Open) (Token.CurlyBrace Token.Closed) (Parser.maybe << content s) >> then maybeRecord:
    here >> then e:
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

    maybeNewLine k =
        discardFirst
            (Parser.maybe (kind Token.NewSiblingLine))
            (kind k)

    kind Token.If >> then ss:
    Token start _ _ = ss
    expr env >> then condition:
    Parser.maybe (maybeNewLine Token.Then) >> then maybeThen:
    if maybeThen == Nothing then
        Parser.abort "`if` should be followed by a `then` but I can't find it"

    else
        inlineStatementOrBlock env >> then true:
        maybeNewLine Token.Else >> then _:
        Parser.maybe (kind Token.Colon) >> then _:
        inlineStatementOrBlock env >> then false:
        here >> then end:
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
        pattern env >> then p:
        maybeNewLineKind Token.Colon >> then _:
        inlineStatementOrBlock env >> then accept:
        Parser.accept ( p & accept )

    kind Token.Try >> then ss:
    Token start _ _ = ss
    expr env >> then value:
    maybeNewLineKind Token.As >> then _:
    block (Parser.zeroOrMore (maybeNewLine patternAndAccept)) >> then patterns:
    here >> then end:
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
    Parser.maybe (kind Token.NewSiblingLine) >> then _:
    Parser.oneOf
        [ typeAlias env
        , unionDef env
        , definition env
        , expr env >> then e:
          e >> FA.Evaluation (FA.expressionPos e) >> Parser.accept
        ]


definition as Env: Parser FA.Statement =
    env:
    here >> then start:
    pattern env >> then p:
    Parser.maybe (inlineOrBelowOrIndented (nonFunction env)) >> then nf:
    inlineOrBelowOrIndented defop >> then a:
    inlineStatementOrBlock env >> then body:

#    end =
#        body
#            >> List.reverse
#            >> List.head
#            >> Maybe.map getpos
#            >> Maybe.withDefault Pos.T
#            >> Pos.end

    here >> then end:
    { pattern = p
    , mutable = a.mutable
    , body = body
    , nonFn = Maybe.withDefault [] nf
    }
        >> FA.Definition (pos env start end)
        >> Parser.accept


inlineStatementOrBlock as Env: Parser [FA.Statement] =
    env:
    Parser.oneOf
        [ Parser.breakCircularDefinition (_: expr env) >> then e: Parser.accept [FA.Evaluation (FA.expressionPos e) e]
        , block (oomSeparatedBy (kind Token.NewSiblingLine) (statement env))
        ]


#
# Types
#


nonFunction as Env: Parser [Text] =
    env:
    kind Token.With >> then _:
    rawList (lowerNameBare env) >> then nf:
    upperNameBare env >> then aa:
    At _ n = aa
    if n == "NonFunction" then
        Parser.accept << List.map Pos.drop nf
    else
        Parser.abort << "Only NonFunction is supported for now"


typeTerm as Env: Parser FA.Type =
    env:

    oneToken >> then tt:
    Token start end k = tt
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

    here >> then start:
    higher >> then head:
    Parser.zeroOrMore binopAndPrev >> then tail:
    here >> then end:
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
    here >> then start:
    surroundStrict (Token.SquareBracket Token.Open) (Token.SquareBracket Token.Closed) main >> then t:
    here >> then end:
    Parser.accept << FA.TypeList (pos env start end) t


typeFunctionOr as Env: Parser FA.Type: Parser FA.Type =
    env: higher:

    arrowAndHigher as Parser ( Bool & Pos & FA.Type ) =
        arrow env >> then mm:
        mutable & p = mm
        higher >> then h:
        Parser.accept ( mutable & p & h )

    fold as ( Bool & Pos & FA.Type ): ( Bool & FA.Type ): ( Bool & FA.Type ) =
        a: b:
        nextIsMutable & p & ty = a
        thisIsMutable & accum = b
        ( nextIsMutable & FA.TypeFunction p ty thisIsMutable accum)

    here >> then fs:
    higher >> then e:
    here >> then fe:
    Parser.zeroOrMore arrowAndHigher >> then es:

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
        >> List.foldl fold reversedArgs
        >> x: x.second
        >> Parser.accept


arrow as Env: Parser ( Bool & Pos ) =
    env:
    oneToken >> then tt:
    Token start end k = tt
    try k as
        Token.Colon:
            Parser.accept ( False & (pos env start end) )

        Token.MutableColon:
            Parser.accept ( True & (pos env start end) )

        _:
            Parser.reject


typeConstructorAppOr as Env: Parser FA.Type: Parser FA.Type =
    env: higher:
    higher >> then ty:
    try ty as
        FA.TypeConstant p1 maybeModule name []:
            (Parser.zeroOrMore higher) >> then args:
            here >> then end2:
            if args == [] then
                Parser.accept ty

            else
                Parser.accept << FA.TypeConstant p1 maybeModule name args

        _:
            Parser.accept ty


#
# Lambda
#

lambdaColon as Parser Bool =

    Parser.oneOf
        [ kind Token.Colon >> then _: Parser.accept False
        , kind Token.MutableColon >> then _: Parser.accept True
        ]




lambdaBody as Env: Parser [FA.Statement] =
    env:
    Parser.oneOf
        [ [#
             x:
             a
             b
             c
          #]
        , Parser.oneOrMore (sib (statement env)) >> then ht:
          h & t = ht
          Parser.accept << h :: t
          [#
             x: a

             x:
               a

          #]
        , inlineStatementOrBlock env
        ]


lambda as Env: Parser FA.Expression =
    env:

    pattern env >> then param:
    lambdaColon >> then mutable:
    lambdaBody env >> then b:
    Parser.accept << FA.Lambda (FA.patternPos param) param mutable b


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
        , higherOr << record env (Token.Defop { mutable = False }) FA.PatternRecord nest
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
        , record env (Token.Defop { mutable = False }) FA.PatternRecord nest
        ]


patternApplication as Env: Parser FA.Pattern: Parser FA.Pattern =
    env: param:

    oneToken >> then tt:
    Token start end k = tt

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
                Parser.maybe (inlineOrBelowOrIndented << typeAnnotation env) >> then maybeTy:
                Parser.accept << FA.PatternAny p mutable name maybeTy

            try modifier as
                Token.NameNoModifier: thingy False
                Token.NameMutable: thingy True
                Token.NameStartsWithDot: Parser.reject

        Token.UpperName maybeModule name:
            Parser.zeroOrMore param >> then params:
            here >> then end1:
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

    here >> then start:
    sepList (binaryOperators precedenceGroup) higher >> then hs:
    head & sepTail = hs
    here >> then end:
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
    higher >> then h:

    r =
        h :: accum

    maybeWithDefault r << inlineOrBelowOrIndented (recInlineOrIndentedOrBelow higher r)


functionApplicationOr as Env: Parser FA.Expression: Parser FA.Expression =
    env: higher:

    recInlineOrIndented as [FA.Expression]: Parser [FA.Expression] =
        accum:
        higher >> then h:

        r =
            h :: accum

        Parser.oneOf
            # after at least one indented block, allow arguments to appear also as siblings (ie, right below)
            [ block (recInlineOrIndentedOrBelow higher r)
            , recInlineOrIndented r
            , Parser.accept r
            ]

    here >> then start:
    recInlineOrIndented [] >> then reversedArgs:
    here >> then end:
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

    Parser.maybe unaryOperator >> then maybeUnary:
    higher >> then right:
    here >> then end:
    try maybeUnary as
        Just ( op & Token start _ _ ):
            Parser.accept << FA.Unop (pos env start end) op right

        Nothing:
            Parser.accept right


unaryOperator as Parser ( Op.Unop & Token ) =
    oneToken >> then token:
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

    oneToken >> then tt:
    Token start end k = tt
    try k as
        Token.Binop binop:
            Parser.accept << FA.PrefixBinop (pos env start end) binop.symbol

        _:
            Parser.reject


binopsOr as Env: Op.Precedence: Parser FA.Expression: Parser FA.Expression =
    env: group: higher:
    here >> then start:
    sepList (binaryOperators group) higher >> then hs:
    head & sepTail = hs
    here >> then end:
    if sepTail == [] then
        Parser.accept head

    else
        FA.Binop (pos env start end) group ( head & sepTail )
            >> Parser.accept


binaryOperators as Op.Precedence: Parser Op.Binop =
    group:
    oneToken >> then tt:
    Token s e k = tt
    try k as
        Token.Binop op:
            if op.precedence == group then
                Parser.accept op

            else
                Parser.reject

        _:
            Parser.reject

