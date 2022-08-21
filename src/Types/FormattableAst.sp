[# This AST is meant to reflect more closely the cosmetic choices of the user.

It is meant for two purposes:

1.  Transform it into canonical AST for compiling
2.  Transform it back into human-readable, nicely formatted code

It is a lot more permissive than what the actual language syntax allows, because in this way we can give more helpful error messages to the user.
Instead than errors at parse time, we can produce more meaningful errors when translating into canonical.

#]


alias Module =
    [Statement]


alias ValueDef = {
    , pattern as Pattern
    # TODO Should be At Name?
    , nonFn as [Name]
    , body as [Statement]
    }


union Statement =
    # TODO merge Evaluation and Definition, so that Evaluations are Definitions with discard pattern Pos.G
    , Evaluation Pos Expression
    , Definition Pos ValueDef
    , TypeAlias {
        , name as At Name
        , args as [At Name]
        , ty as Type
        }
    , UnionDef Pos {
        , name as Name
        , args as [Name]
        , constructors as [Constructor]
        }


alias Constructor =
    At Name & [Type]


union Type =
    , TypeVariable Pos Name
    , TypeConstant Pos (Maybe Name) Name [Type]
    , TypeFunction Pos Type LambdaModifier Type
    , TypeTuple Pos [Type]
    , TypeList Pos Type
    , TypeRecord Pos (RecordArgs Type)
    , TypeMutable Pos Type


# expr op expr op expr op...
alias SepList sep item =
      item & [ sep & item ]

sepList_mapItem as (a: b): SepList sep a: SepList sep b =
    f: aAndLa:
    ( a & la ) = aAndLa
    f a & List.map (Tuple.mapSecond f) la


union Expression =
    , LiteralText Pos Text
    , LiteralNumber Pos Text
    , Variable Pos (Maybe Name) Name [Name]
    , Mutable Pos Name [Name]
    , Constructor Pos (Maybe Name) Name
    , PrefixBinop Pos Text
    , RecordShorthand Pos [Name]
    , Lambda Pos Pattern LambdaModifier [Statement]
    , FunctionCall Pos Expression [Expression]
    , Binop Pos Op.Precedence (SepList Op.Binop Expression)
    , Unop Pos Op.Unop Expression
    , If Pos {
        , isCompact as Bool
        , condition as Expression
        , true as [Statement]
        , false as [Statement]
        }
    , Try Pos {
        , isCompact as Bool
        , value as Expression
        , patterns as [ Pattern & [Statement] ]
        }
    , Record Pos (RecordArgs Expression)
    , List Pos [Expression]

union Pattern =
    , PatternAny Pos Bool Name (Maybe Type)
    , PatternLiteralNumber Pos Text
    , PatternLiteralText Pos Text
    , PatternConstructor Pos (Maybe Name) Name [Pattern]
    , PatternList Pos [Pattern]
    , PatternListCons Pos [Pattern]
    , PatternRecord Pos (RecordArgs Pattern)
    , PatternTuple Pos [Pattern]


alias RecordArgs expr = {
    , extends as Maybe expr
    , attrs as [ At Name & Maybe expr ]
    }


#
# Helpers
#


statementPos as Statement: Pos =
    statement:
    try statement as
        Evaluation pos _: pos
        Definition pos _: pos
        TypeAlias { name = At pos _, args, ty }: pos
        UnionDef pos _: pos


typePos as Type: Pos =
    type:
    try type as
        TypeVariable p _: p
        TypeConstant p _ _ _: p
        TypeFunction p _ _ _: p
        TypeTuple p _: p
        TypeList p _: p
        TypeRecord p _: p
        TypeMutable p _: p


expressionPos as Expression: Pos =
    expr:
    try expr as
       LiteralText p _: p
       LiteralNumber p _: p
       Variable p _ _ _: p
       Constructor p _ _: p
       Mutable p _ _: p
       PrefixBinop p _: p
       Lambda p _ _ _: p
       FunctionCall p _ _: p
       Binop p _ _: p
       Unop p _ _: p
       If p _: p
       Try p _: p
       Record p _: p
       RecordShorthand p _: p
       List p _: p


patternPos as Pattern: Pos =
    pa:
    try pa as
        PatternAny p _ _ _: p
        PatternLiteralNumber p _: p
        PatternLiteralText p _: p
        PatternConstructor p _ _ _: p
        PatternList p _: p
        PatternListCons p _: p
        PatternRecord p _: p
        PatternTuple p _: p


patternNames as Pattern: Dict Name Pos =
    pattern:

    foldOver =
        pas:
        List.for pas (p: p >> patternNames >> Dict.join) Dict.empty

    insertAttr =
        a:
        ((At pos name) & maybePa) = a
        try maybePa as
            Nothing: Dict.insert name pos
            Just pat: pat >> patternNames >> Dict.join

    try pattern as
        PatternAny pos _ n _: Dict.singleton n pos
        PatternLiteralNumber _ _: Dict.empty
        PatternLiteralText _ _: Dict.empty
        PatternConstructor _ _ _ pas: foldOver pas
        PatternList _ pas: foldOver pas
        PatternListCons _ pas: foldOver pas
        PatternRecord pos ars: List.for ars.attrs insertAttr Dict.empty
        PatternTuple _ pas: foldOver pas

