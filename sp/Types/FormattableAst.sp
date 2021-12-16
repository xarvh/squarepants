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
    , mutable as Bool
    , nonFn as [Name]
    , body as [Statement]
    }


union Statement =
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
    , TypeFunction Pos Type Bool Type
    , TypeTuple Pos [Type]
    , TypeList Pos Type
    , TypeRecord Pos (RecordArgs Type)


# expr op expr op expr op...
alias SepList sep item =
      item & [ sep & item ]

sepList_mapItem f ( a & la ) =
    as (a: b): SepList sep a: SepList sep b
    f a & List.map (Tuple.mapSecond f) la


union Expression =
    , LiteralText Pos Text
    , LiteralNumber Pos Text
    , Variable Pos (Maybe Name) Name [Name]
    , Constructor Pos (Maybe Name) Name
    , Mutable Pos Name [Name]
    , PrefixBinop Pos Text
    , RecordShorthand Pos [Name]
    , Lambda Pos Pattern Bool [Statement]
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
    # TODO: remove the Bool from the pattern, it can never be mutable
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


statementPos statement =
    as Statement: Pos

    try statement as
        Evaluation pos _: pos
        Definition pos _: pos
        TypeAlias { name = At pos _ }: pos
        UnionDef pos _: pos


typePos type =
    as Type: Pos
    try type as
        TypeVariable p _: p
        TypeConstant p _ _ _: p
        TypeFunction p _ _ _: p
        TypeTuple p _: p
        TypeList p _: p
        TypeRecord p _: p



expressionPos expr =
    as Expression: Pos
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



patternPos pa =
    as Pattern: Pos
    try pa as
        PatternAny p _ _ _: p
        PatternLiteralNumber p _: p
        PatternLiteralText p _: p
        PatternConstructor p _ _ _: p
        PatternList p _: p
        PatternListCons p _: p
        PatternRecord p _: p
        PatternTuple p _: p


patternNames pattern =
    as Pattern: Dict Name Pos

    foldOver pas =
        List.foldl (fn p: p >> patternNames >> Dict.join) pas Dict.empty

    insertAttr ((At pos name) & maybePa) =
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
        PatternRecord pos ars: List.foldl insertAttr ars.attrs Dict.empty
        PatternTuple _ pas: foldOver pas

[#

recordArgs_map f ar =
    as (a: b): RecordArgs a: RecordArgs b
    {
    , extends = Maybe.map f ar.extends
    , attrs = List.map (Tuple.mapSecond (Maybe.map f)) ar.attrs
    }


posMap_statement f stat =
    as (Pos: Pos): Statement: Statement
    try stat as
        Evaluation p e:
            Evaluation (f p) (posMap_expression f e)

        Definition p def:
            Definition (f p) {
                , pattern = posMap_pattern f def.pattern
                , mutable = def.mutable
                , nonFn = def.nonFn
                , body = List.map (posMap_statement f) def.body
                }

        TypeAlias ar:
            TypeAlias {
                , name = ar.name
                , args = ar.args
                , ty = posMap_type f ar.ty
                }

        UnionDef p ar:
            UnionDef (f p) {
                , name = ar.name
                , args = ar.args
                , constructors = List.map (posMap_constructor f) ar.constructors
                }


posMap_constructor f (At pos name & types) =
    as (Pos: Pos): Constructor: Constructor
    At (f pos) name & List.map (posMap_type f) types


posMap_expression f expr =
    as (Pos: Pos): Expression: Expression
    try expr as
        LiteralNumber pos value:
            LiteralNumber (f pos) value

        LiteralText pos value:
            LiteralText (f pos) value

        Variable pos isbin name:
            Variable (f pos) isbin name

        Mutable pos name:
            Mutable (f pos) name

        Lambda pos pa stats:
            Lambda (f pos)
                (posMap_pattern f pa)
                (List.map (posMap_statement f) stats)

        FunctionCall pos ref args:
            FunctionCall (f pos)
                (posMap_expression f ref)
                (List.map (posMap_expression f) args)

        Binop pos group sepList:
            Binop (f pos) group (sepList_mapItem (posMap_expression f) sepList)

        Unop pos name right:
            Unop (f pos) name (posMap_expression f right)

        If pos ar:
            If (f pos)
                { isCompact = ar.isCompact
                , condition = posMap_expression f ar.condition
                , true = List.map (posMap_statement f) ar.true
                , false = List.map (posMap_statement f) ar.false
                }

        Try pos ar:
            Try (f pos)
                { isCompact = ar.isCompact
                , value = posMap_expression f ar.value
                , patterns = List.map (Tuple.mapBoth (posMap_pattern f) (List.map (posMap_statement f))) ar.patterns
                }

        Record pos ar:
            Record (f pos) (recordArgs_map (posMap_expression f) ar)

        List pos exs:
            List (f pos) (List.map (posMap_expression f) exs)


posMap_pattern f pa =
    as (Pos: Pos): Pattern: Pattern
    try pa as
        PatternAny pos mutable name maybeType:
            PatternAny (f pos) mutable name (Maybe.map (posMap_type f) maybeType)

        PatternLiteralNumber pos val:
            PatternLiteralNumber (f pos) val

        PatternLiteralText pos val:
            PatternLiteralText (f pos) val

        PatternApplication pos cons pas:
            PatternApplication (f pos) cons (List.map (posMap_pattern f) pas)

        PatternList pos pas:
            PatternList (f pos) (List.map (posMap_pattern f) pas)

        PatternRecord pos ar:
            PatternRecord (f pos) (recordArgs_map (posMap_pattern f) ar)

        PatternCons pos pas:
            PatternCons (f pos) (List.map (posMap_pattern f) pas)

        PatternTuple pos pas:
            PatternTuple (f pos) (List.map (posMap_pattern f) pas)


posMap_type f ty =
    as (Pos: Pos): Type: Type
    try ty as
        TypeVariable pos name:
            TypeVariable (f pos) name

        TypeConstant pos name args:
            TypeConstant (f pos) name (List.map (posMap_type f) args)

        TypeFunction pos from mut to:
            TypeFunction (f pos) (posMap_type f from) mut (posMap_type f to)

        TypeTuple pos tys:
            TypeTuple (f pos) (List.map (posMap_type f) tys)

        TypeRecord pos ar:
            TypeRecord (f pos) (recordArgs_map (posMap_type f) ar)
            #]
