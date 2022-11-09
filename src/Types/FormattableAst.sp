[# This AST is meant to reflect more closely the cosmetic choices of the user.

It is meant for two purposes:

1.  Transform it into canonical AST for compiling
2.  Transform it back into human-readable, nicely formatted code

It is a lot more permissive than what the actual language syntax allows, because in this way we can give more helpful error messages to the user.
Instead than errors at parse time, we can produce more meaningful errors when translating into canonical.

#]


alias Module =
    [Statement]


union Statement =
    , Evaluation Expression
    , ValueDef
        {
        , pattern as Expression
        , nonFn as [Token.Word]
        , body as Expression
        }
    , AliasDef
        {
        , name as At Word
        , args as [At Word]
        , type as Expression
        }
    , UnionDef Pos
        {
        , name as Word
        , args as [Word]
        , constructors as [At Word & [Expression]]
        }


# expr op expr op expr op...
alias SepList sep item =
    item & [ sep & item ]


union Expression =
    , LiteralText Pos Text
    , LiteralNumber Pos Text

    , Statements Pos [Statement]

    # The Bool is for whether there's a trailing `...`
    , List Pos [Bool & Expression]

    #
    , Record Pos
        {
        # no extension: { attr1, ... }
        # pattern extension: { with attr1, ... }
        # expression extension: { expt with attr1, ... }
        , maybeExtension as Maybe (Maybe Expression)
        , attrs as Dict Name { pos as Pos, maybeType as Type, maybeExpr as Maybe Expression }
        }

    , Variable Pos
        {
        , isUnique as Bool
        , maybeModule as Maybe Name
        , name as Name
        , attrPath as [Name]
        }

    , RecordShorthand Pos [Name]

    , Fn Pos [Expression] Expression

    , Call Pos Expression [Expression]

    , Binop Pos Op.Precedence (SepList Op.Binop Expression)

    , Unop Pos Op.Unop Expression

    , If Pos {
        , condition as Expression
        , true as Expression
        , false as Expression
        }

    , Try Pos {
        , value as Expression
        , patterns as [ Expression & Expression ]
        }



#
# Helpers
#


#statementPos as Statement: Pos =
#    statement:
#    try statement as
#        Evaluation pos _: pos
#        Definition pos _: pos
#        TypeAlias { name = At pos _, args, ty }: pos
#        UnionDef pos _: pos
#
#
#typePos as Type: Pos =
#    type:
#    try type as
#        TypeVariable p _: p
#        TypeConstant p _ _ _: p
#        TypeFunction p _ _ _: p
#        TypeTuple p _: p
#        TypeList p _: p
#        TypeRecord p _: p
#        TypeMutable p _: p
#
#
#expressionPos as Expression: Pos =
#    expr:
#    try expr as
#       LiteralText p _: p
#       LiteralNumber p _: p
#       Variable p _ _ _: p
#       Constructor p _ _: p
#       Mutable p _ _: p
#       PrefixBinop p _: p
#       Lambda p _ _ _: p
#       FunctionCall p _ _: p
#       Binop p _ _: p
#       Unop p _ _: p
#       If p _: p
#       Try p _: p
#       Record p _: p
#       RecordShorthand p _: p
#       List p _: p



#patternNames as Pattern: Dict Name Pos =
#    pattern:
#
#    foldOver =
#        pas:
#        List.for pas (p: p >> patternNames >> Dict.join) Dict.empty
#
#    insertAttr =
#        a:
#        ((At pos name) & maybePa) = a
#        try maybePa as
#            Nothing: Dict.insert name pos
#            Just pat: pat >> patternNames >> Dict.join
#
#    try pattern as
#        PatternAny pos _ n _: Dict.singleton n pos
#        PatternLiteralNumber _ _: Dict.empty
#        PatternLiteralText _ _: Dict.empty
#        PatternConstructor _ _ _ pas: foldOver pas
#        PatternList _ pas: foldOver pas
#        PatternListCons _ pas: foldOver pas
#        PatternRecord pos ars: List.for ars.attrs insertAttr Dict.empty
#        PatternTuple _ pas: foldOver pas

