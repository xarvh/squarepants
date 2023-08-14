[# This AST is meant to reflect more closely the cosmetic choices of the user.

It is meant for two purposes:

1.  Transform it into canonical AST for compiling
2.  Transform it back into human-readable, nicely formatted code

It is a lot more permissive than what the actual language syntax allows, because in this way we can give more helpful error messages to the user.
Instead than errors at parse time, we can produce more meaningful errors when translating into canonical.

#]


alias Module =
    [Statement]


alias InlineComments =
    [Text]


union Word =
    , Word InlineComments Pos Token.Word


alias ValueDef = {
    , pattern as Expression
    , nonFn as [Word]
    , body as Expression
    }


alias AliasDef = {
    , name as Word
    # TODO rename to pars
    , args as [Word]
    , type as Expression
    }


alias UnionDef = {
    , name as Word
    , args as [Word]
    , constructors as [Expression]
    }


union Statement =
    , Evaluation Expression
    , ValueDef ValueDef
    , AliasDef AliasDef
    , UnionDef UnionDef


union Expression =
    Expression InlineComments Pos InlineComments Expr_


union Expr_ =
    , LiteralText Text
    , LiteralNumber Bool Text
    , ArgumentPlaceholder

    , Statements [Statement]

    # The Bool is for whether there's a leading `...`
    , List [Bool & Expression]

    #
    , Record
        {
        # no extension: { attr1, ... }
        # pattern extension: { with attr1, ... }
        # expression extension: { expt with attr1, ... }
        , maybeExtension as Maybe (Maybe Expression)
        , attrs as [RecordAttribute]
        }

    , Variable
        {
        , maybeType as Maybe Expression
        , word as Word
        }

    , Fn [Expression] Expression

    # This is only an intermediate value, always replaced by UnopCall in the output
    , Unop Op.UnopId

    , UnopCall Op.UnopId Expression

    # This is only an intermediate value, always replaced by BinopChain in the output
    , Binop Op.Binop

    , BinopChain Int (SepList Op.Binop Expression)

    , Call Expression [Expression]

    , Poly Text Expression

    , If
        {
        , condition as Expression
        , true as Expression
        , false as Expression
        }

    , Try
        {
        , value as Expression
        , patterns as [ Expression & Expression ]
        }


alias RecordAttribute = {
    , name as Expression
    , maybeExpr as Maybe Expression
    }


# expr op expr op expr op...
alias SepList sep item =
    item & [ sep & item ]


# TODO rename to sepList_items
sepToList as fn SepList sep item: [item] =
    fn (head & tuples):
    head :: List.map Tuple.second tuples


sepList_reverse as fn SepList sep item: SepList sep item =

    rec as fn [sep & item], SepList sep item: SepList sep item =
        fn acc, oddItem & remainder:
        try remainder as
            , []: oddItem & acc
            , [sep & item, ...tail]: rec [sep & oddItem, ...acc] (item & tail)

    rec [] __


sepList_allSeps as fn (fn sep: Bool), SepList sep item: Bool =
    fn f, ls:
    try ls.second as
        , []:
            True

        , [sep & item, ...tail]:
            if f sep then
                sepList_allSeps f (item & tail)

            else
                False




statementPos as fn Statement: Pos =
    fn statement:
    try statement as
        , Evaluation (Expression pos expr_): pos
        # TODO: the position should encompass the WHOLE definition, not just its start
        , ValueDef { pattern = Expression pos expr_, nonFn, body }: pos
        , AliasDef { name = At pos _, args, type }: pos
        , UnionDef { name = At pos _, args, constructors }: pos

