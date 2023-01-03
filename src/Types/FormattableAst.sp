[# This AST is meant to reflect more closely the cosmetic choices of the user.

It is meant for two purposes:

1.  Transform it into canonical AST for compiling
2.  Transform it back into human-readable, nicely formatted code

It is a lot more permissive than what the actual language syntax allows, because in this way we can give more helpful error messages to the user.
Instead than errors at parse time, we can produce more meaningful errors when translating into canonical.

#]


alias Module =
    [Statement]


alias ValueDef =
    {
    , pattern as Expression
    , nonFn as [At Token.Word]
    , body as Expression
    }


alias AliasDef =
    {
    , name as At Token.Word
    # TODO rename to pars
    , args as [At Token.Word]
    , type as Expression
    }


alias UnionDef =
    {
    , name as At Token.Word
    , args as [At Token.Word]
    , constructors as [Expression]
    }


union Statement =
    , Evaluation Expression
    , ValueDef ValueDef
    , AliasDef AliasDef
    , UnionDef UnionDef


# expr op expr op expr op...
alias SepList sep item =
    item & [ sep & item ]


# TODO
#
# alias Expression = At Expr_ ?
# alias AtExpression = At Expression ?
#
union Expression =
    Expression Pos Expr_

union Expr_ =
    , LiteralText Text
    , LiteralNumber Text
    , ArgumentPlaceholder

    , Statements [Statement]

    # The Bool is for whether there's a trailing `...`
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
        # TODO rename to atWord
        , word as Token.Word
        }

    , Fn [Expression] Expression

    , Call Expression [Expression]

    , Binop Op.Precedence (SepList Op.Binop Expression)

    , Unop Op.UnopId Expression

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

alias RecordAttribute =
    {
    , name as Expression
    , maybeExpr as Maybe Expression
    }


sepToList as SepList sep item: [item] =
    (head & tuples):
    head :: List.map Tuple.second tuples


statementPos as Statement: Pos =
    statement:
    try statement as
        Evaluation (Expression pos expr_): pos
        # TODO: the position should encompass the WHOLE definition, not just its start
        ValueDef { pattern = Expression pos expr_, nonFn, body }: pos
        AliasDef { name = At pos _, args, type }: pos
        UnionDef { name = At pos _, args, constructors }: pos

