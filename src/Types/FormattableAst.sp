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
    Expression Pos Expr_

union Expr_ =
    , LiteralText Text
    , LiteralNumber Text

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
        , attrs as Dict Name { pos as Pos, maybeType as Maybe Expression, maybeExpr as Maybe Expression }
        }

    , Variable { maybeType as Maybe Expression, word as Token.Word }

    , RecordShorthand [Name]

    , Fn [Expression] Expression

    , Call Expression [Expression]

    , Binop Op.Precedence (SepList Op.Binop Expression)

    , Unop Op.Unop Expression

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

