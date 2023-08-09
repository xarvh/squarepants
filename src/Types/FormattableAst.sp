[# This AST is meant to reflect more closely the cosmetic choices of the user.

It is meant for two purposes:

1.  Transform it into canonical AST for compiling
2.  Transform it back into human-readable, nicely formatted code

It is a lot more permissive than what the actual language syntax allows, because in this way we can give more helpful error messages to the user.
Instead than errors at parse time, we can produce more meaningful errors when translating into canonical.

#]


union Word =
    , Word Pos Token.Word


alias Binop = {
    , comments as [Comment]
    , usr as USR
    , symbol as Text
    , precedence as Int
    , pos as Pos
    , line as Int
    }


alias Module =
    [Statement]


alias ValueDef = {
    , pattern as Expression
    , nonFn as [Word]
    , body as Expression
    }


alias AliasDef =
    {
    , name as Word
    # TODO rename to pars
    , args as [Word]
    , type as Expression
    }


alias UnionDef =
    {
    , name as Word
    , args as [Word]
    , constructors as [Expression]
    }


alias Comment = {
    , start as Int
    , end as Int
    , indent as Int
    , isBlock as Bool
    , isFollowedByBlank as Bool
    }


union Statement =
    , CommentStatement Comment
    , Evaluation Expression
    , ValueDef ValueDef
    , AliasDef AliasDef
    , UnionDef UnionDef


union Expression =
    Expression [Comment] Pos Expr_


union Expr_ =
    , LiteralText Text
    , LiteralNumber Bool Text
    , ArgumentPlaceholder

    , Statements [Statement]

    # List isMultiline [isUnpacked & item]
    , List Bool [Bool & Expression]

    # NOTE: `{ name as Type = value }` is valid
    , Record {
        # no extension: { attr1, ... }
        # pattern extension: { with attr1, ... }
        # expression extension: { expt with attr1, ... }
        , maybeExtension as Maybe (Maybe Expression)
        , attrs as [RecordAttribute]
        , isMultiline as Bool
        }

    , Variable {
        , maybeType as Maybe Expression
        , tokenWord as Token.Word
        }

    , Fn Bool [Expression] Expression

    , UnopCall Op.UnopId Expression

    , BinopChain Int BinopChain

    , Call Expression [Expression]

    , Poly Text Expression

    , If
        {
        , isMultiline as Bool
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
    # `name` can also contain the type as an annotation
    , name as Expression
    , maybeExpr as Maybe Expression
    }


# expr op expr op expr op...
alias BinopChain =
    Expression & [ Binop & Expression ]


binopChainExpressions as fn BinopChain: [Expression] =
    fn (head & tuples):
    head :: List.map Tuple.second tuples


binopChainReverse as fn BinopChain: BinopChain =

    rec as fn [Binop & Expression], BinopChain: BinopChain =
        fn acc, oddItem & remainder:
        try remainder as
            , []: oddItem & acc
            , [sep & item, ...tail]: rec [sep & oddItem, ...acc] (item & tail)

    rec [] __


binopChainAllBinops as fn (fn Binop: Bool), BinopChain: Bool =
    fn f, ls:
    try ls.second as
        , []:
            True

        , [sep & item, ...tail]:
            if f sep then
                binopChainAllBinops f (item & tail)

            else
                False


statementPos as fn Statement: Pos =
    fn statement:
    try statement as
        , CommentStatement { with start, end }: Pos.P start end
        , Evaluation (Expression _ pos expr_): pos
        # TODO: the position should encompass the WHOLE definition, not just its start
        , ValueDef { pattern = Expression _ pos expr_, nonFn, body }: pos
        , AliasDef { name = Word pos _, args, type }: pos
        , UnionDef { name = Word pos _, args, constructors }: pos

