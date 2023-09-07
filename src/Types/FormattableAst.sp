[# This AST is meant to reflect more closely the cosmetic choices of the user.

It is meant for two purposes:

1.  Transform it into canonical AST for compiling
2.  Transform it back into human-readable, nicely formatted code

It is a lot more permissive than what the actual language syntax allows, because in this way we can give more helpful error messages to the user.
Instead than errors at parse time, we can produce more meaningful errors when translating into canonical.

#]


alias Binop =
    {
    , comments as [ Comment ]
    , line as Int
    , pos as Pos
    , precedence as Int
    , symbol as Text
    , usr as USR
    }


alias Module =
    [ Statement ]


alias ValueDef =
    {
    , body as Expression
    , nonFn as [ Pos & Name ]
    , pattern as Expression
    }


alias AliasDef =
    {
    # TODO rename to pars
    , args as [ Pos & Name ]
    , name as Pos & Name
    , type as Expression
    }


alias UnionDef =
    {
    , args as [ Pos & Name ]
    , constructors as [ Constructor ]
    , name as Pos & Name
    }

alias Constructor =
    {
    , comments as [Comment]
    , name as Pos & Name
    , pars as [Expression]
    }


alias Comment =
    {
    , end as Int
    , indent as Int
    , isBlock as Bool
    , isFollowedByBlank as Bool
    , start as Int
    }


union Layout =
    , Inline
    , Aligned
    , Indented


union Statement =
    , CommentStatement Comment
    , Evaluation Expression
    , ValueDef ValueDef
    , AliasDef AliasDef
    , UnionDef UnionDef


union Expression =
    , Expression [ Comment ] Pos Expr_


union Expr_ =
    , LiteralText Token.SingleOrTriple Text
    , LiteralNumber Bool Text
    , ArgumentPlaceholder
    , # This is used only when translating from FA to CA
      ResolvedArgumentPlaceholder Int
    , Statements [ Statement ]
    , # List isMultiline [isUnpacked & item]
      List Bool [ Bool & Expression ]
    , # NOTE: `{ name as Type = value }` is valid
      Record
          {
          , attrs as [ RecordAttribute ]
          , isMultiline as Bool
          # no extension: { attr1, ... }
          # pattern extension: { with attr1, ... }
          # expression extension: { expt with attr1, ... }
          , maybeExtension as Maybe (Maybe Expression)
          }
    , Lowercase
          {
          , attrPath as [ Name ]
          , maybeModule as Maybe Name
          , maybeType as Maybe Expression
          , name as Name
          }
    , Uppercase
          {
          , maybeModule as Maybe Name
          , name as Name
          }
    , Constructor
          {
          , maybeModule as Maybe Name
          , name as Name
          }
    , RecordShorthand
          {
          , attrPath as [ Name ]
          , name as Name
          }
    , Fn Layout [ Expression ] Expression
    , UnopCall Op.UnopId Expression
    , BinopChain Int BinopChain
    , Call Expression [ Expression ]
    , Poly Text Expression
    , If
          {
          , condition as Expression
          , false as Expression
          , isMultiline as Bool
          , true as Expression
          }
    , Try
          {
          , patterns as [ Expression & Expression ]
          , value as Expression
          }


alias RecordAttribute =
    {
    , maybeExpr as Maybe Expression
    # `name` can also contain the type as an annotation
    , name as Expression
    }


# expr op expr op expr op...
alias BinopChain =
    Expression & [ Binop & Expression ]


binopChainExpressions as fn BinopChain: [ Expression ] =
    fn head & tuples:
    head :: List.map Tuple.second tuples


binopChainReverse as fn BinopChain: BinopChain =
    rec as fn [ Binop & Expression ], BinopChain: BinopChain =
        fn acc, oddItem & remainder:
        try remainder as
            , []: oddItem & acc
            , [ sep & item, ...tail ]: rec [ sep & oddItem, ...acc ] (item & tail)

    rec [] __


binopChainAllBinops as fn fn Binop: Bool, BinopChain: Bool =
    fn f, ls:
    try ls.second as

        , []:
            True

        , [ sep & item, ...tail ]:
            if f sep then
                binopChainAllBinops f (item & tail)
            else
                False


statementPos as fn Statement: Pos =
    fn statement:
    try statement as

        , CommentStatement { with  end, start }:
            Pos.P start end

        , Evaluation (Expression _ pos expr_):
            pos

        # TODO: the position should encompass the WHOLE definition, not just its start
        , ValueDef { body, nonFn, pattern = Expression _ pos expr_ }:
            pos

        , AliasDef { args, name, type }:
            name.first

        , UnionDef { args, constructors, name }:
            name.first

