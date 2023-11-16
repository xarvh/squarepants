[# This AST is meant to reflect more closely the cosmetic choices of the user.

It is meant for two purposes:

1.  Transform it into canonical AST for compiling
2.  Transform it back into human-readable, nicely formatted code

It is a lot more permissive than what the actual language syntax allows, because in this way we can give more helpful error messages to the user.
Instead than errors at parse time, we can produce more meaningful errors when translating into canonical.

#]

Binop =
    {
    , comments as [ Comment ]
    , line as Int
    , pos as Pos
    , precedence as Int
    , symbol as Text
    , usr as USR
    }


Module =
    [ Statement ]


ValueDef =
    {
    , body as Expression
    , nonFn as [ Pos & Name ]
    , pattern as Expression
    }


AliasDef =
    {
    # TODO rename to pars
    , args as [ Pos & Name ]
    , name as Pos & Name
    , type as Expression
    }


VariantTypeDef =
    {
    , args as [ Pos & Name ]
    , constructors as [ Expression ]
    , name as Pos & Name
    }


Comment =
    {
    , end as Int
    , indent as Int
    , isBlock as Bool
    , isFollowedByBlank as Bool
    , start as Int
    }


var Layout =
    , 'inline
    , 'aligned
    , 'indented


var Statement =
    , 'commentStatement Comment
    , 'evaluation Expression
    , 'valueDef ValueDef
    , 'aliasDef AliasDef
    , 'unionDef VariantTypeDef


var Expression =
    , 'expression [ Comment ] Pos Expr_


var Expr_ =
    , 'literalText Token.SingleOrTriple Text
    , 'literalNumber Bool Text
    , 'argumentPlaceholder
    , # This is used only when translating from FA to CA
      'resolvedArgumentPlaceholder Int
    , 'statements [ Statement ]
    , # List isMultiline [isUnpacked & item]
      'list Bool [ Bool & Expression ]
    , # NOTE: `{ name as Type = value }` is valid
      'record
          {
          , attrs as [ RecordAttribute ]
          , isMultiline as Bool
          # no extension: { attr1, ... }
          # pattern extension: { with attr1, ... }
          # expression extension: { expt with attr1, ... }
          , maybeExtension as Maybe (Maybe Expression)
          }
    , 'lowercase
          {
          , attrPath as [ Name ]
          , maybeModule as Maybe Name
          , maybeType as Maybe Expression
          , name as Name
          }
    , 'uppercase
          {
          , maybeModule as Maybe Name
          , name as Name
          }
    , 'constructor
          {
          , maybeModule as Maybe Name
          , name as Name
          }
    , 'recordShorthand
          {
          , attrPath as [ Name ]
          , name as Name
          }
    , 'fn Layout [ Expression ] Expression
    , 'unopCall Op.UnopId Expression
    , 'binopChain Int BinopChain
    , 'call Expression [ Expression ]
    , 'poly Text Expression
    , 'if
          {
          , condition as Expression
          , false as Expression
          , isMultiline as Bool
          , true as Expression
          }
    , 'try
          {
          , patterns as [ Expression & Expression ]
          , value as Expression
          }
    , 'native


RecordAttribute =
    {
    , maybeExpr as Maybe Expression
    # `name` can also contain the type as an annotation
    , name as Expression
    }


# expr op expr op expr op...
BinopChain =
    Expression & [ Binop & Expression ]


binopChainExpressions as fn BinopChain: [ Expression ] =
    fn head & tuples:
    head :: List.map Tuple.second tuples


binopChainReverse as fn BinopChain: BinopChain =
    rec as fn [ Binop & Expression ], BinopChain: BinopChain =
        fn acc, oddItem & remainder:
        try remainder as
            []: oddItem & acc
            [ sep & item, tail... ]: rec [ sep & oddItem, acc... ] (item & tail)

    rec [] __


binopChainAllBinops as fn fn Binop: Bool, BinopChain: Bool =
    fn f, ls:
    try ls.second as

        []:
            'true

        [ sep & item, tail... ]:
            if f sep then
                binopChainAllBinops f (item & tail)
            else
                'false


statementPos as fn Statement: Pos =
    fn statement:
    try statement as

        'commentStatement { with  end, start }:
            Pos.'p start end

        'evaluation ('expression _ pos expr_):
            pos

        # TODO: the position should encompass the WHOLE definition, not just its start
        'valueDef { body, nonFn, pattern = 'expression _ pos expr_ }:
            pos

        'aliasDef { args, name, type }:
            name.first

        'unionDef { args, constructors, name }:
            name.first
