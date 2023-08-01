#
# TODO move this in Targets/JavaScript
#
# TODO rename Expr to Expression
#
# TODO records use Lists of tuples rather than Dicts
#


alias Name =
    Text


union Statement =
    , Eval Expr
    , Return Expr
    , Define Bool Name Expr
    , If Expr [Statement]


union Expr =
    , Literal Text
      #
      # `name`
    , Var Name
      #
      # -a
      # !a
    , Unop Text Expr
      #
      # (a + b)
    , Binop Text Expr Expr
      #
      # (a += b, null)
    , Mutop Text Name Expr Expr
      #
      # (someFunction)(a, b)
    , Call Expr [Expr]
      #
      # (a, b) => a + b + c
    , SimpleLambda [Name] Expr
      #
      # (a, b) => { blah; return meh; }
    , BlockLambda [Name] [Statement]
      #
      # (p ? a : b)
    , Conditional Expr Expr Expr
      #
      # { a: 1, b: 2 }
    , Record (Dict Name Expr)
      #
      # [a, b, c]
    , Array [Expr]
      #
      # (expr or lvalue).attrName
    , AccessWithDot Text Expr
      #
      # (expr)[2]
    , AccessWithBrackets Expr Expr
      #
      # (a, b, c)
    , Comma [Expr]

