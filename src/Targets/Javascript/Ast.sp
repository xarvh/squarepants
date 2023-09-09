#
# TODO move this in Targets/JavaScript
#
# TODO rename Expr to Expression
#
# TODO records use Lists of tuples rather than Dicts
#

Name =
    Text


var Statement =
    , 'eval Expr
    , 'return Expr
    , 'define Bool Name Expr
    , 'if Expr [ Statement ]


var Expr =
    , 'literal Text
    , #
      # `name`
      'var Name
    , #
      # -a
      # !a
      'unop Text Expr
    , #
      # (a + b)
      'binop Text Expr Expr
    , #
      # (a += b, null)
      'mutop Text Name Expr Expr
    , #
      # (someFunction)(a, b)
      'call Expr [ Expr ]
    , #
      # (a, b) => a + b + c
      'simpleLambda [ Name ] Expr
    , #
      # (a, b) => { blah; return meh; }
      'blockLambda [ Name ] [ Statement ]
    , #
      # (p ? a : b)
      'conditional Expr Expr Expr
    , #
      # { a: 1, b: 2 }
      'record (Dict Name Expr)
    , #
      # [a, b, c]
      'array [ Expr ]
    , #
      # (expr or lvalue).attrName
      'accessWithDot Text Expr
    , #
      # (expr)[2]
      'accessWithBrackets Expr Expr
    , #
      # (a, b, c)
      'comma [ Expr ]
