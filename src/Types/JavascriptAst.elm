module Types.JavascriptAst exposing (..)

import Dict exposing (Dict)


type alias Name =
    String


type Statement
    = Eval Expr
    | Return Expr
    | Define Name Expr
    | If Expr (List Statement)


type Expr
    = Literal String
      --
      -- `name`
    | Var Name
      --
      -- (a + b)
    | Binop String Expr Expr
      --
      -- (a += b, null)
    | Mutop String Name Expr Expr
      --
      -- (someFunction)(a, b)
    | Call Expr (List Expr)
      --
      -- (a, b) => a + b + c
    | SimpleLambda (List Name) Expr
      --
      -- (a, b) => { blah; return meh; }
    | BlockLambda (List Name) (List Statement)
      --
      -- (p ? a : b)
    | Conditional Expr Expr Expr
      --
      -- { a: 1, b: 2 }
    | Record (Dict Name Expr)
      --
      -- [a, b, c]
    | Array (List Expr)
      --
      -- (expr or lvalue).attrName
    | AccessWithDot String Expr
      --
      -- (expr)[2]
    | AccessWithBrackets Expr Expr
