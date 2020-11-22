module Types.FormattableAst exposing (..)

{-| This AST is meant to reflect more closely the cosmetic choices of the user.

It is meant for two purposes:

1.  Transform it back into human-readable, nicely formatted code
2.  Transform it into canonical AST for compiling

-}

import OneOrMore exposing (OneOrMore)


type alias Pattern =
    -- TODO
    String


type Expression
    = StringLiteral String
    | NumberLiteral String
    | Variable String
    | Lambda { parameters : OneOrMore Pattern, body : OneOrMore Statement }
    | FunctionCall { reference : Expression, arguments : OneOrMore Expression }
    | Binop Expression String Expression
    | Unop String Expression
    | If_Functional { condition : Expression, true : Expression, false : Expression }
    | Match_Functional { value : Expression, patterns : List ( Pattern, Expression ), maybeElse : Maybe Expression }
    | Error


type Statement
    = Pass
    | Evaluate Expression
    | Definition { name : Pattern, parameters : List Pattern, body : OneOrMore Statement }
    | Return Expression
    | If_Imperative { condition : Expression, true : List Statement, false : List Statement }
    | Match_Imperative { value : Expression, patterns : List ( Pattern, List Statement ), maybeElse : Maybe (List Statement) }
