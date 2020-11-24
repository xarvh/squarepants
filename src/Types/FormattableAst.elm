module Types.FormattableAst exposing (..)

{-| This AST is meant to reflect more closely the cosmetic choices of the user.

It is meant for two purposes:

1.  Transform it back into human-readable, nicely formatted code
2.  Transform it into canonical AST for compiling

-}

import OneOrMore exposing (OneOrMore)


type Expression
    = StringLiteral
        { start : Int
        , end : Int
        , string : String
        }
    | NumberLiteral
        { start : Int
        , end : Int
        , number : String
        }
    | Variable
        { start : Int
        , end : Int
        , variable : String
        }
    | Lambda
        { start : Int
        , parameters : OneOrMore Pattern
        , body : OneOrMore Statement
        }
    | FunctionCall
        { reference : Expression
        , arguments : OneOrMore Expression
        }
    | Binop
        { left : Expression
        , op : String
        , right : Expression
        }
    | Unop
        { start : Int
        , op : String
        , right : Expression
        }
    | If_Functional
        { start : Int
        , condition : Expression
        , true : Expression
        , false : Expression
        }
    | Match_Functional
        { start : Int
        , value : Expression
        , patterns : List ( Pattern, Expression )
        , maybeElse : Maybe Expression
        }
    | Error


type Pattern
    = PatternAny String


type Statement
    = Pass
    | Evaluate Expression
    | Definition
        { name : Pattern
        , parameters : List Pattern
        , body : OneOrMore Statement
        }
    | Return Expression
    | If_Imperative
        { condition : Expression
        , true : List Statement
        , false : List Statement
        }
    | Match_Imperative
        { value : Expression
        , patterns : List ( Pattern, List Statement )
        , maybeElse : Maybe (List Statement)
        }


exprStart : Expression -> Int
exprStart expr =
    case expr of
        StringLiteral { start, end, string } ->
            start

        NumberLiteral { start, end, number } ->
            start

        Variable { start, end, variable } ->
            start

        Lambda { start, parameters, body } ->
            start

        FunctionCall { reference, arguments } ->
            exprStart reference

        Binop { left, op, right } ->
            exprStart left

        Unop { start, op, right } ->
            start

        If_Functional { start, condition, true, false } ->
            start

        Match_Functional { start, value, patterns, maybeElse } ->
            start

        Error ->
            Debug.todo ""

