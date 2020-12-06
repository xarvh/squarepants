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
    | If
        { start : Int
        , isOneLine : Bool
        , condition : Expression
        , true : Expression
        , false : Expression
        }
    | Match
        { start : Int
        , isOneLine : Bool
        , value : Expression
        , patterns : List ( Pattern, Expression )
        , maybeElse : Maybe Expression
        }
    | Tuple2
        { first : Expression
        , second : Expression
        }
    | Record
        (List
            { name : String
            , value : Expression
            }
        )


type Pattern
    = PatternAny String


type Statement
    = Evaluate Expression
    | Definition
        { name : Pattern
        , maybeAnnotation : Maybe TypeAnnotation
        , parameters : List Pattern
        , body : OneOrMore Statement
        }
    | Mutation
        { left : String
        , mutop : String
        , right : Expression
        }


type TypeAnnotationUnion
    = TypeDefined
        { name : String
        , args : List TypeAnnotation
        }
    | TypeVariable
        { name : String
        }
    | TypeFunction
        { from : TypeAnnotation
        , to : TypeAnnotation
        }
    | TypeTuple2 ( TypeAnnotation, TypeAnnotation )
    | TypeRecord (List ( String, TypeAnnotation ))


type alias TypeAnnotation =
    { isMutableReference : Bool
    , union : TypeAnnotationUnion
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

        Tuple2 { first, second } ->
            exprStart first

        Error ->
            Debug.todo ""
