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
    | Tuple (List Expression)
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


{-| Unlike the canonical annotation, the formattable annotation allows the mutability flag even where it's invalid.
This way we can tell the user why they can't flag those as mutable rather than just producing a syntax error.
-}
type TypeAnnotationUnion
    = TypeConstant
        { name : String
        }
    | TypeVariable
        { name : String
        }
      -- TODO TypeFunction's List is guaranteed to have at least TWO items, but I'm not yet sure what's the best format for them
    | TypeFunction (List TypeAnnotation)
    | TypePolymorphic
        -- TODO name should be a String
        { name : TypeAnnotation
        , args : OneOrMore TypeAnnotation
        }
    | TypeTuple (List TypeAnnotation)
    | TypeRecord (List ( String, TypeAnnotationUnion ))


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

        If { start, condition, true, false } ->
            start

        Match { start, value, patterns, maybeElse } ->
            start

        Tuple _ ->
            Debug.todo ""

        Record attrs ->
            Debug.todo ""
