module Types.FormattableAst exposing (..)

{-| This AST is meant to reflect more closely the cosmetic choices of the user.

It is meant for two purposes:

1.  Transform it into canonical AST for compiling
2.  Transform it back into human-readable, nicely formatted code

It is a lot more permissive than what the actual language syntax allows, because in this way we can give more helpful error messages to the user.
Instead than errors at parse time, we can produce more meaningful errors when translating into canonical.

-}

import OneOrMore exposing (OneOrMore)
import SepList exposing (SepList)
import Types.Token


type alias Module =
    List Statement


type alias ValueDefinition =
    { name : Pattern
    , mutable : Bool
    , maybeAnnotation : Maybe Annotation
    , parameters : List Pattern
    , body : OneOrMore Statement
    }


type alias Annotation =
    { name : String
    , mutable : Bool
    , type_ : Type
    }


type Statement
    = Evaluation Expression
    | Definition ValueDefinition
    | TypeAlias
        { name : String
        , args : List String
        , type_ : Type
        }
    | TypeDefinition
        { name : String
        , args : List String
        , constructors : List TypeConstructor
        }


type alias TypeConstructor =
    { name : String
    , args : List Type
    }


type Type
    = TypeConstantOrVariable
        { name : String
        , args : List Type
        }
    | TypeFunction
        { from : Type
        , fromIsMutable : Bool
        , to : Type
        }
    | TypeTuple (List Type)
    | TypeRecord (List ( String, Type ))


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
        , name : String
        }
    | Lvalue
        -- TODO rename to `Mutable`?
        { start : Int
        , end : Int
        , name : String
        }
    | Lambda
        { start : Int

        -- TODO this should be a list
        , parameters : OneOrMore Pattern
        , body : OneOrMore Statement
        }
    | FunctionCall
        { reference : Expression
        , arguments : OneOrMore Expression
        }
    | Binop
        { group : Types.Token.PrecedenceGroup
        , sepList : SepList String Expression
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
    | Record
        { maybeUpdateTarget : Maybe Expression
        , attrs : List ( String, Maybe Expression )
        }
    | List (List Expression)


type Pattern
    = PatternAny String


exprStart : Expression -> Int
exprStart expr =
    case expr of
        StringLiteral { start, end, string } ->
            start

        NumberLiteral { start, end, number } ->
            start

        Variable { start, end, name } ->
            start

        Lvalue { start, end, name } ->
            start

        Lambda { start, parameters, body } ->
            start

        FunctionCall { reference, arguments } ->
            exprStart reference

        Binop { group, sepList } ->
            exprStart (Tuple.first sepList)

        Unop { start, op, right } ->
            start

        If { start, condition, true, false } ->
            start

        Match { start, value, patterns, maybeElse } ->
            start

        Record args ->
            Debug.todo ""

        List _ ->
            Debug.todo ""
