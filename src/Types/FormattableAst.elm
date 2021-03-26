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
import Types.Literal
import Types.Token


type alias Module =
    List Statement


type alias ValueDef =
    { pattern : Pattern
    , mutable : Bool
    , maybeAnnotation : Maybe Annotation
    , body : OneOrMore Statement
    }


type alias Annotation =
    { name : String
    , mutable : Bool
    , type_ : Type
    }


type Statement
    = Evaluation Expression
    | Definition ValueDef
    | TypeAlias
        { name : String
        , args : List String
        , type_ : Type
        }
    | UnionDef
        { name : String
        , args : List String

        -- constructors are parsed into a TypePolymorphic
        , constructors : List Type
        }


type Type
    = TypeName
        { name : String
        }
    | TypePolymorphic
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
    = Literal
        { start : Int
        , end : Int
        , value : Types.Literal.Value
        }
    | Variable
        { start : Int
        , end : Int
        , name : String
        , binop : Bool
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
    | FunctionCall Int Int
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
        , true : OneOrMore Statement
        , false : OneOrMore Statement
        }
    | Try
        { start : Int
        , isOneLine : Bool
        , value : Expression
        , patterns : List ( Pattern, OneOrMore Statement )
        , maybeElse : Maybe (OneOrMore Statement)
        }
    | Record
        { maybeUpdateTarget : Maybe Expression
        , attrs : List ( String, Maybe Expression )
        }
    | List (List Expression)


type Pattern
    = PatternAny String
    | PatternLiteral Types.Literal.Value
    | PatternApplication String (List Pattern)
    | PatternList (List Pattern)
    | PatternRecord (List ( String, Maybe Pattern ))
    | PatternCons Pattern Pattern
    | PatternTuple (List Pattern)


{-
exprStart : Expression -> Int
exprStart expr =
    case expr of
        Literal { start, end, value } ->
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

        Try { start, value, patterns, maybeElse } ->
            start

        Record args ->
            Debug.todo "exprStart"

        List _ ->
            Debug.todo "exprStart"
-}
