module Types.CanonicalAst exposing (..)


type Expression
    = NumberLiteral
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
        , parameter : String
        , body : Expression
        }
    | Call
        { reference : Expression
        , argument : Expression
        }
    | If
        { start : Int
        , condition : Expression
        , true : Expression
        , false : Expression
        }
