module Types.CanonicalAst exposing (..)

{- Canonical AST is meant for
   * type inference
   * optimization
   * emission or interpretation
-}


type Expression
    = NumberLiteral
        { start : Int
        , end : Int

        -- TODO this should be Int
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
    -- TODO use a record instead?
    | Tuple2
        { first : Expression
        , second : Expression
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


type Statement
    = Definition { name : String, body : Expression }
