module Types.CanonicalAst exposing (..)

{-| Canonical AST is meant for

  - Type inference
  - Optimization
  - Emission or interpretation

-}


type Expression
    = NumberLiteral
        { start : Int
        , end : Int
        , number : String
        }
    | Variable
        { start : Int
        , end : Int
        , name : String

        --, moduleReference : ModuleReference
        }
    | Lambda
        { start : Int
        , parameter : String
        , body : Expression
        }
    | Record
        (List
            { name : String
            , value : Expression
            }
        )
    | Call
        { reference : Expression
        , argument : Expression
        , argumentIsMutable : Bool
        }
    | If
        { start : Int
        , condition : Expression
        , true : Expression
        , false : Expression
        }


type Statement
    = Evaluation Expression
    | Definition
        { name : String
        , body : Expression
        , maybeAnnotation : Maybe Type
        }
    | Assignment
        { lvalue : String
        , op : String
        , body : Expression
        }


type Type
    = TypeConstant
        { name : String

        --, moduleReference : ModuleReference
        --, args : List Type
        }
      {- TODO: support polymorphism
         | TypeVariable
             { name : String
             }
      -}
    | TypeFunction
        -- `from` can actually be mutable
        { from : Type
        , fromIsMutable : Bool
        , to : Type
        }
    | TypeRecord
        (List
            { name : String
            , type_ : Type
            }
        )
