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
        , variable : String

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
        , maybeAnnotation : Maybe TypeAnnotation
        }
    | Assignment
        { lvalue : String
        , op : String
        , body : Expression
        }


type alias TypeAnnotation =
    { isMutable : Bool
    , union : TypeAnnotationUnion
    }


type TypeAnnotationUnion
    = TypeConstant
        { name : String

        --, moduleReference : ModuleReference
        , args : List TypeAnnotationUnion
        }
    | TypeVariable
        { name : String
        }
    | TypeFunction
        -- `from` can actually be mutable
        { from : TypeAnnotation
        , to : TypeAnnotationUnion
        }
    | TypeRecord (List ( String, TypeAnnotationUnion ))
