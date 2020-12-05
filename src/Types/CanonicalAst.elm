module Types.CanonicalAst exposing (..)

{-| Canonical AST is meant for

  - Type inference
  - Optimization
  - Emission or interpretation

-}


type alias ModuleReference =
    String


type TypeAnnotationUnion
    = TypeDefined
        { moduleReference : ModuleReference
        , name : String
        , args : List TypeAnnotation
        }
    | TypeVariable
        { name : String
        }
    | TypeFunction
        { from : TypeAnnotation
        , to : TypeAnnotation
        }
    | TypeRecord (List ( String, TypeAnnotation ))


type alias TypeAnnotation =
    { isMutableReference : Bool
    , union : TypeAnnotationUnion
    }


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
        , moduleReference : ModuleReference
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
