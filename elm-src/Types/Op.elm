module Types.Op exposing (..)

import Types.CanonicalAst as CA


type alias Unop =
    { symbol : String
    , ty : CA.Type
    }


type alias Binop =
    { symbol : String
    , precedence : Precedence

    -- TODO Do I really need this?
    -- It is actually used only for pipes, everything else is hardcoded or derived from the precedence group
    , associativity : LeftOrRight
    , ty : CA.Type
    , nonFn : List String
    }


type LeftOrRight
    = NonAssociative
    | Left
    | Right


type Precedence
    = Exponential
    | Multiplicative
    | Addittive
    | Cons
    | Comparison
    | Logical
    | Tuple
    | Pipe
    | Mutop
