module Types.Binop exposing (..)

import Types.CanonicalAst as CA


type alias Binop =
    { symbol : String
    , precedence : Precedence

    -- TODO Do I really need this?
    -- It is actually used only for pipes, everything else is hardcoded or derived from the precedence group
    , associativity : LeftOrRight
    , ty : CA.Type
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
