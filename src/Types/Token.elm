module Types.Token exposing (..)


type alias Token =
    { kind : Kind
    , start : Int
    , end : Int
    }


type
    Kind
    -- Structure
    = NewSiblingLine
    | BlockStart
    | BlockEnd
      -- Comment
    | Comment
      -- Terms
    | StringLiteral String
    | NumberLiteral String
    | Name { mutable : Bool } String
      -- Types
    | HasType { mutable : Bool }
      -- Keywords
    | Fn
    | If
    | Try
    | As
    | Then
    | Else
    | With
      -- Ops
    | Defop { mutable : Bool }
    | Unop String
    | Binop PrecedenceGroup String
    | Arrow { mutable : Bool }
      -- Parens
    | RoundParen OpenOrClosed
    | SquareBracket OpenOrClosed
    | CurlyBrace OpenOrClosed
    | Comma


type PrecedenceGroup
    = Exponential
    | Multiplicative
    | Addittive
    | Comparison
    | Logical
    | Tuple
    | Pipe
    | Mutop


type OpenOrClosed
    = Open
    | Closed
