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
    | Symbol String
      -- Keywords
    | Fn
    | If
    | Try
    | As
    | Then
    | Else
      -- Ops
    | Unop String
    | Defop
    | Binop PrecedenceGroup String
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
    | Pipe
    | Assignment


type OpenOrClosed
    = Open
    | Closed
