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
    | Defop
    | Mutop String
    | Unop String
    | Binop PrecedenceGroup String
    | Arrow
    | HasType
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


type OpenOrClosed
    = Open
    | Closed
