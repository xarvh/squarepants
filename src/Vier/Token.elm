module Vier.Token exposing (..)


type alias Token =
    { kind : TokenKind
    , start : Int
    , end : Int
    }


type
    TokenKind
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
    | If
    | Is
    | Then
    | Else
    | Return
      -- Ops
    | Unop String
    | Binop PrecedenceGroup String
      -- Parens
    | RoundParen OpenOrClosed
    | SquareBracket OpenOrClosed
    | CurlyBrace OpenOrClosed
    | Comma


type PrecedenceGroup
    = Exponential
    | Multiplicative
    | AddittiveSpaced
    | AddittiveUnspaced
    | Comparison
    | Logical
    | Pipe
    | Assignment


type OpenOrClosed
    = Open
    | Closed
