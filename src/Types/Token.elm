module Types.Token exposing (..)

import Types.Op


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
    | TextLiteral String
    | NumberLiteral String
    | Name { mutable : Bool } String
      -- Keywords
    | Fn
    | If
    | Try
    | As
    | Is
    | Colon
    | MutableColon
    | Then
    | Else
    | With
      -- Ops
    | Defop { mutable : Bool }
    | Unop Types.Op.Unop
    | Binop String Types.Op.Binop
      -- Parens
    | RoundParen OpenOrClosed
    | SquareBracket OpenOrClosed
    | CurlyBrace OpenOrClosed
    | Comma


type OpenOrClosed
    = Open
    | Closed
