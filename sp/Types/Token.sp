
alias Token =
    { kind as Kind
    , start as Int
    , end as Int
    }


union Kind =
    # Structure
    , NewSiblingLine
    , BlockStart
    , BlockEnd
    # Comment
    , Comment
    # Terms
    , TextLiteral Text
    , NumberLiteral Text
    , Name { mutable as Bool } Text
    # Keywords
    , Fn
    , If
    , Try
    , As
    , Colon
    , Then
    , Else
    , With
    # Ops
    , Defop { mutable as Bool }
    , Unop Types/Unop.Unop
    , Binop Text Types/Binop.Binop
    , Arrow { mutable as Bool }
    # Parens
    , RoundParen OpenOrClosed
    , SquareBracket OpenOrClosed
    , CurlyBrace OpenOrClosed
    , Comma


union OpenOrClosed =
    , Open
    , Closed
