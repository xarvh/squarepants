
alias Token =
    { kind is Kind
    , start is Int
    , end is Int
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
    , Name { mutable is Bool } Text
    # Keywords
    , Fn
    , If
    , Try
    , As
    , Is
    , Colon
    , Else
    , With
    # Ops
    , Defop { mutable is Bool }
    , Unop Op.Unop
    , Binop Text Op.Binop
    , Arrow { mutable is Bool }
    # Parens
    , RoundParen OpenOrClosed
    , SquareBracket OpenOrClosed
    , CurlyBrace OpenOrClosed
    , Comma


union OpenOrClosed =
    , Open
    , Closed
