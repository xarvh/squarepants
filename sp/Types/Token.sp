
# TODO what if this was `Token Int Int Kind`?
alias Token =
    { kind is Kind
    , start is Int
    , end is Int
    }


union NameModifier =
    , NameNoModifier
    , NameMutable
    , NameStartsWithDot


union OpenOrClosed =
    , Open
    , Closed


union Kind =
    # Structure
    , NewSiblingLine
    , BlockStart
    , BlockEnd
    , BadIndent
    # Comment
    , Comment
    # Terms
    , TextLiteral Text # TODO add a flag to remember whether it was multiline or not
    , NumberLiteral Text
    , Name NameModifier Text
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
    , Binop Op.Binop
    , Arrow { mutable is Bool }
    # Parens
    , RoundParen OpenOrClosed
    , SquareBracket OpenOrClosed
    , CurlyBrace OpenOrClosed
    , Comma
    #
    , ErrorUnknownOp Text
    , ErrorBlock Text
    , ErrorUnterminated Text # TODO check that everywhere it is used start position is set

