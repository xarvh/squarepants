
union Token = Token Int Int Kind


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
    , MutableColon
    , Else
    , With
    # Ops
    , Defop { mutable as Bool }
    , Unop Op.Unop
    , Binop Op.Binop
    # Parens
    , RoundParen OpenOrClosed
    , SquareBracket OpenOrClosed
    , CurlyBrace OpenOrClosed
    , Comma
    #
    , ErrorUnknownOp Text
    , ErrorBlock Text
    , ErrorUnterminated Text # TODO check that everywhere it as used start position as set

