
union Token =
    Token Int Int Kind


union NameModifier =
    , NameNoModifier
    , NameMutable
    , NameStartsWithDot


union OpenOrClosed =
    , Open
    , Closed


union Kind =
    # Block
    , NewSiblingLine # TODO this divides statements within a block, find a better name for it
    , BlockStart
    , BlockEnd
    # Comment
    , Comment
    # Terms
    , TextLiteral Text
    , NumberLiteral Text
    , UpperName (Maybe Name) Name
    , LowerName NameModifier (Maybe Name) Name [Name]
    # Keywords
    , If
    , Then
    , Else
    , Try
    , As
    , Colon
    , MutableColon
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

