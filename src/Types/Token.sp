
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
    # Structure
    , NewSiblingLine
    , BlockStart
    , BlockEnd
    , BadIndent
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
    , ConsumingColon
    , With
    # Ops
    , Defop
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

