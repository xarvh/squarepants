
union Token =
    Token Int Int Kind


union NameModifier =
    , NameNoModifier
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
    , Name
        {
        , modifier as NameModifier
        , isUpper as Bool
        , maybeModule as Maybe Name
        , name as Name
        , attrPath as [Name]
        }
    # Keywords
    , If
    , Then
    , Else
    , Try
    , As
    , With
    , Colon
    , ConsumingColon
    , ThreeDots
    # Ops
    , Defop
    , Mutop
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

