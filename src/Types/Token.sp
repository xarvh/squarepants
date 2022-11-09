
union Comment =
    # No comment
    , N
    # start, end
    , C Int Int

union Token =
    Token Comment Int Int Kind


union NameModifier =
    , NameNoModifier
    , NameStartsWithDot


alias Name =
        {
        , modifier as NameModifier
        , isUpper as Bool
        , maybeModule as Maybe Name
        , name as Name
        , attrPath as [Name]
        }



union OpenOrClosed =
    , Open
    , Closed


union Kind =
    # Structure
    , NewSiblingLine
    , BlockStart
    , BlockEnd
    , BadIndent
    # Terms
    , TextLiteral Text
    , NumberLiteral Text
    , Name Namr
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

