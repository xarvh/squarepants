
union Comment =
    # No comment
    , N
    # Before, start, end
    , Be Int Int
    # After, start, end
    , Af Int Int
    # The idea is to determine whether to attach a comment to the previous token or to the next by deciding which one is closer
    # in terms of col or row distance


union Token =
    Token Comment Int Int Kind


union NameModifier =
    , NameNoModifier
    , NameStartsWithDot


alias Word =
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
    , Word Word
    # Keywords
    , Fn
    , If
    , Then
    , Else
    , Try
    , As
    , With
    # Separators
    , Comma
    , Colon
    , ThreeDots
    # Ops
    , Defop
    , Unop Op.UnopId
    , Binop Op.Binop
    # Parens
    , RoundParen OpenOrClosed
    , SquareBracket OpenOrClosed
    , CurlyBrace OpenOrClosed

