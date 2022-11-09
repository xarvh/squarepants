
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
    , Unop Op.Unop
    , Binop Op.Binop
    # Parens
    , RoundParen OpenOrClosed
    , SquareBracket OpenOrClosed
    , CurlyBrace OpenOrClosed
