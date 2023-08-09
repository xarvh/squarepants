

union Token =
    Token [Text] Int Int Kind


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
    , NumberLiteral Bool Text
    , Word Word
    , ArgumentPlaceholder
    , UniquenessPolymorphismBinop
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

