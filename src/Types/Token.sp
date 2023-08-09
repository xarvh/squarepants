
alias LineNumber =
    Int


union Token =
    Token Int Int Kind


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
    , Comment { isBlock as Bool, indent as Int, isFollowedByBlank as Bool }
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
    , If LineNumber
    , Then
    , Else LineNumber
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
    , Binop LineNumber Op.Binop
    # Parens
    , RoundParen OpenOrClosed
    , SquareBracket LineNumber OpenOrClosed
    , CurlyBrace LineNumber OpenOrClosed

