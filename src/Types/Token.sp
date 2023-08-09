
union Comment =
    #
    # Let's start simple, and support only comments that do NOT share the line with actual code
    # If that works, then we can add "section" comments?
    #
    # No comment
    , N
    # Multiline comment with start row and end row
    # Precedes the token.
    , M Text


union Token =
    Token [Comment] Int Int Kind


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

