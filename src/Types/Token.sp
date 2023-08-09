
alias LineNumber =
    Int


union Token =
    Token Int Int Kind


union WordType =
    , Variable
    , Constructor
    , TypeOrModule
    , RecordShorthand


alias Word = {
    , type as WordType
    , maybeModule as Maybe Name
    , name as Name
    , attrPath as [Name]
    }


union OpenOrClosed =
    , Open
    , Closed


union SingleOrTriple =
    , SingleQuote
    , TripleQuote


union Kind =
    , Comment { isBlock as Bool, indent as Int, isFollowedByBlank as Bool }
    # Structure
    , NewSiblingLine
    , BlockStart
    , BlockEnd
    , BadIndent
    # Terms
    , TextLiteral SingleOrTriple Text
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
    , IntrospectValue
    , IntrospectType
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

