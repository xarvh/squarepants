alias LineNumber =
    Int


union Token =
    , Token Int Int Kind


union OpenOrClosed =
    , Open
    , Closed


union SingleOrTriple =
    , SingleQuote
    , TripleQuote


union Kind =
    , Comment { indent as Int, isBlock as Bool, isFollowedByBlank as Bool }
    , # Structure
      NewSiblingLine
    , BlockStart
    , BlockEnd
    , BadIndent
    , # Terms
      TextLiteral SingleOrTriple Text
    , NumberLiteral Bool Text
    , # variable names, record attribute names
      # Record attributes will never have a module or an attrPath
      # However, the lexer can't distinguish between variable and attribute, we need to wait the parser
      Lowercase { attrPath as [ Name ], maybeModule as Maybe Name, name as Name }
    , Constructor { maybeModule as Maybe Name, name as Name }
    , # Named Types
      Uppercase { maybeModule as Maybe Name, name as Name }
    , RecordShorthand { attrPath as [ Name ], name as Name }
    , ArgumentPlaceholder
    , UniquenessPolymorphismBinop
    , # Keywords
      Fn
    , If LineNumber
    , Then
    , Else LineNumber
    , Try
    , As
    , With
    , IntrospectValue
    , IntrospectType
    , # Separators
      Comma
    , Colon
    , ThreeDots
    , # Ops
      Defop
    , Unop Op.UnopId
    , Binop LineNumber Op.Binop
    , # Parens
      RoundParen OpenOrClosed
    , SquareBracket LineNumber OpenOrClosed
    , CurlyBrace LineNumber OpenOrClosed
