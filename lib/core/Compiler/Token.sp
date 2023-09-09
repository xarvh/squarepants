LineNumber =
    Int


var Token =
    , 'token Int Int Kind


var OpenOrClosed =
    , 'open
    , 'closed


var SingleOrTriple =
    , 'singleQuote
    , 'tripleQuote


var Kind =
    , 'comment { indent as Int, isBlock as Bool, isFollowedByBlank as Bool }
    , # Structure
      'newSiblingLine
    , 'blockStart
    , 'blockEnd
    , 'badIndent
    , # Terms
      'textLiteral SingleOrTriple Text
    , 'numberLiteral Bool Text
    , # variable names, record attribute names
      # Record attributes will never have a module or an attrPath
      # However, the lexer can't distinguish between variable and attribute, we need to wait the parser
      'lowercase { attrPath as [ Name ], maybeModule as Maybe Name, name as Name }
    , 'constructor { maybeModule as Maybe Name, name as Name }
    , # Named Types
      'uppercase { maybeModule as Maybe Name, name as Name }
    , 'recordShorthand { attrPath as [ Name ], name as Name }
    , 'argumentPlaceholder
    , 'uniquenessPolymorphismBinop
    , # Keywords
      'fn
    , 'if LineNumber
    , 'then
    , 'else LineNumber
    , 'try
    , 'as
    , 'with
    , 'introspectValue
    , 'introspectType
    , # Separators
      'comma
    , 'colon
    , 'threeDots
    , # Ops
      'defop
    , 'unop Op.UnopId
    , 'binop LineNumber Op.Binop
    , # Parens
      'roundParen OpenOrClosed
    , 'squareBracket LineNumber OpenOrClosed
    , 'curlyBrace LineNumber OpenOrClosed
    , 'native
