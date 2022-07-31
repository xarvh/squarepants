
union Token =
    Token Int Int Kind


union NameModifier =
    , NameNoModifier
    , NameStartsWithDot


union OpenOrClosed =
    , Open
    , Closed


union Kind =
    , Indent Int
    , Comment
    # Terms
    , TextLiteral Text
    , NumberLiteral Text
    , UpperName (Maybe Name) Name
    , LowerName NameModifier (Maybe Name) Name [Name]
    # Keywords
    , If
    , Then
    , Else
    , Try
    , As
    , Colon LambdaModifier
    , With
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

