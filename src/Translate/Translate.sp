
union Ctx =
    , Value
    , Type
    , Pattern



translateStatement as fn FA.Statement: FN.Statement =
    fn statement:

    try statement as
      , FA.CommentStatement comment: FN.CommentStatement comment

      , FA.Evaluation expression: FN.Evaluation (translateExpression Value expression)

      , FA.ValueDef { body, nonFn, pattern }:
            FN.ValueDef
              {
              , body = translateExpression Value body
              , nonFn = List.map translateWord nonFn
              , pattern = translateExpression Pattern pattern
              }

      , FA.AliasDef { args, name, type }:
            FN.AliasDef
                  {
                  , args = List.map translateWord args
                  , name = translateWord name
                  , type = translateExpression Type type
                  }

      , FA.UnionDef { args, constructors, name }:
            FN.UnionDef
                {
                , args = List.map translateWord args
                , constructors = List.map translateConstructorDef constructors
                , name = translateWord name
                }


translateWord as fn FA.Word: Pos & Name =
    fn FA.Word pos word: pos & word.name


translateConstructorDef =
    fn FA.Expression comms pos x:

    try x as
      , FA.Variable { maybeType, tokenWord }:
          {
          , name = updateConstructorName tokenWord.name
          , maybeModule = Nothing
          }
          >> FN.Constructor
          >> FN.Expression comms pos __

      , FA.Call (FA.Expression c p (FA.Variable { maybeType, tokenWord })) pars:
          {
          , name = updateConstructorName tokenWord.name
          , maybeModule = Nothing
          }
          >> FN.Constructor
          >> FN.Expression c p __
          >> FN.Call __ (List.map (translateExpression Type __) pars)
          >> FN.Expression comms pos __

      , _:
          todo "WUT"


updateConstructorName as fn Name: Name =
    fn name:

    re = Text.startsWithRegex "[A-Z][A-Z]"

    if re name /= "" then
      "'" .. name
    else

      first = Text.slice 0 1 name
      rest = Text.slice 1 999999 name

      "'" .. Text.toLowercase first .. rest




translateExpression as fn Ctx, FA.Expression: FN.Expression =
    fn ctx, FA.Expression comms pos ee:

    rec = translateExpression ctx __

    try ee as
        , FA.LiteralText s text: FN.LiteralText s text
        , FA.LiteralNumber bool text: FN.LiteralNumber bool text
        , FA.ArgumentPlaceholder: FN.ArgumentPlaceholder
        , FA.ResolvedArgumentPlaceholder int: FN.ResolvedArgumentPlaceholder int
        , FA.Statements s: FN.Statements << List.map translateStatement s
        , FA.List bool xs: FN.List bool (List.map (fn b & x: b & rec x) xs)
        , FA.Record { attrs  , isMultiline , maybeExtension }:
            FN.Record {
                , attrs = List.map (translateAttribute ctx __) attrs
                , isMultiline
                , maybeExtension = Maybe.map (Maybe.map rec __) maybeExtension
                }

        , FA.Fn layout pars body:
            FN.Fn layout (List.map rec pars) (rec body)

        , FA.Variable { maybeType, tokenWord }:
            translateVariable ctx maybeType tokenWord


        , FA.UnopCall unopId expression:
            FN.UnopCall unopId (rec expression)


        , FA.BinopChain int (head & tail):
            FN.BinopChain int (rec head & List.map (fn op & e: op & rec e) tail)

        , FA.Call ref args:
            FN.Call (rec ref) (List.map rec args)

        , FA.Poly text expression:
            FN.Poly text (rec expression)

        , FA.If { condition  , false  , isMultiline , true }:
            FN.If { condition = rec condition, false = rec false, isMultiline, true = rec true }

        , FA.Try { patterns , value }:
            FN.Try {
                , patterns = List.map (fn p & e: translateExpression Pattern p & translateExpression Value e) patterns
                , value = translateExpression Value value
                }
    >> FN.Expression comms pos __


translateAttribute as fn Ctx, FA.RecordAttribute: FN.RecordAttribute =
      fn ctx, { maybeExpr, name }:

      {
      , maybeExpr = Maybe.map (translateExpression ctx __) maybeExpr
      , name = translateExpression ctx name
      }



translateVariable as fn Ctx, Maybe FA.Expression, Token.Word: FN.Expr_ =
    fn ctx, maybeType_, { modifier, isUpper, maybeModule, name, attrPath }:

    maybeType = Maybe.map (translateExpression Type __) maybeType_

    try ctx as

        , Type:
            # Type
            # variable
            # ---> can have module
            if isUpper then
                FN.Uppercase { name, maybeModule }
            else
                FN.Lowercase { maybeModule, name, attrPath, maybeType }

        , _:
            # .shorthand
            # Constructor
            # variable
            # ---> can have module and attrPath
            if modifier == Token.NameStartsWithDot then
                FN.RecordShorthand { name = "." .. name, attrPath }
            else if isUpper then
                FN.Constructor { name = updateConstructorName name, maybeModule }
            else
                FN.Lowercase { maybeModule, name, attrPath, maybeType }



